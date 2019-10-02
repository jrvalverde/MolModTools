#!/bin/bash

master=00_orig/receptor.pdb
ligands="00_orig/ligands/ATP.mol2 00_orig/ligands/E89C4m34.mol2 00_orig/ligands/E89C4m4.mol2"
#   this is the leeway we allow around the reference ligand for docking
#   other molecules
radius=5.0

if [ ! -e $master ] ; then exit ; fi

chimerabin=~/contrib/chimera/bin
dock6dir=~/contrib/dock6
dock6bin=$dock6dir/bin
pdbtools=~/contrib/pdb-tools
jrbin=~/work/gneshich/bin

export PATH=$jrbin:$dock6bin:$chimerabin:$pdbtools:$PATH

# RECEPTOR PREPARATION
# --------------------
# 
#    DONE BY HAND
# 
# 2GSZ structure was processed with Chimera using DockPrep, removing ADP,
# non-complexed ions, assigning Gasteiger charges and saved in 
# 01_dockprep/rec.* both as mol2 and PDB
# 
# The known ligand (ADP) was saved with unmodified coordinates to 
# 01_dockprep/knl.* both as mol2 and PDB

if [ ! -d 01_dockprep ] ; then mkdir 01_dockprep ; fi
cp $master 01_dockprep
cd 01_dockprep

# UNUSED AT THIS POINT
if [ -e AUTO ] ; then
	cat > dockprep.py <<END
import chimera
from DockPrep import prep
models = chimera.openModels.list(modelTypes=[chimera.Molecule])
#prep(models,    addHFunc=AddH.hbondAddHydrogens, hisScheme=None, 
#                mutateMSE=True, mutate5BU=True, mutateUMS=True, mutateCSL=True,
#                delSolvent=True, delIons=False, delLigands=False, 
#                delAltLocs=True, incompleteSideChains="rotamers", nogui=False, 
#                rotamerLib=defaults[INCOMPLETE_SC], rotamerPreserve=True)
prep(models, delIons=True, delLigands=True)
from WriteMol2 import writeMol2
writeMol2(models, "rec.mol2")
import Midas
Midas.write(models, None, "rec.pdb")
END
	chimera --nogui `basename $master` dockprep.py &> dockprep.log
fi

if [ ! -s rec.pdb ] ; then
    cat <<EOF

Please, process the file using DockPrep, removing the known ligand (if any),
non-complexed ions, ssigning Gasteiger charges and save both as PDB and mol2
using the names
    rec.mol2	    -- the receptor in mol2 format
    rec.pdb 	    -- the receptor in PDB format
    knl.mol2	    -- the known ligand (if any) in mol2 format
    knl.pdb 	    -- the known ligand (if any) in PDB format

EOF
    chimera `basename $master`
fi
echo "receptor molecule saved in 01_dockprep/rec.pdb"
echo "charged receptor molecule saved in 01_dockprep/rec.mol2"
cd ..


# GENERATE RECEPTOR WITHOUT H
# ---------------------------
# 
# PDB versions of the structures without the H atoms are generated 
# using grep and pdb-tools (to renumber atoms) using:

if [ ! -d 02_noH ] ; then mkdir 02_noH ; fi
cp 01_dockprep/*.pdb 02_noH
cd 02_noH

for i in rec.pdb ; do 
    if [ ! -e `basename $i .pdb`_noH.pdb ] ; then
        grep -v '^ATOM *[0-9]* *H' $i > zzz.pdb
        $pdbtools/pdb_atom_renumber.py zzz > \
    	    `basename $i .pdb`_noH.pdb
    fi
    echo $i H stripped into 02_noH/`basename $i .pdb`_noH.pdb
done
rm -f zzz zzz.pdb
cd ..


# GENERATE RECEPTOR MOLECULAR SURFACES
# ------------------------------------
# 
# Once  the rec_noH.pdb file has been generated, compute its molecular surface


if [ ! -d 03_molsurf ] ; then mkdir 03_molsurf ; fi
if [ ! -s 02_noH/rec_noH.pdb ] ; then
	echo "ERROR: 02_noH/rec_noH.pdb does not exist!"
	if [ ! -e 01_dockprep/rec.pdb ] ; then
		echo "       01_dockprep/rec.pdb does not exist either!"
		echo "       Please, ensure rec.pdb containing the prepared receptor exists!"
	else
		echo "       Please, revise the rec.pdb file for correctness!"
	fi
	exit
fi
cp 02_noH/rec_noH.pdb 03_molsurf
cd 03_molsurf

for i in rec_noH.pdb ; do
    if [ ! -s `basename $i _noH.pdb`.dms ] ; then
        $chimerabin/dms $i -n -w 1.4 -v -o \
            `basename $i _noH.pdb`.dms
    fi
    echo $i surface saved as 03_molsurf/`basename $i _noH.pdb`.dms
done
cd ..


# GENERATE RECEPTOR SURROUNDING SPHERES
# -------------------------------------
# 
# Spheres surrounding the receptor were generated and named 'rec.sph' using
# a modified version of SPHGEN (270 format statement modified to allow bigger
# numbers of spheres in temp2.sph as default 2i5 was not enough on some
# molecules, and 15 and 355 statements for similar reasons in temp3.atc).

if [ ! -d 04_sphgen ] ; then mkdir 04_sphgen ; fi
if [ ! -s 03_molsurf/rec.dms ] ; then
	echo "ERROR: 03_molsurf/rec.dms does not exist!"
	echo "       Please check that the molecular surface was computed properly"
	exit
fi
cp 03_molsurf/rec.dms 04_sphgen
cd 04_sphgen

if [ ! -s rec.sph ] ; then
	echo -n "$j .. "

    rm -f rec.sph temp* OUTSPH
    for i in *.dms ; do
        cat > INSPH<<END
$i
R
X
0.0
4.0
1.4
rec.sph
`basename $i .dms`.sph
END
        $jrbin/jrsphgen 
#         mv OUTSPH `basename $i .dms`.OUTSPH
    done
fi
    echo "rec.dms sphere calculated into 04_sphgen/rec.sph"
cd ..


# NOTE: for some structures the contributed programs sphgen_cpp and
# sphgen_cpp_threads had to be used instead to overcome limitations 
# in the original sphgen.


# SELECT BINDING SITE SPHERES
# ---------------------------
# 
# For many of the proteins, the binding site(s) is/are known and
# available on the original PDB file. To select the putative binding
# site we must therefore:
# 
#     - open original PDB file
#     - select ligand(s)
#     - invert selection
#     - delete everything else
#     - save as knownligand.mol2
#     - select all spheres within $radius Angstrom of the known ligand(s):
#     sphere_selector sphgen_sphere_cluster_file.sph set_of_atoms_file.mol2 radius
# 
# This can be automated too using chimera --nogui:

if [ ! -d 05_sitsph ] ; then mkdir 05_sitsph ; fi
if [ ! -s 04_sphgen/rec.sph ] ; then
    echo "ERROR: no rec.sph spheres generated."
    echo "       cannot pick site spheres!"
    exit
fi
cp 04_sphgen/rec.sph 05_sitsph
cp $master 05_sitsph
# just in case it was hand done for us
if [ -s 01_dockprep/knl.mol2 ] ; then
    echo "01_dockprep/knl.mol2 found. Using it as the reference ligand"
    cp 01_dockprep/knl.mol2 05_sitsph
fi

cd 05_sitsph

# if we have no known ligand, look for one
if [ ! -s knl.mol2 ] ; then
# extract ligands
    base=`basename $master .pdb`
    chimera --nogui <<END
open ${base}.pdb
select ligand
write selected format mol2 0 ${base}-L.mol2
select nucleic acid
write selected format mol2 0 ${base}-N.mol2
END
    echo "$master ligands saved in 05_sitsph/${base}-L.mol2"
    echo "$master nucleic acids saved in 05_sitsph/${base}-L.mol2"

    # Hand reviewing is needed next to remove invalid ligands (e.g. ethylene glycol).
    echo "Please, review in a separate terminal the ligand candidates"
    echo "    05_sitsph/${base}-L.mol2 contains molecules identified as ligands"
    echo "    05_sitsph/${base}-N.mol2 contains nucleic acids"
    echo ""
    echo "Review these files and remove all but the valid ligand(s) to be"
    echo "used to identify the active site"
    echo ""
    echo "Once done, press [ENTER] to continue"
    read line

    mkdir empty
    # empty mol2 files have a size of 109 bytes
    find . -size 109c -exec mv {} empty \; -print
    
    if [ -s ${base}-L.mol2 ] ; then
        ln -s ${base}-L.mol2 knl.mol2
    elif [ -s ${base}-N.mol2 ] ; then
        ln -s ${base}-N.mol2 knl.mol2
    else
        echo "WARNING: no reference ligands found in $master"
        echo "         the biggest cavity will be used"
    fi
fi
# When there is no ligand in the original file we will default to using
# the whole surface (cluster 0).
if [ ! -s rec_site.sph ] ; then
    if [ -e knl.mol2 ] ; then
    	# select spheres in a radius of RADIUS angstrom
    	sphere_selector rec.sph knl.mol2 $radius
    	showsphere <<END
selected_spheres.sph
1
N
selected_spheres.pdb
END
    	ln -s selected_spheres.sph rec_site.sph
    	ln -s selected_spheres.pdb rec_site.pdb

    else
    	# no known ligand available, select the whole protein surface
    	showsphere <<END
rec.sph
0
N
selected_spheres.pdb
END
	ln -s rec.sph selected_spheres.sph
	ln -s rec.sph rec_site.sph
    	ln -s selected_spheres.pdb rec_site.pdb
    fi
fi
echo "generated site spheres in 05_sitsph/rec_site.sph"
cd ..


# GENERATE GRID BOX
# -----------------
#
#Generate grid box using showbox

if [ ! -d 06_gridbox ] ; then mkdir 06_gridbox ; fi
if [ ! -s 05_sitsph/rec_site.sph ] ; then
    echo "ERROR: receptor site generation into 05_sitsph/rec_site.sph failed"
    exit
fi
cp 05_sitsph/rec_site.sph 06_gridbox
cd 06_gridbox

if [ ! -e receptor_box.pdb ] ; then
    showbox <<END
Y
5
rec_site.sph
1
receptor_box.pdb
END
fi
echo "computed receptor box into 06_gridbox/receptor_box.pdb"

cd ..

# GENERATE GRID
# -------------
# 
# Generate grid using program 'grid'

if [ ! -d 07_grid ] ; then mkdir 07_grid ; fi
if [ ! -s 06_gridbox/receptor_box.pdb ] ; then
    echo "ERROR: 06_gridbox/receptor_box.pdb does not exist"
    exit
fi
# copy required files:
cp 01_dockprep/rec.mol2 07_grid
cp 05_sitsph/selected_spheres.sph 07_grid
cp 06_gridbox/receptor_box.pdb 07_grid

cd 07_grid

if [ ! -e grid.nrg ] ; then
    cat > grid.in <<END
compute_grids                  yes
grid_spacing                   0.3
output_molecule                yes
receptor_out_file              rec_clean.pdb
contact_score                  yes
contact_cutoff_distance        4.5
bump_filter                    yes
bump_overlap                   0.75
energy_score                   yes
energy_cutoff_distance         10
atom_model                     a
attractive_exponent            6
repulsive_exponent             9
distance_dielectric            yes
dielectric_factor              4.0
receptor_file                  rec.mol2
box_file                       receptor_box.pdb
vdw_definition_file            $dock6dir/parameters/vdw_AMBER_parm99.defn
score_grid_prefix              grid
END
    echo -n "computing grids.. "
    grid -i grid.in > grid.stdout 
fi

#NOTE: on the Finis-Terrae supercomputer the command should end as
#
#    qsub -l num_proc=1,s_rt=24:00:00,s_vmem=1G,h_fsize=1G <<ENDJOB
#        cd $HOME/work/arrojas/dock6/receptor-basic/$i
#	$PATH=$HOME/dock6/bin:$PATH
#        grid -i grid.in > grid.stdout&
#ENDJOB
#    cd ..
#done
    echo "computation of 07_grid/grid.* done"
cd ..


# PREPARE ACTUAL DOCKING DATASETS
# -------------------------------
# 
# Make a directory for each ligand
# Copy required receptor data to corresponding directories in the
# ligand-docking directory.

if [ ! -d 09_dock ] ; then mkdir 09_dock ; fi

for j in $ligands ; do
    echo "Processing $j"
    lig=`basename $j .mol2`
    if [ -d 09_dock/$lig ] ; then  continue ; fi
    mkdir -p 09_dock/$lig
    cd 09_dock/$lig
    ln -s ../../01_dockprep/rec.mol2 receptor.mol2
    ln -s ../../01_dockprep/rec.pdb receptor.pdb
    ln -s ../../05_sitsph/rec_site.sph .
    ln -s ../../07_grid/grid.nrg .
    ln -s ../../07_grid/grid.bmp .
    ln -s ../../07_grid/grid.cnt .
    if [ -e ../../05_sitsph/knl.mol2 ] ; then
	ln -s ../../05_sitsph/knl.mol2 .
    fi
    cd ../..
    cp $j 09_dock/$lig/ligand.mol2
# following is unneeded but handy for later evaluation
    cp $master 09_dock/$lig/orig.pdb
done

echo "Prepared directories for docking $master to $ligands"


# DO RIGID LIGAND DOCKING
# -----------------------
# 
# For each receptor run a rigid docking experiment in its own directory
# generating the configurarion file and running dock.
# 
# NOTES:	consider secondary scoring with gbsa_hawkings or Dock3.5 (requires DelPhi)
# 	consider writing orientations and scored conformers

cd 09_dock
for i in * ; do
    # Do not repeat an existing calculation
    if [ -s $i/rigid_scored.mol2 ] ; then continue ; fi
    cd $i
    cat > rigid.in <<END
ligand_atom_file                                             ./ligand.mol2
limit_max_ligands                                            no
skip_molecule                                                no
read_mol_solvation                                           no
calculate_rmsd                                               no
use_database_filter                                          no
orient_ligand                                                yes
automated_matching                                           yes
receptor_site_file                                           ./rec_site.sph
max_orientations                                             500
critical_points                                              no
chemical_matching                                            no
use_ligand_spheres                                           no
use_internal_energy                                          yes
internal_energy_rep_exp                                      12
flexible_ligand                                              no
bump_filter                                                  no
score_molecules                                              yes
contact_score_primary                                        no
contact_score_secondary                                      no
grid_score_primary                                           yes
grid_score_secondary                                         no
grid_score_rep_rad_scale                                     1
grid_score_vdw_scale                                         1
grid_score_es_scale                                          1
grid_score_grid_prefix                                       ./grid
dock3.5_score_secondary                                      no
continuous_score_secondary                                   no
gbsa_zou_score_secondary                                     no
gbsa_hawkins_score_secondary                                 no
amber_score_secondary                                        no
minimize_ligand                                              yes
simplex_max_iterations                                       1000
simplex_tors_premin_iterations                               0
simplex_max_cycles                                           1
simplex_score_converge                                       0.1
simplex_cycle_converge                                       1.0
simplex_trans_step                                           1.0
simplex_rot_step                                             0.1
simplex_tors_step                                            10.0
simplex_random_seed                                          0
simplex_restraint_min                                        no
atom_model                                                   all
vdw_defn_file                                                $dock6dir/parameters/vdw_AMBER_parm99.defn
flex_defn_file                                               $dock6dir/parameters/flex.defn
flex_drive_file                                              $dock6dir/parameters/flex_drive.tbl
ligand_outfile_prefix                                        rigid
write_orientations                                           yes
num_scored_conformers                                        100
write_conformations					     no
cluster_conformations                                        yes
cluster_rmsd_threshold                                       2.0
rank_ligands                                                 no

END
    echo -n "Rigid docking $i .. "
    dock6 -i rigid.in -o rigid.out
    echo "$i docked"
    cd ..
done
echo "Rigid docking done"
cd ..


# DO FLEXIBLE LIGAND DOCKING
# --------------------------
# 
# For each receptor run a rigid docking experiment in its own directory
# generating the configurarion file and running dock.
# 
# NOTES:	consider secondary scoring with gbsa_hawkings or Dock3.5 (requires DelPhi
# 	and AMSOL) consider writing orientations and scored conformers

cd 09_dock

for i in * ; do
    if [ -s $i/flexible_scored.mol2 ] ; then continue ; fi
    cd $i
    cat > flexible.in <<END
ligand_atom_file                                             ./ligand.mol2
limit_max_ligands                                            no
skip_molecule                                                no
read_mol_solvation                                           no
calculate_rmsd                                               no
use_database_filter                                          no
orient_ligand                                                yes
automated_matching                                           yes
receptor_site_file                                           ./rec_site.sph
max_orientations                                             500
critical_points                                              no
chemical_matching                                            no
use_ligand_spheres                                           no
use_internal_energy                                          yes
internal_energy_rep_exp                                      12
flexible_ligand                                              yes
min_anchor_size                                              40
pruning_use_clustering                                       yes
pruning_max_orients                                          100
pruning_clustering_cutoff                                    100
pruning_conformer_score_cutoff                               25.0
use_clash_overlap                                            no
write_growth_tree                                            no
bump_filter                                                  no
score_molecules                                              yes
contact_score_primary                                        no
contact_score_secondary                                      no
grid_score_primary                                           yes
grid_score_secondary                                         no
grid_score_rep_rad_scale                                     1
grid_score_vdw_scale                                         1
grid_score_es_scale                                          1
grid_score_grid_prefix                                       ./grid
dock3.5_score_secondary                                      no
continuous_score_secondary                                   no
gbsa_zou_score_secondary                                     no
gbsa_hawkins_score_secondary                                 no
amber_score_secondary                                        no
minimize_ligand                                              yes
minimize_anchor                                              yes
minimize_flexible_growth                                     yes
use_advanced_simplex_parameters                              no
simplex_max_cycles                                           1
simplex_score_converge                                       0.1
simplex_cycle_converge                                       1.0
simplex_trans_step                                           1.0
simplex_rot_step                                             0.1
simplex_tors_step                                            10.0
simplex_anchor_max_iterations                                500
simplex_grow_max_iterations                                  500
simplex_grow_tors_premin_iterations                          0
simplex_random_seed                                          0
simplex_restraint_min                                        no
atom_model                                                   all
vdw_defn_file                                                $dock6dir/parameters/vdw_AMBER_parm99.defn
flex_defn_file                                               $dock6dir/parameters/flex.defn
flex_drive_file                                              $dock6dir/parameters/flex_drive.tbl
ligand_outfile_prefix                                        flexible
write_orientations                                           yes
num_scored_conformers                                        100
write_conformations					     no
cluster_conformations                                        yes
cluster_rmsd_threshold                                       2.0
rank_ligands                                                 no

END
    echo -n "Flexible docking $i .. "
    dock6 -i flexible.in -o flexible.out
    echo "$i docked"
    cd ..
done
echo "Flexible docking done"
cd ..


# ZOU GB/SA DOCK
# --------------
# 
# Repeat flexible docking with other scores. Primary score used Grid-Based
# score(non-bonded terms of molecular mechanics force field).
# 
# Next step should be to use Dock3.5 as primary score (a variant of Grid score 
# using desolvation in addition to steric and electrostatic interactions), and 
# gbsa_hawkins (MM-GBSA Molecular Mechanichs Generalized Born Surface Area) as 
# secondary score. However, Dock3.5 rquires AMSOL and Hawkins requires other
# programs (DELPHI)
# 
# Instead we will be using Zou GB/SA as the primary score, as according to the
# manual gives similar results to the grid-based free energy model with less
# computation efforts. But this one requires previous generation of GB and SA 
# grids, which we must do:

cd 09_dock

for i in * ; do
    if [ -s gbsa_zou_scored.mol2 ] ; then continue ; fi
    cd $i
    if [ ! -h receptor_box.pdb ] ; then
    	ln -s ../../06_gridbox/receptor_box.pdb .
    fi
    cat > solvent_grid.in <<END
compute_grids                  yes
grid_spacing                   0.3
output_molecule                no
contact_score                  no
energy_score                   yes
energy_cutoff_distance         9999
atom_model                     a
attractive_exponent            6
repulsive_exponent             12
distance_dielectric            no 
dielectric_factor              1 
bump_filter                    yes
bump_overlap                   0.75
receptor_file                  ligand.mol2
box_file                       receptor_box.pdb
vdw_definition_file            $dock6dir/parameters/vdw_AMBER_parm99.defn
score_grid_prefix              solvent_grid
END
    if [ ! -s solvent_grid.bmp ] ; then grid -i solvent_grid.in ; fi
    if [ ! -h params ] ; then ln -s $dock6dir/parameters params ; fi
    if [ ! -d nchemgrid_GB ] ; then mkdir nchemgrid_GB ; fi
    cd nchemgrid_GB
    touch cavity.pdb
    cat > INCHEM<<END
../receptor.pdb
cavity.pdb
../params/prot.table.ambcrg.ambH
../params/vdw.parms.amb
../receptor_box.pdb
0.3
2
1
8.0 8.0
78.3 78.3
2.3 2.8
gbsa_zou_grid
1
END
    if [ ! -s gbsa_zou_grid.bmp ] ; then nchemgrid_GB ; fi
    if [ ! -d ../nchemgrid_SA ] ; then mkdir ../nchemgrid_SA ; fi
    cd ../nchemgrid_SA
    if [ ! -h params ] ; then ln -s $dock6dir/parameters params ; fi
    cat > INCHEM<<END
../receptor.pdb
../params/prot.table.ambcrg.ambH
../params/vdw.parms.amb
../receptor_box.pdb
0.3
1.4
2
8.0
gbsa_zou_grid
END
    if [ ! -s gbsa_zou_grid.bmp ] ; then nchemgrid_SA ; fi
    cd ..

    # Then, we can perform a GB/SA calculation using something like the following gbsa_zou.in
    cat > gbsa_zou.in <<END
ligand_atom_file                                             ./ligand.mol2
automated_matching                                           yes
receptor_site_file                                           ./rec_site.sph
max_orientations                                             500
critical_points                                              no
chemical_matching                                            no
use_ligand_spheres                                           no
limit_max_ligands                                            no
skip_molecule                                                no
read_mol_solvation                                           no
calculate_rmsd                                               no
use_database_filter                                          no
orient_ligand                                                yes
use_internal_energy                                          no
flexible_ligand                                              no
bump_filter                                                  no
score_molecules                                              yes
contact_score_primary                                        no
contact_score_secondary                                      no
grid_score_primary                                           no
grid_score_secondary                                         no
dock3.5_score_primary                                        no
dock3.5_score_secondary                                      no
continuous_score_primary                                     no
continuous_score_secondary                                   no
gbsa_zou_score_primary                                       yes
gbsa_zou_score_secondary                                     no
gbsa_zou_gb_grid_prefix                                      ./nchemgrid_GB/gbsa_zou_grid
gbsa_zou_sa_grid_prefix                                      ./nchemgrid_SA/gbsa_zou_grid
gbsa_zou_vdw_grid_prefix                                     ./solvent_grid
gbsa_zou_screen_file                                         ./params/screen.in
gbsa_zou_solvent_dielectric                                  78.300003
gbsa_hawkins_score_secondary                                 no
amber_score_secondary                                        no
minimize_ligand                                              no
atom_model                                                   all
vdw_defn_file                                                ./params/vdw_AMBER_parm99.defn
flex_defn_file                                               ./params/flex.defn
flex_drive_file                                              ./params/flex_drive.tbl
ligand_outfile_prefix                                        gbsa_zou
write_orientations                                           yes
write_conformations					     no
cluster_conformations                                        yes
cluster_rmsd_threshold                                       2.0
num_scored_conformers                                        100
rank_ligands                                                 no
END
    echo -n "Zou GB/SA docking $i .. "
    dock6 -i gbsa_zou.in -o gbsa_zou.out
    echo "$i docked"

    cd ..
done
echo "Zou GB/SA docking done"
cd ..


# USE HAWKINS MM-GB/SA DOCKING
# ----------------------------

cd 09_dock
for i in * ; do
    if [ -e mmgbsa_hawkins_scored.mol2 ] ; then continue ; fi
    cd $i
    cat > mmgbsa_hawkins.in <<END
ligand_atom_file                                             ./ligand.mol2
limit_max_ligands                                            no
skip_molecule                                                no
read_mol_solvation                                           no
calculate_rmsd                                               no
use_database_filter                                          no
orient_ligand                                                yes
automated_matching                                           yes
receptor_site_file                                           ./rec_site.sph
max_orientations                                             500
critical_points                                              no
chemical_matching                                            no
use_ligand_spheres                                           no
use_internal_energy                                          yes
internal_energy_rep_exp                                      12
flexible_ligand                                              yes
min_anchor_size                                              40
pruning_use_clustering                                       yes
pruning_max_orients                                          100
pruning_clustering_cutoff                                    100
pruning_conformer_score_cutoff                               25.0
use_clash_overlap                                            no
write_growth_tree                                            no
bump_filter                                                  no
score_molecules                                              yes
contact_score_primary                                        no
contact_score_secondary                                      no
grid_score_primary                                           yes
grid_score_secondary                                         no
grid_score_rep_rad_scale				     1.0
grid_score_vdw_scale                                         1
grid_score_es_scale                                          1
grid_score_grid_prefix                                       ./grid
dock3.5_score_primary                                        no
dock3.5_score_secondary                                      no
dock3.5_vdw_score					     yes
dock3.5_grd_prefix					     chem52
dock3.5_electrostatic_score				     yes
dock3.5_ligand_internal_energy				     yes
dock3.5_ligand_desolvation_score			     volume
dock3.5_write_atomic_energy_contrib			     yes
dock3.5_score_vdw_scale					     1.0
dock3.5_score_es_scale					     1.0
continuous_score_secondary                                   no
gbsa_zou_score_secondary                                     no
gbsa_hawkins_score_primary				     no
gbsa_hawkins_score_secondary                                 yes
gbsa_hawkins_score_rec_filename				     receptor.mol2
gbsa_hawkins_score_solvent_dielectric			     78.5
gbsa_hawkins_use_salt_screen       			     no
gbsa_hawkins_score_gb_offset				     0.09
gbsa_hawkins_score_cont_vdw_and_es                           yes
gbsa_hawkins_score_vdw_att_exp				     6
gbsa_hawkins_score_vdw_rep_exp				     12
grid_score_rep_rad_scale				     1.0
amber_score_secondary                                        no
minimize_ligand                                              yes
minimize_anchor                                              yes
minimize_flexible_growth                                     yes
use_advanced_simplex_parameters                              no
simplex_max_cycles                                           1
simplex_score_converge                                       0.1
simplex_cycle_converge                                       1.0
simplex_trans_step                                           1.0
simplex_rot_step                                             0.1
simplex_tors_step                                            10.0
simplex_anchor_max_iterations                                500
simplex_grow_max_iterations                                  500
simplex_grow_tors_premin_iterations                          0
simplex_random_seed                                          0
simplex_restraint_min                                        no
atom_model                                                   all
vdw_defn_file                                                $dock6dir/parameters/vdw_AMBER_parm99.defn
flex_defn_file                                               $dock6dir/parameters/flex.defn
flex_drive_file                                              $dock6dir/parameters/flex_drive.tbl
ligand_outfile_prefix                                        mmgbsa_hawkins
write_orientations                                           yes
num_scored_conformers                                        100
write_conformations					     no
cluster_conformations                                        yes
cluster_rmsd_threshold                                       2.0
rank_ligands                                                 no

END
#   this works for most cases in FINISTERRAE
#	qsub -N FDOCK6-$j-$i -l num_proc=1,s_rt=20:00:00,s_vmem=2G,h_fsize=1G <<ENDSUB
#   try with more memory for big grid files
#	qsub -N FOCK6-$j-$i -l num_proc=1,s_rt=20:00:00,s_vmem=4G,h_fsize=1G <<ENDSUB
#   1dg3 requires 4GB memory (from NGS, an x86_64 machine)
#	qsub -l num_proc=1,s_rt=20:00:00,s_vmem=4G,h_fsize=1G <<ENDSUB
#    this should work on NGS
#	qsub -q slow -N DOCK6+$j-$i \
#	     -e $HOME/work/arrojas/dock6/$j/$i/DOCK6+.err \
#	     -o $HOME/work/arrojas/dock6/$j/$i/DOCK6+.out <<ENDSUB
#    	    cd $HOME/work/arrojas/dock6/$j/$i
#	    export PATH=$HOME/dock6/bin:$PATH
    export DELPHI_HOME=$HOME/bin/delphi
    echo -n "Hawkins MM-GB/SA docking $i .. "
    dock6 -i mmgbsa_hawkins.in -o mmgbsa_hawkins.out -v
    echo "$i docked"
#ENDSUB
    cd ..
done
cd ..
