#!/bin/bash
#
#   rescore docked poses using PB/SA

export PATH=/u/jr/contrib/dock6/bin:/u/jr/contrib/openeye/bin:$PATH
export OE_LICENSE=/home/jr/contrib/openeye/lic/oe_license

# Prepare required files
if [ ! -e solvent_grid.nrg ] ; then
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
vdw_definition_file            ./params/vdw_AMBER_parm99.defn
score_grid_prefix              solvent_grid
END
    grid -i solvent_grid.in
fi

#	Perform computations in a subdirectory

if [ ! -d pbsa ] ; then mkdir pbsa ; fi
cd pbsa

cp ../../../sh/pbsa.in .

if [ ! -e params ] ; then ln -s ../params . ; fi
if [ ! -e ligand.mol2 ] ; then ln -s ../ligand.mol2 . ; fi
if [ ! -e receptor.mol2 ] ; then ln -s ../receptor.mol2 . ; fi
if [ ! -e receptor.pdb ] ; then ln -s ../receptor.pdb . ; fi
if [ ! -e receptor_box.pdb ] ; then ln -s ../receptor_box.pdb . ; fi
if [ ! -e solvent_grid.in ] ; then
    ln -s ../solvent_grid.in .
    ln -s ../solvent_grid.bmp .
    ln -s ../solvent_grid.nrg .
fi

for i in \
	../best*.mol2 \
	../flex-*_secondary_conformers.mol2 \
        ../flex-*_primary_conformers.mol2 \
        ../flexible_scored.mol2 \
        ../rigid_scored.mol2 \
        ../rec+lig_*.mol2 ; do
    # if nothing matches a wildcard, the wilcard is passwd as argument
    if [ ! -e "$i" ] ; then continue ; fi
    if [ -s "$i" ] ; then
    	rm -f docked.mol2
    	ln -s $i docked.mol2
	nam=`basename $i .mol2`
        if [ ! -e ${nam}_pbsa_scored.mol2 ] ; then
            dock6.pbsa -i pbsa.in -o pbsa.out 
            mv pbsa_scored.mol2 ${nam}_pbsa_scored.mol2
            mv pbsa.out ${nam}_pbsa.out
        fi
    fi
done


exit

#### THIS IS IGNORED ###

#### OLD SELECTIVE LOGIC: USE ONLY LAST RESULTS ###


if [ ! -e docked.mol2 ] ; then 
    # look for a successful docking run to rescore
    #	start looking at secondary results if available
    if [ -s ../flex-grid-gbsa_secondary_conformers.mol2 ] ; then
            ln -s ../flex-grid-gbsa_secondary_conformers.mol2 docked.mol2	    
    elif [ -s ../flex-grid-gbsa-0_secondary_conformers.mol2 ] ; then
	ln -s ../flex-grid-gbsa-0_secondary_conformers.mol2 docked.mol2
    elif [ -s ../flex-grid-gbsa-1_secondary_conformers.mol2 ] ; then
            ln -s ../flex-grid-gbsa-1_secondary_conformers.mol2 docked.mol2
    #	otherwise look for primary results
    elif [ -s ../flex-grid-gbsa_primary_conformers.mol2 ] ; then
            ln -s ../flex-grid-gbsa_primary_conformers.mol2 docked.mol2
    elif [ -s ../flex-grid-gbsa-0_primary_conformers.mol2 ] ; then
            ln -s ../flex-grid-gbsa-0_primary_conformers.mol2 docked.mol2
    elif [ -s ../flex-grid-gbsa-1_primary_conformers.mol2 ] ; then
            ln -s ../flex-grid-gbsa-1_primary_conformers.mol2 docked.mol2
    elif [ -s ../flexible_scored.mol2 ] ; then
    	    ln -s ../flexible_scored.mol2 docked.mol2
    else
            echo "ERROR: NOTHING TO SCORE"
    fi
fi

if [ ! -e pbsa_scored.mol2 ] ; then
    dock6.pbsa -i pbsa.in -o pbsa.out -v
fi
cd ..

exit

#### this is ignored ###
#### this is pbsa.in ###
ligand_atom_file                                             docked.mol2
limit_max_ligands                                            no
skip_molecule                                                no
read_mol_solvation                                           no
calculate_rmsd                                               no
use_database_filter                                          no
orient_ligand                                                no
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
gbsa_zou_score_primary                                       no
gbsa_zou_score_secondary                                     no
gbsa_hawkins_score_primary                                   no
gbsa_hawkins_score_secondary                                 no
pbsa_score_primary                                           yes
pbsa_score_secondary                                         no
pbsa_receptor_filename                                       receptor.mol2
pbsa_interior_dielectric                                     2.0
pbsa_exterior_dielectric                                     78.5
pbsa_vdw_grid_prefix                                         solvent_grid
amber_score_secondary                                        no
minimize_ligand                                              yes
atom_model                                                   all
vdw_defn_file                                                ./params/vdw_AMBER_parm99.defn
flex_defn_file                                               ./params/flex.defn
flex_drive_file                                              ./params/flex_drive.tbl
ligand_outfile_prefix                                        pbsa
write_orientations                                           no
num_scored_conformers                                        1
rank_ligands                                                 no
