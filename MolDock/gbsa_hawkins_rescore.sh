#!/bin/bash
#
#   rescore docked poses using GB/SA

export PATH=/u/jr/contrib/dock6/bin:$PATH

# prepare needed files
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

if [ ! -d hawkins ] ; then mkdir hawkins ; fi
cd hawkins

cp ../../../sh/gbsa_hawkins_1ary.in gbsa_hawkins.in

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
        if [ ! -e ${nam}_gbsa_hawkins_scored.mol2 ] ; then
            dock6 -i gbsa_hawkins.in -o gbsa_hawkins.out -v
            mv gbsa_hawkins_scored.mol2 ${nam}_gbsa_hawkins_scored.mol2
            mv gbsa_hawkins.out ${nam}_gbsa_hawkins.out
        fi
    fi
done

exit
### THIS IS IGNORED ###

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

if [ ! -e gbsa_hawkins_scored.mol2 ] ; then
    dock6 -i gbsa_hawkins.in -o gbsa_hawkins.out -v
fi

cd ..

exit

#### this is ignored ###
#### this is gbsa.in ###
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
gbsa_hawkins_score_primary                                   yes
gbsa_hawkins_score_secondary                                 no
gbsa_hawkins_score_rec_filename                              receptor.mol2
gbsa_hawkins_score_solvent_dielectric                        78.5
gbsa_hawkins_use_salt_screen                                 no
gbsa_hawkins_score_gb_offset                                 0.09
gbsa_hawkins_score_cont_vdw_and_es                           no
gbsa_hawkins_score_grid_prefix                               ../solvent_grid
amber_score_secondary                                        no
minimize_ligand                                              yes
simplex_max_iterations                                       500
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
vdw_defn_file                                                params/vdw_AMBER_parm99.defn
flex_defn_file                                               params/flex.defn
flex_drive_file                                              params/flex_drive.tbl
ligand_outfile_prefix                                        gbsa_hawkins
write_orientations                                           no
num_scored_conformers                                        500
write_conformations                                          yes
cluster_conformations                                        no
rank_ligands                                                 no
