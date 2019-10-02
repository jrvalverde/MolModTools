#!/bin/bash
#
#   rescore docked poses using PB/SA

export PATH=/u/jr/contrib/dock6/bin:$PATH

if [ ! -d hawkins ] ; then mkdir hawkins ; fi
cd hawkins

cp ../../../sh/gbsa_hawkins_1ary.in gbsa_hawkins.in

if [ ! -e params ] ; then ln -s ../params . ; fi
if [ ! -e receptor.mol2 ] ; then ln -s ../receptor.mol2 . ; fi

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
    else
            echo "ERROR: NOTHING TO SCORE"
    fi
fi

if [ ! -s gbsa_hawkins_scored.mol2 ] ; then
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
