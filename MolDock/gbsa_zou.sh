#!/bin/bash
#
#   rescore docked poses using PB/SA

export PATH=/u/jr/contrib/dock6/bin:$PATH

if [ ! -d zou ] ; then mkdir zou ; fi
cd zou

cp ../../../sh/gbsa_zou_1ary.in gbsa_zou.in

if [ ! -e params ] ; then ln -s ../params . ; fi
if [ ! -e receptor.mol2 ] ; then ln -s ../receptor.mol2 . ; fi
if [ ! -e nchemgrid_GB ] ; then ln -s ../nchemgrid_GB . ; fi
if [ ! -e nchemgrid_SA ] ; then ln -s ../nchemgrid_SA . ; fi

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


if [ ! -s gbsa_zou_scored.mol2 ] ; then
    dock6 -i gbsa_zou.in -o gbsa_zou.out -v
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
gbsa_zou_score_primary                                       yes
gbsa_zou_score_secondary                                     no
gbsa_zou_gb_grid_prefix                                      ../nchemgrid_GB/gbsa_zou_grid
gbsa_zou_sa_grid_prefix                                      ../nchemgrid_SA/gbsa_zou_grid
gbsa_zou_vdw_grid_prefix                                     ../solvent_grid
gbsa_zou_screen_file                                         ../params/screen.in
gbsa_zou_solvent_dielectric                                  78.300003
gbsa_hawkins_score_secondary                                 no
pbsa_score_secondary                                         no
amber_score_secondary                                        no
minimize_ligand                                              yes
atom_model                                                   all
vdw_defn_file                                                ../params/vdw_AMBER_parm99.defn
flex_defn_file                                               ../params/flex.defn
flex_drive_file                                              ../params/flex_drive.tbl
ligand_outfile_prefix                                        gbsa_zou
write_orientations                                           no
num_scored_conformers                                        1
rank_ligands                                                 no
