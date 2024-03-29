#!/bin/bash

for j in 8ogtp gtp Mg8OGTP-- MgGTP-- ; do
    cd $j
    for i in * ; do
	if [ -e flexible.out ] ; then continue ; fi
        cd $i
	echo $j/$i
	rm -f core flexible* STD*
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
vdw_defn_file                                                ./params/vdw_AMBER_parm99.defn
flex_defn_file                                               ./params/flex.defn
flex_drive_file                                              ./params/flex_drive.tbl
ligand_outfile_prefix                                        flexible
write_orientations                                           yes
num_scored_conformers                                        100
write_conformations					     no
cluster_conformations                                        yes
cluster_rmsd_threshold                                       2.0
rank_ligands                                                 no

END
#   this works for most cases
#	qsub -l num_proc=1,s_rt=10:00:00,s_vmem=1G,h_fsize=1G <<ENDSUB
#   try with more memory for big grid files
#	qsub -l num_proc=1,s_rt=10:00:00,s_vmem=2G,h_fsize=1G <<ENDSUB
#   1dg3 requires 4GB memory (from NGS, an x86_64 machine)
#	qsub -l num_proc=1,s_rt=10:00:00,s_vmem=4G,h_fsize=1G <<ENDSUB
#   this is for our simpler queue system
	qsub -q slow -e `pwd` -o `pwd` -N FLEXIBLE <<ENDSUB
    	    cd $HOME/work/a/newdock6/$j/$i
	    echo `pwd`
	    $HOME/contrib/dock6/bin/dock6 -i flexible.in -o flexible.out
ENDSUB
    cd ..
    done
    cd ..
done
