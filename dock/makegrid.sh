#!/bin/bash

for i in * ; do
    echo "Making grid for $i" >> ../log
    cd $i
    cat > grid.in <<END
compute_grids                  yes
grid_spacing                   0.3
output_molecule                no
contact_score                  no
energy_score                   yes
energy_cutoff_distance         9999
atom_model                     a
attractive_exponent            6
repulsive_exponent             9
distance_dielectric            yes
dielectric_factor              4
bump_filter                    yes
bump_overlap                   0.75
receptor_file                  $i.mol2
box_file                       receptor_box.pdb
vdw_definition_file            /home/jr/contrib/dock6/parameters/vdw_AMBER_parm99.defn
score_grid_prefix              grid
END
    grid -i grid.in > grid.stdout 
    cd ..
done
