#!/bin/bash
#
GDOCK=~/contrib/GalaxyDock
BITS=64bit
SYS=linux

# Next are in Angstroms
GRID_WIDTH="0.375"
XTRA_SPC="4"

# Prepare receptor and ligand molecules
# -------------------------------------
#	Requires the following files
#	00_orig/receprot.pdb	- Hydrogens should be attached
#	00_orid/ligands/*.mol2	- Appropriate atomic charges
#				should be assigned and hydrogens
#				must be added too.
#
echo "Preparing receptor and ligand molecules"
if [ ! -d 01_structure_preparation ] ; then
	mkdir 01_structure_preparation
fi
cd 01_structure_preparation/
ln -s ../00_orig/receptor.pdb rec.pdb
# find extreme coordinates
#X
MAXX=`cat rec.pdb | grep ^ATOM | cut -b 31-38 | sort -g | tail -1`
MINX=`cat rec.pdb | grep ^ATOM | cut -b 31-38 | sort -g | head -1`
#X
MAXY=`cat rec.pdb | grep ^ATOM | cut -b 39-46 | sort -g | tail -1`
MINY=`cat rec.pdb | grep ^ATOM | cut -b 39-46 | sort -g | head -1`
#X
MAXZ=`cat rec.pdb | grep ^ATOM | cut -b 47-54 | sort -g | tail -1`
MINZ=`cat rec.pdb | grep ^ATOM | cut -b 47-54 | sort -g | head -1`
#
# Protein preparation
#	Generate PDBQS file rec.pdbqs
#	(PDB + Q(charge) + S(solvation)
#
if [ ! -e rec.pdbqs ] ; then
    $GDOCK/script/pdb2pdbqs rec.pdb $GDOCK $SYS
fi
# Ligand preparation
if [ ! -d ligands ] ; then
    mkdir ligands
fi
#
# Ligand preparation
#	generate corresponding ligand/*.pdbq files
#
cd ligands/
ln -s ../../00_orig/ligands/* .
for i in *.mol2 ; do
    if [ ! -e `basename $i mol2`pdbq ] ; then
        $GDOCK/script/gen_ligand $i $GDOCK $SYS
    fi
done
cd ..
cd ..

#
# Generate ligand conformation library
# ------------------------------------
#
#	This step will generate a library of PDBQ files with starting
# conformations for each ligand, as well as a gen_conf.in.beta and a
# gen_conf.in.full files, containing additional information.
#
#	Requires a file gen_conf.in that will be generated for each
# ligand using a template in $GDOCK/cnb/ligand_gen_conf_rigid.in
#
#	The input file should contain:
#
# ! (1) I/O parameters: Controls the input and output
# !
# data_directory : Location of data directory
# task_mode      : Specifying the type of work. Please set the parameter as
# !	         'gen_conf', which means conformation generation.
# infile_pdb     : Receptor structure
# infile_ligand  : Ligand structure
# infile_mol2    : Ligand mol2 file, to calculate torsion term of PLP score
# conf_prefix    : Prefix for output. After finishing, output file names will 
# !	         be started using conf_prefix.
# !
# ! (2) Energy Parameters: Energy options for conformation generation
# !
# PLP_weight     : Relative weight of PLP part. [0.1]
# allow_bump     : Number of clashes of ligand before minimization.
# !	         After random values were assigned to torsion of ligand, 
# !		 generate ligand conformation and check a number of clashes. 
# !		 If number of clashes is larger than allow_bump, 
# !		 the conformation is discarded. [0]
# int_E_cut      : Energy cut of conformations. After minization, calculate
# !	         energy of that conformation. If the energy is higher than
# !		 int_E_cut, the conformation is buried also. [10.0]
# !
# ! (3) Library Parameters
# !
# n_conf         : Number of conformations in library. [50]
# n_gen_trial    : Maximum number of trials to make library. If the number of 
# !	         trials excceds n_gen_trial not making library, the program 
# !		 stops. [10000]
# 
#
echo "Generating library(ies) of ligand conformers"
if [ ! -d 02_library_generation ] ; then
    mkdir 02_library_generation
fi
cd 02_library_generation
# ligand conformation library generation
for i in ../00_orig/ligands/*.mol2 ; do
    LIG=`basename $i .mol2`
    if [ ! -e $LIG.gen_conf.in ] ; then
        cat $GDOCK/cnb/ligand_gen_conf_rigid.in.tmpl | sed -e "s%GDOCK%$GDOCK%g" \
            -e "s%LIG%$LIG%g" > $LIG.gen_conf.in
    fi
    if [ ! -e $LIG.gen_conf.in.beta ] ; then
    	$GDOCK/bin/$BITS/gen_lig_conf $LIG.gen_conf.in > $LIG.gen_conf.log
    fi
    #mv box.pdb $LIG.box.pdb
    #mv out.trj $LIG.out.trj
    echo $LIG
done
cd ..

#
# Pre-docking using beta-shape
# ----------------------------
#
#	Generate initial bank candidates. This is guided by a file pre_dock.in
# that will be generated for each ligand from a template in 
# $GDOCK/cnb/rigid_pre_dock.in.tmpl
#
# ! (1) I/O parameters: Controls the input and output
# !
# infile_pdb     : Receptor structure
# out_lig_prefix : Prefix for output. After finishing, output file names will 
# !	         be started using conf_prefix.
# !
# ! (2) Docking Box Parameters: Energy options for conformation generation
# !
# grid_box_cntr  : (x,y,z) coordinate of center of docking box. If this is not
# !	         given, the geometrical center of ligand is selected.
# grid_n_elem    : Number of grid points for each direction. This is should be 
# !	         given in odd number. [61 61 61]
# grid_width     : Grid space between points in angstrom. [0.375]
# !
# !The docking box ranges from [grid_cntr - grid_width * (grid_n_elem - 1) / 2]
# !to [grid_cntr + grid_width * (grid_n_elem - 1) / 2]
# !
# ! (3) Library Parameters
# !
# center_atom    : Index of geometrical center of ligand. If you don't know
# !	         this, please refer a file that ends with beta generated
# !		 in previous step.
# n_gen_conf     : Number of generated pose of each ligand conformation in
# !	         library.
# beta_value     : A radius of a probe to generate beta-shape of receptor.
# offset_value   : Tangent sphere radius to locate ligand atom.
# infile_ligand  : File names of conformations in library. All the names should
# !		 be separated by semi-colon ';'
# 

echo "Running pre-dock"
if [ ! -d 03_running_pre-dock ] ; then
    mkdir 03_running_pre-dock
fi
cd 03_running_pre-dock

# Use bc for clarity (dc uses reverse polish notation and _ instead of - for sign)
XSTEPS=`echo "scale=0; ($MAXX - ($MINX - $XTRA_SPC))/$GRID_WIDTH" | bc -l`
if [ $(( $XSTEPS % 2 )) -eq 0 ] ; then XSTEPS=$(( $XSTEPS + 1)) ; fi

YSTEPS=`echo "scale=0; ($MAXY - ($MINY - $XTRA_SPC))/$GRID_WIDTH" | bc -l`
if [ $(( $YSTEPS % 2 )) -eq 0 ] ; then YSTEPS=$(( $YSTEPS + 1)) ; fi

ZSTEPS=`echo "scale=0; ($MAXZ - ($MINX - $XTRA_SPC))/$GRID_WIDTH" | bc -l`
if [ $(( $ZSTEPS % 2 )) -eq 0 ] ; then ZSTEPS=$(( $ZSTEPS + 1)) ; fi

XCENTER=`echo "scale=6; ($MAXX + $MINX) / 2" | bc -l`
YCENTER=`echo "scale=6;($MAXY + $MINY) / 2" | bc -l`
ZCENTER=`echo "scale=6;($MAXZ + $MINZ) / 2" | bc -l`

# pre-docking using beta-shape
for i in ../00_orig/ligands/*.mol2 ; do
    LIG=`basename $i .mol2`
    GRCNTR=`grep grid_box_cntr ../02_library_generation/$LIG.gen_conf.in.beta`
    GRCNTR="grid_box_cntr $XCENTER $YCENTER $ZCENTER"
    ATCNTR=`grep center_atom ../02_library_generation/$LIG.gen_conf.in.beta`
    LFILES=`ls -1 ../02_library_generation/${LIG}*.pdbq | sed -e ':a;N;$!ba;s/\n/;/g'`
    if [ ! -e $LIG.pre_dock.in ] ; then
        cat $GDOCK/cnb/rigid_pre_dock.in.tmpl | \
           sed -e "s%GDOCK%$GDOCK%g" -e "s/LIG/$LIG/g" \
               -e "s/GRCNTR/$GRCNTR/g" -e "s/ATCNTR/$ATCNTR/g" \
               -e "s%LFILES%$LFILES%g" -e "s/XSTEPS/$XSTEPS/g" \
               -e "s/YSTEPS/$YSTEPS/g" -e "s/ZSTEPS/$ZSTEPS/g"> $LIG.pre_dock.in
    fi

    if [ ! -e $LIG.pre_dock.log ] ; then
        $GDOCK/bin/$BITS/PreBetaDock . $LIG.pre_dock.in > $LIG.pre_dock.log
    fi
    echo $LIG
done
cd ..

#
# Run GalaxyDock
# --------------
#
#	Take pre-docked initial configurations and dock them with GalaxyDock 
# following instructions in GalaxyDock.in, which is generated for each ligand
# from $GDOCK/cnb/rigid_GalaxyDock.in.tmpl
#
# ! (1) I/O parameters: Controls the input and output
# ! 
# data_directory : Location of data directory
# task_mode      : Denotes what type of this work is. 'csa' means that
# ! 		 you will run docking program.
# infile_pdb     : Receptor structure
# infile_ligand  : Ligand structure
# infile_mol2    : Ligand mol2 file, to calculate torsion term of PLP score
# outfile_prefix : Prefix for output. After finishing docking calculation, all 
# ! 		 output file names will be start using outfile_prefix.
# print_bank_evol: To see the evolution of bank or not. [no]
# ! 
# ! (2) Grid options: Docking box boundary
# ! 
# grid_box_cntr  : (x,y,z) coordinate of center of docking box. If this is not
# ! 	         given, the geometrical center of ligand is selected.
# grid_n_elem    : Number of grid points for each direction. This is should be 
# ! 	         given in odd number. [61 61 61]
# grid_width     : Grid space between points in angstrom. [0.375]
# ! 
# ! The docking box ranges from [grid_cntr - grid_width * (grid_n_elem - 1) / 2]
# ! to [grid_cntr + grid_width * (grid_n_elem - 1) / 2]
# ! 
# ! (3) Energy parameters
# ! 
# PLP_weight      : Weight for PLP torsion term to calculate ligand internal 
# ! 		  energy. [0.1]
# ! 
# ! (4) CSA parameters
# ! 
# csa_bank        : Size of bank. [30 30]
# csa_n_opr_x     : Number of operations for CSA cycle. 
# ! 		  The first two swap translation and rotation of ligand.
# ! 		  The mid two swap torsion angle of ligand.
# ! 		  The last two swap side-chain of protein.
# ! 		  [1 2 1 2 1 2]
# !  
# ! (5) Initial bank parameters
# ! 
# e1max           : The first cut to accept initial bank. If energy of generated
# ! 		  initial bank is lower than e1max, it is minimized by Simplex.
# ! 		  [500000.0]
# e0max           : The last cut for accepting initial bank. After minimizing the 
# ! 		  conformation, evaluate energy, and if the energy is lower than
# ! 		  e0max, it is selected to make initial bank.
# ! 		  [0.0]
# ligand_method   : Side-chain attachment method. Two options are available.
# ! 		  'Voro' means that you will use initial bank from pre-generated
# ! 		  conformation generated by beta-shape.
# ! 		  'rand' generates initial bank by assigning random values.
# ! 		  [rand]
# ! 
# ! If you select 'rand';
# !    max_trial       : Number of trials to generate inital bank. [5000]
# ! 
# ! or if you select 'Voro'
# !    n_conf          : Number of generated conformations in library.
# !    n_gen_conf      : Number of pre-docked pose for each library conformations
# !    voro_prefix     : Prefix of pre-docked pose that you used in step 3.
# 
# After docking, there will be for each ligand PDB and info files describing
# (1)	*_fb_*.pdb
#	*_fb.info
# Energy: GalaxyDock score of each bank.
# l_RMSD: Ligand RMSD from reference structure. (pdbq file)
# p_Chi : Hamming distance of receptor conformation between bank and initial 
# 	structure.
# LIG_E : Protein-ligand intermolecular energy.
# INT_E : Ligand internal energy calculated from AutoDock score.
# PROT_E: Lennard-Jones van der Waals of flexible side-chains.
# PLP_E : Ligand internal energy calculated from PLP torsion term.
# ROTA_E: ROTA score of flexible side-chains.
# 
# To view the top scoring pose, open [out_file_prefix]_fb_0001_lig.pdb and
# [out_file_prefix]_fb_0001_prot.pdb using any molecule viewer.
#
# (2)	*_cl_*.pdb
#	*_cl.info
# Information about clustering the final bank (population and representative
#	conformation are in *_cl.log file)
#
# (3)	*_ib_*,pdb
#	*_ib.info
# Infomration and conformation of initial bank
#
# (4)	box.pdb
# Docking box boundaries
#

echo "Running rigid docking"
if [ ! -d 04_running_rigid_docking ] ; then
    mkdir 04_running_rigid_docking
fi
cd 04_running_rigid_docking

# Running GalaxyDock
for i in ../00_orig/ligands/*.mol2 ; do
    LIG=`basename $i .mol2`
    GRCNTR=`grep grid_box_cntr ../02_library_generation/$LIG.gen_conf.in.beta`
    GRCNTR="grid_box_cntr $XCENTER $YCENTER $ZCENTER"
    ATCNTR=`grep center_atom ../02_library_generation/$LIG.gen_conf.in.beta`
    if [ ! -e $LIG.GalaxyDock.in ] ; then
        cat $GDOCK/cnb/rigid_GalaxyDock.in.tmpl | \
           sed -e "s%GDOCK%$GDOCK%g" -e "s/LIG/$LIG/g" \
               -e "s/GRCNTR/$GRCNTR/g" -e "s/ATCNTR/$ATCNTR/g" \
               -e "s/XSTEPS/$XSTEPS/g" -e "s/YSTEPS/$YSTEPS/g" \
               -e "s/ZSTEPS/$ZSTEPS/g" > $LIG.GalaxyDock.in
    fi
           
    if [ ! -e $LIG.GalaxyLog.log ] ; then
        $GDOCK/bin/$BITS/galaxy_dock $LIG.GalaxyDock.in > $LIG.GalaxyDock.log
    fi
    mv box.pdb $LIG.box.pdb
    #mv out.trj $LIG.out.trj
    echo $LIG
done
cd ..

#
# Binding affinity prediction
# ---------------------------
#
#	Requires the ligand pdbq files, ligand mol2 files and 
# docking information files
#
if [ ! -e 05_binding_affinity_prediction ] ; then
    mkdir 05_binding_affinity_prediction
fi
cd 05_binding_affinity_prediction
for i in ../00_orig/ligands/*.mol2 ; do
    LIG=`basename $i .mol2`
    $GDOCK/scripts/calc_BA ../01_structure_preparation/ligands/$LIG.pdbq \
                           ../01_structure_preparation/ligands/$LIG.mol2 \
                           ../04_running_rigid_docking/$LIG.GalaxyDock_fb.info \
                           $GDOCK $BITS
    mv binding_affinity.dat $LIG.binding_affinity.dat
done
cd ..
