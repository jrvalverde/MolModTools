#!/bin/bash
#
#	Perform an MD simulation in vacuo after an initial minimization
#
#	Usage:	sim-vacuo.sh `pwd`/file.pdb
#	     	f=`pwd`/file.pdb sim-vacuo.sh
#
#	Use second form to submit to job queues.
#
#	Expects the model to be already set up in the especified working
# directory. See below for details.
#
#	This script will use the simulation values in the corresponding
# files under 'etc'. Currently that means a minimization step of up to
# 50000 0.01 steps with a tolerance of 1000 kJ/mol/nm, and a dynamics
# step of 1000000 0.002 steps (2 ns total) using PME.
#
#	(C) Jose R. Valverde, EMBnet/CNB. 2011

#customization
#	etc - directory containing directive files for gromacs
etc=/home/cnb/jrvalverde/work/britt/simul/etc

export PATH=/opt/gromacs-4.5.3/bin:$PATH

f=${f:-$1}
dir=`dirname $f`
base=`basename $f .pdb`

# Change to working directory and set up link to model data file
cd $dir

###
###	THIS SHOULD BE YOUR PREPARATION
###

#if [ ! -d vacuo ] ; then mkdir vacuo ; fi
#cd vacuo
#
#if [ -e DONE ] ; then exit ; fi
#
#ln -s ../`basename $f` model.pdb
#
#
##
## Convert from PDB to GROMACS format
##
#pdb2gmx -water spc -f model.pdb -p model.top -o model.gro -ignh <<END
#4
#END
#
##
## set up simulation box
##
#editconf -f model.gro -o model_box.gro -c -d 1.0 -bt cubic
#
##
## fix the model.top file
##	comment out lines containing CA2+ (insert ; at beginning)
##	add CA2+	2	to [ molecules ] section

#
# Set up MD minimization run
#
ln -s $etc/minim.mdp .

grompp -f minim.mdp -c model_box.gro -p model.top -o input.tpr

#
# Run minimization described in input.tpr
#
mdrun -nice 0 -v -s input.tpr -o minim_traj.trr -c minimized.gro -e minim_ener.edr 

#
# Make plot of potential energy evolution during the run
#
g_energy -f minim_ener.edr -o minim_ener.xvg <<END02
9 0
END02

#
# and display it
#
#JR# xmgr minim_ener.xvg 


#
# Prepare input for MD run
#
ln -s $etc/vacuomd.mdp .

grompp -f vacuomd.mdp -c minimized.gro -p model.top -o vacuomd.tpr

#
# And run the MD simulation specified by fullmd.tpr
#
mdrun -nice 0 -v -s vacuomd.tpr -o md_traj.trr -x md_traj.xtc -c md_final.gro -e md_ener.edr
#
# Compare original and final structures
#
g_confrms -f1 model.pdb -f2 md_final.gro <<END03
4
4
END03

#JR# rasmol fit.pdb

#
# Compute RMS variation during the simulation run
#
g_rms -s vacuomd.tpr -f md_traj.xtc -dt 10 <<END04
4
4
END04

#JR# xmgr rmsd.xvg

#
# Analyze changes in secondary structure
#
do_dssp -s vacuomd.tpr -f md_traj.xtc -dt 10 

#JR# xv ss.xpm

xpm2ps -f ss.xpm -o ss.eps

#JR# xpsview ss.eps

