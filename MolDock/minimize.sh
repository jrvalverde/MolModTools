#!/bin/bash
#
#	Perform an MD simulation in vacuo after an initial minimization
#
#	Usage:	minimize.sh `pwd`/file.pdb
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

if [ ! -d minim ] ; then mkdir minim ; fi
cd minim
#
if [ -e DONE ] ; then exit ; fi
#
ln -s ../`basename $f` model.pdb
#
#
##
## Convert from PDB to GROMACS format
##
pdb2gmx -water spc -f model.pdb -p model.top -o model.gro -ignh -ff gromos53a6
#
##
## set up simulation box
##
editconf -f model.gro -o model_box.gro -c -d 1.0 -bt dodecahedron
#
##
## fix the model.top file
##	comment out lines containing CA2+ (insert ; at beginning)
##	add CA2+	2	to [ molecules ] section
cat model.top | sed -e '/CA2\+/s/^/;/g' > tmp
mv tmp model.top
cat >>model.top<<END
CA2+	2
END
#
# Set up MD minimization run
#
ln -s $etc/minimize.mdp .

grompp -f minimize.mdp -c model_box.gro -p model.top -o minim.tpr

#
# Run minimization described in input.tpr
#
mdrun -nice 0 -v -s minim.tpr -o minim.trr -c minim.gro -e minim.edr 

#
# Make plot of potential energy evolution during the run
#
g_energy -f minim.edr -o minim_energy.xvg <<END02
10 0
END02

#
# and display it
#
#JR# xmgr minim_ener.xvg 

touch DONE
