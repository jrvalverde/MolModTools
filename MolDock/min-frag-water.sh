#!/bin/bash
#
#	Perform a simulation in solution after an initial minimization
#
#	Usage:	min-water.sh `pwd`/file.pdb
#	     	f=`pwd`/file.pdb min-water.sh
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
#etc=/home/cnb/jrvalverde/work/britt/simul/etc
etc=/home/jr/work/britt/KChIP3/simul/etc

# group to add ions to (depends on the number of groups detected), msut be SOL
# for Ca++Mg++ in gromacs 4.5.4 SOL is 19 
# for Ca++ in gromacs 4.5.4 SOL is 15
# for Mg++ in gromacs 4.5.5 SOL is 15
# for no ions in gromas 4.5.4 SOL is 13
grp=13

export PATH=~/bin:$PATH

f=${f:-$1}
dir=`dirname $f`
base=`basename $f .pdb`

# Change to working directory and set up link to model data file
cd $dir

###
###	PREPARATION
###

if [ ! -d solution ] ; then mkdir solution ; fi
cd solution

if [ -e H2O_MINIMIZED ] ; then exit ; fi

# start from minimized structure
if [ ! -e model.pdb ] ; then
#    ln -s ../`basename $f` model.pdb
    cat ../`basename $f` | sed -e 's/MG2/MG /g' > model.pdb
fi

#
#
##
## Convert from PDB to GROMACS format, generate topology and position restrains
## and include information on water using SPC model
##
if [ ! -e model.gro ] ; then
    cat<<MSG
-----------------------------------------------------------------------------
    				PDB2GMX
-----------------------------------------------------------------------------
MSG
    if [ ! -e posre.itp ] ; then ln -s $etc/fragment-posre.itp posre.itp ; fi

    pdb2gmx -water spc -f model.pdb -p model.top -o model.gro -ignh \
    	-ff gromos53a6 -i posre.itp > pdb2gmx.log 2>&1

#
# The following corrections may be needed for some PDB files
#
    #
    # fix the model.top file
    #	comment out lines containing CA2+ (insert ; at beginning)
    #	add CA2+	2	to [ molecules ] section (which is the last one)

#    sed model.top  -e '/CA2/s/^/;/g' > tmp
#    echo "CA2+    2" >> tmp
#    mv tmp model.top

    # fix the posre.itp file
    #	need to comment lines corresponding to Ca2+ atoms (two last lines)
#    head -n -2 posre.itp > tmp
#    mv tmp posre.itp
fi

# set up simulation box as a rhombic dodecahedron, centered and at least 1.0nm
# from the molecule on each side

if [ ! -e model_box.gro ] ; then
    cat<<MSG
-----------------------------------------------------------------------------
				EDITCONF
-----------------------------------------------------------------------------
MSG

    editconf -f model.gro -o model_box.gro -c -d 1.0 -bt dodecahedron
fi

# fill the box with water

if [ ! -e model_solv.gro ] ; then
    cat<<MSG
-----------------------------------------------------------------------------
    				GENBOX
-----------------------------------------------------------------------------
MSG
    genbox -cp model_box.gro -cs spc216.gro -o model_solv.gro -p model.top
fi

# add ions to compensate protein charges
#   these need not be 4+ due to the two Ca++, but may be something else
#   due to protein charges, the last protein atom contains (qtot) the
#   protein total charge, and the last Ca++ atom the total charge of the
#   system (e.g. for e26w, atom 1017 has qtot -8 and 1019 (last Ca++) has
#   a qtot of -4).
# first generate atomistic description of the system

if [ ! -e ions.mdp ] ; then
    ln -s $etc/ions.mdp .
fi

if [ ! -e ions.tpr ] ; then
    cat<<MSG
-----------------------------------------------------------------------------
    				GROMPP IONS
-----------------------------------------------------------------------------
MSG
    grompp -f ions.mdp -c model_solv.gro -p model.top -o ions.tpr 
fi

# now add enough ions to neutralize charges to SOL group 
#	(substitute waters by ions), with -conc a specific concentration can be
#	also specified in millimol/liter:
#	* physiological serum is ~154mM, -conc ~0.154,
#	* intracellular ionic strength is ~200-215-250mM 
#	  -- Mouat MF, Manchester KL (1998)
#	  The intracellular ionic strength of red cells and the influence of 
#	  complex formation. Comp Hematol Int 8:58-60
#	* Mg++ concentration in mammalian cells is 0.2-0.6mM
#	  -- Levy LA, Murphy E, Raju B, London RE (1988) 
#	     Measurement of cytosolic free magnesium ion concentration 
#	     by 19F NMR. Biochemistry 27:4041-4048
#	  -- Rotevatn S, et al. (1989) Cytosolic free magnesium concentration 
#	     in cultured chick heart cells. Am J Physiol 257:C141-C146
#	* See also http://www.ncbi.nlm.nih.gov/books/NBK21627/
#		Ion	Cell	Blood (mM)
#		K+	 139	   4
#		Na+	  12	 145
#	  	Cl-	   4	 116
#		HCO3-	  12	  29
#		X-	 138	   9 (proteins have a net negative charge)
#		Mg++	   0.8	   1.5
#		Ca++	<0.0002	   1.8
#
#	Input to genion: select SOL group, should be 13
#
if [ ! -e model_ions.gro ] ; then
    cat<<MSG
-----------------------------------------------------------------------------
    				GENION
-----------------------------------------------------------------------------
MSG

  if [ -e pdb2gmx.log ] ; then
    chg=`grep -i 'total charge' pdb2gmx.log | cut -d: -f2`
    # compute charge and type to add
    echo $chg | grep '-' &> /dev/null
    if [ $? -eq 0 ] ; then
       chg=`echo $chg | cut -d- -f2 | cut -d. -f1`
       typ='-np'
    else
       chg=`echo $chg | cut -d. -f1`
       typ='-nn'
    fi
  fi
#echo "genion -s ions.tpr -o model_ions.gro -p model.rop -pname NA+\\"
#echo "    -nname CL- -neutral $typ $chg"
#genion -s ions.tpr -o model_ions.gro -p model.top -pname NA+ \
#       -nname CL- -neutral $typ $chg<<ENDION
#13
#ENDION
genion -s ions.tpr -o model_ions.gro -p model.top -pname NA \
	-nname CL -neutral -conc 0.215<<ENDION
$grp
ENDION
fi

###
###	READY
###

#
# Set up MD minimization run
#
if [ ! -e minim.mdp ] ; then
    ln -s $etc/fragres-minim.mdp minim.mdp
fi

if [ ! -e minim.tpr ] ; then
    cat<<MSG
-----------------------------------------------------------------------------
    				GROMPP MINIM
-----------------------------------------------------------------------------
MSG
    grompp -f minim.mdp -c model_ions.gro -p model.top -o minim.tpr
fi

#
# Run minimization described in minim.tpr
#	Save trajectory in minim.trr, energies in minim.edr
#	log in minim.log and geometry in minim.gro
#

if [ ! -e minim.trr ] ; then
    cat<<MSG
-----------------------------------------------------------------------------
    				MDRUN MINIM
-----------------------------------------------------------------------------
MSG
    #mdrun -v -s minim.tpr -o minim.trr -c minim.gro -e minim.edr 
    #mdrun -v -deffnm minim
    # on NGS/XISTRAL use
    mpirun -np 32 mdrun_mpi -v -deffnm minim < /dev/null
fi

# generate minimized PDB file
editconf -f minim.gro -o minim.pdb < /dev/null

# get rid of water and ions so they do not get in our way for visualization
cat minim.pdb | grep -v ' SOL ' | grep -v ' NA ' | grep -v ' CL ' > minim_nosol.pdb

#
# Make plot of potential energy evolution during the run
#
if [ ! -e minim_energy.xvg ] ; then
g_energy -f minim.edr -o minim_energy.xvg <<END02
9 0
END02
fi

#
# and display it
#
#JR# xmgrace minim_energy.xvg 

touch H2O_MINIMIZED

exit

