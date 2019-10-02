#!/bin/bash
#   Rescore using DSX

# use these on Xistral
#export ARCH=linux64
#export EXE_ARCH=linux_64
#export PATH=/u/jr/contrib/dsx/$ARCH:$PATH
#export LD_LIBRARY_PATH=/u/jr/contrib/dsx/$ARCH:$LD_LIBRARY_PATH

# use these on NGS
export ARCH=RHEL_linux32
export EXE_ARCH=rhel_linux_32
export PATH=/u/jr/contrib/dsx/$ARCH:$PATH
export LD_LIBRARY_PATH=/u/jr/contrib/dsx/$ARCH:$LD_LIBRARY_PATH

DSX=~/contrib/dsx/$ARCH/dsx_$EXE_ARCH.lnx

# maximum running time of DSX (m = min., s = sec.)
tmo=10m
# grace timeout (15 sec.)
gto=15s
#
# this is to deal with inconsistencies in the timeout command
# across different versions of Ubuntu
#
#	This works in Ubuntu 11.10 (e.g. xistral)
#
#toopt="$tmo -k $gto"
#
#	This works in Ubuntu 10.04.2 [seconds] (e. g. ngs)
#
toopt="300"

if [ ! -d dsx ] ; then mkdir dsx ; fi

cd dsx

if [ ! -e known.mol2 ] ; then ln -s ../known.mol2 . ; fi

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

#
# set a timeout of 5' (300") on each
#	$! expands to the PID of the most recently executed background command:
#	( some-large-command ) & sleep 300 ; kill $!
# the above has a problem in that if the command ends earlier we still wait
# the specified timeout.
#
# The solution by J. R. Valverde in EMBnet.news is better, but we may also
# use now the 'timeout' command from coreutils, which returns 124 if the
# command was terminated by a timeout
#
#	Note: for the EMBnet.News solution try the following
#
# (submitter=$$ ; (sleep $tmo ; pkill -P $submitter)& large-command)& wait $!
# or 
# (submitter=$! ; (sleep $tmo ; kill $submitter)& large-command)& wait $!
#

echo -n "Scoring poses with DSX .."

if [ ! -e DSX_receptor_docked.tot.txt ] ; then
    timeout $toopt $DSX \
	-P receptor.mol2 -L docked.mol2 -R known.mol2 \
	-I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 1 \
	-T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
    if [ $? -ne 124 ] ; then
        mv DSX_receptor_docked.txt DSX_receptor_docked.tot.txt
    fi
fi

if [ ! -e DSX_receptor_docked.rmsd.txt ] ; then
    timeout $toopt $DSX \
	-P receptor.mol2 -L docked.mol2 -R known.mol2 \
	-I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 4 \
	-T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
    if [ $? -ne 124 ] ; then
    	mv DSX_receptor_docked.txt DSX_receptor_docked.rmsd.txt
    fi
fi

if [ ! -e DSX_receptor_docked.txt ] ; then
    timeout $toopt $DSX \
	-P receptor.mol2 -L docked.mol2 -R known.mol2 \
	-I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 0 \
	-T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
fi

echo " done"
####
echo -n "Scoring reference with DSX .."

if [ ! -e DSX_receptor_known.tot.txt ] ; then
    timeout $toopt $DSX \
	-P receptor.mol2 -L known.mol2 -R known.mol2 \
	-I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 1 \
	-T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
    if [ $? -ne 124 ] ; then
        mv DSX_receptor_known.txt DSX_receptor_known.tot.txt
    fi
fi

if [ ! -e DSX_receptor_known.rmsd.txt ] ; then
    timeout $toopt $DSX \
	-P receptor.mol2 -L known.mol2 -R known.mol2 \
	-I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 4 \
	-T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
    if [ $? -ne 124 ] ; then
    	mv DSX_receptor_known.txt DSX_receptor_known.rmsd.txt
    fi
fi


if [ ! -e DSX_receptor_known.txt ] ; then
    timeout $toopt $DSX \
	-P receptor.mol2 -L known.mol2 -R known.mol2 \
	-I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 0 \
	-T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
fi

echo "done"

cd ..
