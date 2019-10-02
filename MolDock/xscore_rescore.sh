#!/bin/bash

. ~/contrib/xscore/setup.sh

if [ ! -d xscore ] ; then
    mkdir -p xscore
fi
cd xscore

if [ ! -e receptor.pdb ] ; then ln -s ../receptor.pdb . ; fi
if [ ! -e receptor.mol2 ] ; then ln -s ../receptor.mol2 . ; fi

for i in \
	../flex-*_secondary_conformers.mol2 \
        ../flex-*_primary_conformers.mol2 \
        ../flexible_scored.mol2 \
        ../rigid_scored.mol2 \
        ../rec+lig_*.mol2 ; do
    # if nothing matches a wildcard, the wilcard is passwd as argument
    if [ ! -e "$i" ] ; then continue ; fi
    if [ -s "$i" ] ; then
	nam=`basename $i .mol2`
        echo "Scoring $nam"
        xscore -score receptor.pdb $i | grep -v "Warning:" > ${nam}_xscore.sum
        mv xscore.log ${nam}_xscore.log
        continue
        # the rest is ignored, left for future, finer control
        echo Scoring $nam
        mkdir -p $nam
        cd $nam
        ln -s ../$i docked.mol2
        ln -s ../receptor.pdb .
        ln -s ../receptor.mol2 .
        #ln -s ../../../../sh/xscore.in .
        #xscore xscore.in
        cd ..
    fi
done
cd ..

exit

### THIS IS IGNORED ###

csplit -f lig -n 3 -z docked.mol2 '/^########## Name*/' '{*}' &> csplit.log

for i in lig* ; do 
	echo -n $i: 
	score receptor.pdb $i 
	mv score.log $i.log 
	mv score.mol2 $i.mol2
done
