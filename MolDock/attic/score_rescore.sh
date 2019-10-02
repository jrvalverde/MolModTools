#!/bin/bash

export PATH=/u/jr/contrib/score/bin:$PATH

if [ ! -d score ] ; then
    mkdir -p score
fi
cd score

ln -s ../receptor.pdb .
ln -s ../receptor.mol2 .

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
        echo Scoring $nam
        mkdir -p $nam
        csplit -f $nam/lig -n 3 -z $i '/^########## Name*/' '{*}' &> $nam.log
        cd $nam
        ln -s ../receptor.pdb .
        ln -s ../receptor.mol2 .
        ln -s /home/jr/contrib/score/score/RESIDUE .
        ln -s /home/jr/contrib/score/score/ATOMTYPE .
        for j in lig* ; do
            echo "$j	"
            score receptor.pdb $j | grep -v "Warning:" | grep -v "This atom" 
            mv score.log $j.log
            mv score.mol2 $j.mol2
        done > all_scores.out 2> all_scores.err
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
