#!/bin/bash

export PATH=/u/jr/contrib/AMMOS_ProtLig/bin:$PATH

mkdir -p ammos
cd ammos

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


if [ ! -e receptor.pdb ] ; then
    ln -s ../receptor.pdb .
fi

if [ ! -e ammos.in ] ; then
cat > ammos.in <<END
path_of_AMMOS_ProtLig= /u/jr/contrib/AMMOS_ProtLig/
protein= receptor.pdb
bank= docked.mol2
case_choice= 3 
radius= 6
END
fi

if [ ! -d docked_case_3_OUTPUT ] ; then
AMMOS_ProtLig_sp4.py ammos.in <<END
1
END
fi

cd ..

