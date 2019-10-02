#!/bin/bash

export PATH=$HOME/contrib/AMMOS_ProtLig/bin:$PATH

if [ ! -e best.mol2 ] ; then
    echo "Nothing to score"
    exit
fi

mkdir -p ammos
cd ammos
if [ ! -e docked.mol2 ] ; then
    ln -s ../best.mol2 docked.mol2
fi
if [ ! -e receptor.pdb ] ; then
    ln -s ../receptor.pdb .
fi

if [ ! -e ammos.in ] ; then
cat > ammos.in <<END
path_of_AMMOS_ProtLig= $HOME/contrib/AMMOS_ProtLig/
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

