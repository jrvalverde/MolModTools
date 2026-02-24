# 
# we do not want DockPrep to be run because it will remove water

pdb=$1
f=`basename $1 .pdb`

if [ -e chim_${f}_amber/$f_amber.pdb ] ; then echo "$f already done" ; exit ; fi

mkdir -p chim_${f}_amber

chimera --nogui <<END | tee chim_${f}_amber/$f_chimera.log
open $pdb
#addh inisolation true hbond true useHisName true useGluName true useAspName true useLysName true useCysName true
#addcharge all chargeModel ff12SB method am1
#addcharge all chargeModel 99sb method am1
# Minimize:
#   we will use dockprep to prepare the system
#   we can "select" some atoms first and freeze them
#   or we can "freeze" a specific atomspec directly
#   default values for nsteps and cgsteps are 100 and 10 respectively
#	ideally we would like at least 10 times more.
#   add "interval" 1 to 100 to collect energies after each 1 to 100 steps
minimize nogui true prep true nsteps 100 cgsteps 10 freeze none interval 1
write format mol2 0 chim_${f}_amber/${f}_amber.mol2
write format pdb 0 chim_${f}_amber/${f}_amber.pdb
END

echo "$f done"
