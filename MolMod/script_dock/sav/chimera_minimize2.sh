# 
# we do not want DockPrep to be run because it will remove water

f=`basename $1 .pdb`

chimera --nogui <<END
open $f.pdb
# we do not want addh because they are already there, have been fixed and
#	chimera adds an spurious H to ATP
#addh inisolation true hbond true useHisName true useGluName true useAspName true useLysName true useCysName true
#addcharge all chargeModel ff12SB method am1
addcharge all chargeModel 99sb method am1
minimize prep true freeze none nogui true
write format mol2 0 ${f}_minimized.mol2
write format pdb  0 ${f}_minimized.pdb
END
