
cg_gro=${1:-CG_posre.gro}
name=${2:-''}
if [ "$name" = '' ] ; then
    name=${cg_gro%.gro}
fi

# make PDB file
if [ ! -e ${name}.CG.pdb ] ; then
    gmx trjconv -f $cg_gro -s $name.tpr -pbc mol -conect -o ${name}.CG.pdb 
fi
# extract backbone
grep ' BB ' ${name}.CG.pdb | sed -e 's/ BB / CA /g' > ${name}.CG-ca.pdb
# make full-atom PDB
~/contrib/UniCon3D/pulchra_306/pulchra -g -v ${name}.CG-ca.pdb
mv ${name}.CG-ca.rebuilt.pdb ${name}.FA.pdb
