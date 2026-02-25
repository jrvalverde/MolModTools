orig=$1
name=${orig%.*}
ext=${orig##*.}

pulchra=~/contrib/UniCon3D/pulchra_306/pulchra
cg="${name}.cg.pdb"
ca="${name}.ca.pdb"
aa="${name}.aa.pulchra.pdb"

if [ ! -s "${name}.cg.pdb" ] ; then
    if [ -s "${name}.gro" -a -s "${name}.tpr" ] ; then
	    echo "Converting $orig to $cg"
	    gmx trjconv -f "${name}.gro" -s "${name}.tpr" \
		    -pbc mol -conect -o "${cg}" 
	fi
else
    ln -s "${name}.pdb" "${cg}"
fi



rm ${aa}

echo "Extracting CA trace (BB)"
grep '^ATOM ' "${cg}" | grep ' BB ' | sed -e 's/ BB / CA /g' > $ca

mkdir -p chains.pulchra
for chain in `grep -E "^(ATOM  |HETATM)" $ca | \
	      cut -c 22 | \
              sort | \
              uniq` ; do
    echo "Extracting chain $chain from $ca"
    # check we can actually extract that chain
    if ! grep "^.\{21\}$chain" $ca > chains.pulchra/${name}_${chain}.ca.pdb ; then
        echo "ERROR: Something went wrong! Chain ${chain} not extracted"
        rm -f chains.pulchra/${name}_${chain}.ca.pdb
    else
        # if the chain was successfully extracted
	# run pulchra on it
	$pulchra -v chains.pulchra/${name}_${chain}.ca.pdb
    fi
    # insert chain ID and add to system
    cat chains.pulchra/${name}_${chain}.ca.rebuilt.pdb \
    | sed "s/^\(.\{21\}\) /\1$chain/g" \
    | grep -E -v "^(END|REMARK)" \
    >> "${aa}"
done

exit

# renumber atoms 
~/contrib/pdb-tools/pdb_residue-renumber.py $aa
# optional minimization
#obminimize -ff GAFF ${aa##*.}_res-renum.pdb > ${name}.pulchra.obm.pdb
