pdb=$1
name=${pdb%.pdb}
ca=$name.ca.pdb
out=$name.aa.ca2all.pdb

cat $pdb | sed -e 's/ BB / CA /g' > $ca

python ./ca2all.py -i $ca -o $name.aa.pdb -n 1 -v
if [ -e OUT_H.pdb ] ; then mv OUT_H.pdb $out ; fi
