#
# BBQ rebuilds ONLY the backbone from CA atoms
#


export CLASSPATH=./bioshell.bioinformatics-2.2.jar:./bioshell.bioinformatics-2.2-mono.jar:./bioshell.bioinformatics-2.2-tests.jar:./bioshell.bioinformatics-2.2.jar:./bioshell.bioinformatics-2.2-mono.jar
bbq='java apps.BBQ'

scwrl=~/contrib/multicom_toolbox/DeepQA/tools/scwrl4/Scwrl4

if [ "$#" -ne 1 ]; then
    echo -e "\nUsage:\n\t./run_bbq.sh 2gb1.pdb\n"
    exit 1
fi

orig=$1
name=${orig%.*}
ext=${orig##*.}

cg="${name}.cg.pdb"
ca="${name}.ca.pdb"
bb="${name}.ca-bb.pdb"
aa="${name}.aa.bbq+scwrl.pdb"

# check if $name.cg.pdb exists, if not, make it
if [ ! -s "${name}.cg.pdb" ] ; then
    if [ -s "${name}.gro" -a -s "${name}.tpr" ] ; then
	    echo "Converting $orig to $cg"
	    gmx trjconv -f "${name}.gro" -s "${name}.tpr" \
		    -pbc mol -conect -o "${cg}" 
    else
        ln -s "${name}.pdb" "${cg}"
    fi
fi

# WARNING!!! clean any previous run 
#rm ${aa}

if [ ! -s "$ca" ] ; then
    echo "Extracting CA trace ($ca)"
    grep '^ATOM ' "${cg}" | grep ' BB ' | sed -e 's/ BB / CA /g' > $ca
fi


if [ ! -s "$bb" ] ; then
    # process the CA trace to get the full backbone
    echo "Building full backbone ($bB)"
    $bbq -ip="$ca"
    # this will produce $name.ca-bb.pdb, with the backbone and all the chains
fi

# SCWRL REQUIRES a full backbone to rebuild the all-atom structure
# we will use the backbone from BBQ for FASPR

if [ ! -s "$aa" ] ; then
    echo "Reconstructing all atoms ($aa)"
    $scwrl -i "$bb" -o "$aa"
fi
