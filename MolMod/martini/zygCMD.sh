pdb=$1
out=${2:-${pdb}_mtn}

if [ ! -e $pdb ] ; then
    echo "$1 not found" 
    return $ERROR
fi

dir=`dirname $pdb`
fin="${pdb##*/}"
ext="${fin##*.}"
nam="${fin%.*}"
if [ ! "$ext" == "pdb" -a ! "$ext" == "brk" ] ; then
    echo "Input file must be a PDB file."
    return $ERROR
fi

activate miniconda3		# for martinize2

if [ ! -s ${nam}_mtn.top -o ! -s ${nam}_mtn.pdb ] ; then
    martinize2 -f $pdb \
    	-o ${nam}_mtn.top \
	-x ${nam}_mtn.pdb \
	-p backbone \
	-ff martini3001
fi

mkdir -p mtnz2
mv ${nam}_mtn* mtnz2
mv molecule* mtnz2
cp -uva doc/tutorial_2/template/martini* mtnz2
cp -uva doc/tutorial_2/template/*mdp mtnz2

cd mtnz2
for i in martini_v*.itp ; do ln $i ${i/_v?.?.?/} ; done 
cd ..

cp -r mtnz2 simul
cd simul

cp ${nam}_mtn.pdb Complex.pdb
cp ${nam}_mtn.top Complex.top

if [ "$control" == "KO" ] ; then
insane -f Complex.pdb \
       -o Complex_pbc.pdb \
       -pbc cubic \
       -box 200,200,200 \
       |& tee Complex_pbc.log
# here it seems to lose track of protein charge
cp Complex.top Complex_solvions.top
insane -f Complex_pbc.pdb \
       -o Complex_solvions.gro \
       -d 7 -sol W -salt 0.15 \
       |& tee -a Complex_solvions.top
       ${comment# alt: -d $((maxsz / 2)) } \
      
insane -f Complex_pbc.pdb \
       -o Complex_solvions.pdb \
       ${comment# alt: -d $((maxsz / 2)) } \
       -d 7 -sol W -salt 0.15 \
       |& tee -a Complex_solvions.top

fi

insane="KO"		# insane can do everything, but loses protein charge
			# after defining the PBC simulation box
if [ "$insane" != "OK" ] ; then
    # set PBC
    gmx editconf -bt triclinic -box 200 200 200 \
		 -center 100 100 100 \
		 -f Complex.pdb -o Complex_PBC.pdb \
		 |& tee log.pbc.log


fi


if [ "$saline" == "yes" ] ; then salt="-salt 0.15" 
elif [ "$counterions" == 'no' ] ; then salt="-salt 0"
elif [ "$neutral" == 'yes' ] ; then salt="-charge auto"
else salt=''
fi

# do the solvation and add the ions with insane
if [ "$solvate" == "yes" ] ; then

    ## solvate		# we'll do it with insane
    #gmx solvate -cp Complex_PBC.pdb -cs water.gro \
    #            -radius 0.21 -o Complex_b4ion.pdb \
    #	    |& tee log.solvgmx.log

    # solvate and add ions in one step
    #	will save the text to merge with the system top file
#    insane -f Complex_PBC.pdb -p SolvIons.top -o SolvIons.gro \
#	   -d 7 -sol W -salt 0.15 \
#	   |& tee log.solvins.log

    #	if we do not use the -p option, insane outputs the content to
    #	append at the end of an existing top file
    #
    #   note that the top file still needs to include the parameters
    #   for the martini force field, which we will insert afterwards
    cp Complex.top Complex_solvins.top 
    insane -f Complex_PBC.pdb -o Complex_solvins.gro \
	   -d 7 -sol W -salt 0.15 \
	   |& tee -a Complex_solvins.top

    # repeat to also have a PDB file
    insane -f Complex_PBC.pdb -o Complex_solvins.pdb \
	   -d 7 -sol W -salt 0.15
fi

# fix ion names
cat Complex_solvins.pdb | sed -e 's/CL-/CL /g' -e 's/NA+/NA /g' > Complex_b4em.pdb

# fix TOPology file
grep "^#" Complex_solvins.top             >  Complex_b4em.tp
echo '#include "martini_solvents_v1.itp"' >> Complex_b4em.tp
echo '#include "martini_ions_v1.itp"'     >> Complex_b4em.tp
grep -v "^#" Complex_solvins.top          >>  Complex_b4em.tp
cat Complex_b4em.tp | sed -e 's/CL-/CL /g' -e 's/NA+/NA /g' > Complex_b4em.top

#cp Complex_b4em.top Complex.top		# this unsyncs Complex.top from Complex.pdb!!!
# just in case
cp Complex_b4em.top topol.top

# use provided minimization
gmx grompp -f minimization.mdp -c Complex_b4em.pdb \
	   -p Complex_b4em.top -r Complex_b4em.pdb \
	   -o minimization.tpr \
	   |& tee log.stderr

gmx mdrun -nt 16 -v -deffnm minimization -pin on -pinoffset 48 \
          |& tee -a log.stderr

cp Complex_b4em.top minimization.top	# the topology is the same

# use provided equilibration
gmx grompp -f equilibration.mdp -c minimization.gro \
           -p minimization.top  -r minimization.gro \
	   -o equilibration.tpr \
           |& tee -a log.stderr

gmx mdrun -nt 16 -v -deffnm equilibration -pin on -pinoffset 48 \
          |& tee -a log.stderr

cp minimization.top equilibration.top

# use provided MD 
# this time we do not use any restraints (no -r)
gmx grompp -f dynamic.mdp  -c equilibration.gro \
           -p equilibration.top\
	   -o dynamic.tpr \
	   -maxwarn 1 \
           |& tee -a log.stderr

gmx mdrun -nt 16 -v -deffnm dynamic -pin on -pinoffset 48 \
          |& tee -a log.stderr

cp equilibration.top dynamic.top

cd ..


exit

# DEPRECATED CODE
# Previously we tried with NAMD, but while it did succeed generating
# a CG conformation (.pdb + .rcg) it failed building the PSF file
# because of the number of elements and bonds

# we also tried to do it with martinize2 + vermouth, but it failed for 
# the whole system, so we attempted to generate the CG system chain 
# by chain:

# Chain by chain

./split_pdb_in_chains.sh $pdb

for i in A B C D E F G H I J K L ; do 
    #./pdb_atom_renumber.bash ${nam}_$i.$ext ${nam}_${i}_rn.$ext 
    echo ;
done
for i in A B C D E F G H I J K L ; do 
    #dssp ${nam}_${i}_rn.$ext ${nam}_${i}_rn.dssp
    echo ;
done

#dssp=/home/jr/miniconda3/envs/bin/mkdssp
dssp=dssp
dssp=mkdssp
dssp=/mnt/jr/bck/work/jr.veda.bck/contrib/ccp4/ccp4-6.4.0/bin/mkdssp
#dssp=`pwd`/mkss
martinize=martinize2

for i in A B C D E F G H I J K L ; do 
	echo MARTINIZING CHAIN $i ; 
    #echo 
	$martinize -f ${nam}_${i}_rn.$ext \
			   -o ${nam}_${i}_cg.top \
			   -ff martini3001 \
		           -dssp \
			   -x ${nam}_${i}_cg.$ext \
			   -scfix \
			   -cys auto \
			   -p backbone \
			   -elastic \
			   -ef 700.0 \
			   -el 0.5 \
			   -eu 0.9 ; 
done

# at this point, vermouth failed to generate the appropriate topologies
