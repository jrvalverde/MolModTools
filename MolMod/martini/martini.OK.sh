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

if ! which conda >& /dev/null ; then
    activate miniconda3		# for martinize2
fi

mkdir -p mtnz2
if [ ! -s mtnz2/${nam}_mtn.top -o ! -s mtnz2/${nam}_mtn.pdb ] ; then
    # -p 			output position restraints (none/all/backbone)
	# -elastic		write elastic bonds (default false)
	# -resid mol	renumber residues per molecule (default)
	# -resid input	keep input numbers 
	# -ignh			ignore all H atoms
	# -dssp			predict ss (if no executable given mdtraj will be used)
    martinize2 -f $pdb \
    	-o ${nam}_mtn.top \
		-x ${nam}_mtn.pdb \
		-name molecule \
		-p backbone \
		-ff martini3001 \
		-resid input \
		-elastic \
		|& tee mtnz2/martinize2.log

    echo "Moving required files to mtnz2"
	mv ${nam}_mtn* mtnz2
	mv molecule* mtnz2
fi

cp -uva ./template/martini* mtnz2
cp -uva ./template/*mdp mtnz2

#cd mtnz2
#for i in martini_v*.itp ; do ln $i ${i/_v?.?.?/} ; done 
#cd ..

# make a wortking copy
echo "Creating a working copy"
cp -r mtnz2 simul

# wotk with the working copy
cd simul

# rename model molecules to Complex.*
for i in ${nam}_mtn.* ; do cp $i Complex.${i##*.} ; done


insane="ONESTEP"		# insane can do everything

if [ "$saline" == "yes" ] ; then salt="-salt 0.15" 
elif [ "$counterions" == 'no' ] ; then salt="-salt 0"
elif [ "$neutral" == 'yes' ] ; then salt="-charge auto"
else salt=''
fi

if [ "$INSANE" == "STEPWISE" ] ; then
    # Add PBC
    # Here we lose chain information
	# but can recover it using the Complex.top file
	cp Complex.top Complex_pbc.top
	
    insane -f Complex.pdb \
	   -o Complex_pbc.gro \
	   -center \
	   -pbc cubic \
	   -box 200,200,200 \
	   |& tee Complex_pbc.log

    
	   
	# Solvate and add ions
	# here it seems to lose track of protein charge
	# -d seems to take precedence over -pbc if the dimensions required 
	# are larger
	# save as GRO
	cp Complex_pbc.top Complex_solvions.top
	insane -f Complex_pbc.gro \
	   -o Complex_solvions.gro \
	   -d 10 \
	   ${comment# alt: -d $((maxsz / 2)) } \
	    -sol W -salt 0.15 \
	   |& tee -a Complex_solvions.top
	   
    # save as PDB
    #insane -f Complex_pbc.gro \
	#   -o Complex_solvions.pdb \
	#   -d 10 -sol W -salt 0.15 \
	#   |& tee -a Complex_solvions.log
	#   ${comment# alt: -d $((maxsz / 2)) } 

elif [ "$INSANE" = "ONSESTEP" ] ; then

    # try to do all at once, 
    # to add a POPC membrane --a better alternative might be
    #       -l DPPC:4 -l DIPC:3 -l CHOL:3
    #	which contains a fully saturated lipid (DPPC),
    #	a polyunsaturated lipid (dilinoleyl-phosphatidylcholine, DPIC)
    #   and cholesterol (CHOL) use
	# -l POPC
    #
    # to use Martini Polarizable Water use PW instead of W (unavail in M3)
    #
	# If PBC are specified, box dimensions will be in nm (e.g., 200nm=2000 Å)
	# UNITS ARE IN NM !!!
	cp Complex.top Complex_solvions.top
	insane -f Complex.pdb \
		   -o Complex_solvions.gro \
	   -center \
	   -d 10 \
	   ${comment# -pbc cubic } \
	   ${comment# -box 200,200,200} \
	   -sol W \
	   -salt 0.15 \
	   |& tee -a Complex_solvions.top
	   
	   
elif [ "$INSANE" = "KO" ] ; then
    # set PBC using GROMACS
	# by saving as PDB we preserve chain information
	# plus, it is already stored in Complex.top
	# dimensions are in nm, not in A !!!
    gmx editconf -bt triclinic -box 200 200 200 \
		 -center 100 100 100 \
		 -f Complex.pdb -o Complex_PBC.pdb \
		 |& tee Complex_PBC.log

	# do the solvation and add the ions with insane
	if [ "$solvate" == "yes" ] ; then

    	## solvate		# we'll do it with insane
    	#gmx solvate -cp Complex_PBC.pdb -cs water.gro \
    	#            -radius 0.21 -o Complex_b4ion.pdb \
    	#	    |& tee log.solvgmx.log

    	# solvate and add ions in one step
    	#	will save the text to merge with the system top file
		#    insane -f Complex_PBC.pdb -p SolvIons.top -o SolvIons.gro \
		#	   -d 10 -sol W -salt 0.15 \
		#	   |& tee log.solvins.log

    	#	if we do not use the -p option, insane outputs the content to
    	#	append at the end of an existing top file
    	#
    	#   note that the top file still needs to include the parameters
    	#   for the martini force field, which we will insert afterwards
    	cp Complex.top Complex_solvions.top 
    	insane -f Complex_PBC.pdb -o Complex_solvions.gro \
		   -d 10 -sol W -salt 0.15 \
		   |& tee -a Complex_solvions.top

    	# repeat to also have a PDB file
    	insane -f Complex_PBC.pdb -o Complex_solvions.pdb \
		   -d 10 -sol W -salt 0.15 \
		   |& tee -a Complex_solvions.log
	fi

fi



# fix TOPology file
mv Complex_solvions.top Complex_solvions.ko
grep "^#" Complex_solvions.ko             >  Complex_solvions.top
echo '#include "martini_solvents_v1.itp"' >> Complex_solvions.top
echo '#include "martini_ions_v1.itp"'     >> Complex_solvions.top
grep -v "^#" Complex_solvions.ko          >> Complex_solvions.top

# fix ion names
cat Complex_solvions.gro | sed -e 's/CL-/CL /g' -e 's/NA+/NA /g' > Complex_b4em.gro
cat Complex_solvions.top | sed -e 's/CL-/CL /g' -e 's/NA+/NA /g' > Complex_b4em.top

#cp Complex_b4em.top Complex.top		# this unsyncs Complex.top from Complex.pdb!!!
# just in case
cp Complex_b4em.top topol.top

# use provided minimization
cp Complex_b4em.top minimization.top	# the topology is the same

gmx grompp -f minimization.mdp -c Complex_b4em.gro \
	   -p Complex_b4em.top -r Complex_b4em.gro \
	   -o minimization.tpr \
	   |& tee log.stderr

# now that we have a TPR file, we can recreate the stating PDB file
echo 0 | gmx trjconv -f Complex_b4em.gro -s minimization.tpr -o Complex_b4em.pdb


gmx mdrun -ntmpi 1 -v -deffnm minimization -pin on  \
          |& tee -a log.stderr

cp minimization.gro minimization1.gro

gmx grompp -f minimization.mdp -c minimization1.gro \
	   -p minimization.top -r minimization1.gro \
	   -o minimization2.tpr \
	   |& tee log.stderr

gmx mdrun -ntmpi 1 -v -deffnm minimization2 -pin on  \
          |& tee -a log.stderr

gmx grompp -f minimization.mdp -c minimization2.gro \
	   -p minimization.top -r minimization2.gro \
	   -o minimization3.tpr \
	   |& tee log.stderr

gmx mdrun -ntmpi 1 -v -deffnm minimization3 -pin on  \
          |& tee -a log.stderr

# change nsteps from 1000 to 5000
gmx grompp -f minimization.mdp -c minimization3.gro \
	   -p minimization.top -r minimization3.gro \
	   -o minimization4.tpr \
	   |& tee log.stderr

gmx mdrun -ntmpi 1 -v -deffnm minimization4 -pin on  \
          |& tee -a log.stderr

# use provided equilibration
cp minimization.top equilibration.top

gmx grompp -f equilibration.mdp -c minimization4.gro \
           -p minimization.top  -r minimization4.gro \
	   -o equilibration.tpr \
           |& tee -a log.stderr

gmx mdrun -ntmpi 1 -v -deffnm equilibration -pin on  \
          |& tee -a log.stderr

# use provided MD 
cp equilibration.top dynamic.top

# this time we do not use any restraints (no -r)
gmx grompp -f dynamic.mdp  -c equilibration.gro \
           -p equilibration.top\
	   -o dynamic.tpr \
	   -maxwarn 1 \
           |& tee -a log.stderr

gmx mdrun -ntmpi 1 -v -deffnm dynamic -pin on  \
          |& tee -a log.stderr


cd ..


exit
