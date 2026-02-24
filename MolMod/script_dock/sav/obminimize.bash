#!/bin/bash
#
export BABEL_PATH=~/contrib/openbabel/bin
export CHIMERA_PATH=~/contrib/chimera/bin
export PROFIT_PATH=~/contrib/bioinf.org.ac.uk/ProFitV3.1/

export PATH=$BABEL_PATH:$PATH
export PATH=$CHIMERA_PATH:$PATH
export PATH=$PROFIT_PATH:$PATH

export babel=`which babel`
export chimera=`which chimera`
export ctioga2=`which ctioga2`
export ProFit=`which ProFit`



# options can be listed with babel -L forcefields
ff=$1
pdb=$2
name=`basename $pdb .pdb`
addHmethod="none"	# chimera, babel or none
energyplot='Y'		# plot energy change during minimization
profitrms='Y'		# compute RMS of minimized structure

if [ "$addHmethod" == "babel" ] ; then
    dir=obm_${name}_${ff}_pH_7.3
else 
  if [ "$addHmethod" == "chimera" ] ; then
    dir=obm_${name}_${ff}_pH_7.4c
  else
    dir=obm_${name}_${ff}
  fi
fi

# check it is a valid force field
babel -L forcefields | grep -q -i "^$ff "
if [ $? -ne 0 ] ; then
	echo "Selected force field [$ff] is not valid"
        exit
fi

if [ ! -d $dir ] ; then
    if [ -e $dir ] ; then
        echo "Error: $dir exists and is not a directory"
        exit 1
    fi
    mkdir $dir
fi

# NOTE: obminimize outputs a PDB file without H
#	obminimize fails to work correctly if the input lacks H
#	therefore we must add appropiate H before minimization.

# options can be listed with babel -L charges
#charges="gasteiger"
charges="qtpie"
if [ "$ff" == "mmff94" -o "$ff" == "mmff94s" ] ; then charges="mmff94" ; fi 

# no need, tee will overwrite it and we may need for partial re-runs
#rm -f $dir/${name}.H_${ff}.log

#	Babel does not know about nucleic acids and produces wrong
#	protonation of the nucleotide
#	UCSF chimera seems to do the right thing
if [ "$addHmethod" == "babel" ] ; then 
    if [ ! -s $dir/${name}.H_${ff}.pdb ] ; then
        echo "babel -h -p 7.365 --center --partialcharge $charges \\
	    -ipdb $2 -opdb $dir/$name.H.pdb |& \\
            tee -a $dir/${name}.H_${ff}.log \\
    " | tee -a $dir/${name}.H_${ff}.log
    #
        babel -h -p 7.365 --center  \
	    -ipdb $2 -opdb $dir/$name.H.pdb |& \
            tee -a $dir/${name}.H_${ff}.log
    fi
elif [ "$addHmethod" == "chimera" ] ; then
    if [ ! -s $dir/${name}.H_${ff}.pdb ] ; then
      chimera --nogui <<END |& tee -a $dir/${name}.H_${ff}.log 
        open $2
        delete element.H
        addh inisolation true hbond true useHisName true useGluName true useAspName true useLysName true useCysName true
	delete solvent
	write format pdb 0 $dir/$name.H.pdb
        stop now
	#solvate shell TIP3PBOX 5
	# We next need to add ions, but can't easily do it, so we'll leave 
	# automatic solvation out for now.
END
    fi
else
    cp $2 $dir/${name}.H.pdb
fi

if [ ! -s $dir/${name}.H_${ff}.pdb ] ; then
    echo "obminimize -ff ${ff} -sd -c 1e-5 -n 5000 \\
       -newton -cut -rvdw 6.0 -rele 10.0 -pf 10  $dir/$name.H.pdb \\ 
       2>&1 > $dir/${name}.H_${ff}.pdb | \\
        tee -a $dir/${name}.H_${ff}.log

" | tee -a $dir/${name}.H_${ff}.log
#
    obminimize -ff ${ff} -sd -c 1e-5 -n 5000 \
	-newton -cut -rvdw 6.0 -rele 10.0 -pf 10 $dir/$name.H.pdb \
	2>&1 > $dir/${name}.H_${ff}.pdb | \
        tee -a $dir/${name}.H_${ff}.log
fi
if [ -s $dir/${name}.H_${ff}.log ] ; then
  #if ! ls $dir/E=*  1> /dev/null 2>&1 ; then
  if ! stat --printf='' $dir/E=*  1> /dev/null 2>&1 ; then
  # for directories with too many files or files with spaces, this may be better
  # if test -n "$(find $dir -maxdepth 1 -name 'E=*' -print -quit)" ; then
    # Save last energy for quick inspection
    lastE=`tail -2 $dir/${name}.H_${ff}.log | head -1 | tr -s ' ' '\t'` 
    echo "$lastE" > $dir/E=`echo "$lastE" | cut -f3`
  fi
fi

#exit
if [ $energyplot=='Y' ] ; then
  if [ ! -s $dir/${name}.H_${ff}.pdf ] ; then
  # make a plot of the energy progression during minimization
    if [ -s $dir/${name}.H_${ff}.log ] ; then
      echo "Plotting energy change"
      cd $dir
	csplit ${name}.H_${ff}.log "/^----------/" > /dev/null 2>&1
	if [ -e xx01 ] ; then
	  # skip first two values as they will often be VERY VERY VERY LARGE
	  tail -n +4 xx01 | head -n -1 > energy
	  #ls -l
	  ctioga2 --title "Energy" --ylabel "Energy" --xlabel "Step" energy
	  #rm xx00 xx01 xxtable
          rm xx*
          # in case ctioga2 is not available or fails (e.g. on a non-X system)
	  if [ -s Plot-000.pdf ] ; then
            mv Plot-000.pdf ${name}.H_${ff}.pdf
	  fi
        else
          echo "no energies!"
          #ls -l
        fi
      cd ..
    else
      echo "No log file $dir/${name}.H_${ff}.log"
    fi
  fi
fi
if [ $profitrms=='Y' ] ; then
  if [ ! -s $dir/${name}.H_${ff}.aa.pdf ] ; then
    # compare 
    echo "Computing RMSD"
    r=$name.pdb
    m=$dir/${name}.H_${ff}.pdb
    rmsd=$dir/${name}.H_${ff}.rmsd
    aarmsd=$dir/${name}.H_${ff}.aa.rmsd
    if [ ! -s $aarmsd ] ; then
      ~/contrib/bioinf.org.ac.uk/ProFitV3.1/ProFit <<END |& tee $rmsd
        REFERENCE $r
        MOBILE $m
        FIT
        RESIDUE $aarmsd
#       align all chains redgardless of chain ID
#       ALIGN WHOLE
#       align only specific chains
#       ALIGN A:*,A:*
#       ALIGN B:*,B:* APPEND
#       trim the ends of the aligned zones and add gaps allowing for a 
#       like versus like comparison by using fitting zones that are common
#       to all the structures
#       TRIMZONES
#       match only C, Cα, N and O
#       ATOMS N,CA,C,O
#       WRITE fitted.pdb
END
    fi
    cat $aarmsd | cut -c1-7,37-42 | tr -d [A-Z] > $dir/aa.rms
    ctioga2 --plot $dir/aa.rms \
            --title "RMSD by a.a." \
	    --xlabel "a.a." \
	    --ylabel "RMSD (Å)"
    #rm aa.rms
    if [ -s Plot-000.pdf ] ; then
      mv Plot-000.pdf $dir/${name}.H_${ff}.aa.pdf
    fi
  fi
fi
