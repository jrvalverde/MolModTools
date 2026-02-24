export PATH=~/contrib/openbabel/bin:$PATH

ff=$1
name=`basename $2 .pdb`
dir=obm_${ff}_all_H

if [ ! -d $dir ] ; then
    if [ -e $dir ] ; then
        echo "Error: $dir exists and is not a directory"
        exit 1
    fi
    mkdir $dir
fi

# NOTE: obminimize outputs a PDB file without H
#	obminimize fails to work correctly if the input lacks H
#	therefore obminimize must add H before minimization.

echo "obminimize -ff $1 -sd -c 1e-5 -n 5000 -h \\
       -newton -cut -rvdw 6.0 -rele 10.0 -pf 10 $2 \\ 
       2>&1 > $dir/${name}_${ff}.pdb | \\
        tee $dir/${name}_${ff}.log

" > $dir/${name}_${ff}.log

obminimize -ff $1 -sd -c 1e-5 -n 5000 -h \
	-newton -cut -rvdw 6.0 -rele 10.0 -pf 10 $2 \
	2>&1 > $dir/${name}_${ff}.pdb | \
        tee -a $dir/${name}_${ff}.log

#xit
# make a plot of the energy progression during minimization
cd $dir
  csplit ${name}_${ff}.log "/^------/"
  if [ ! -e xx01 ] ; then rm xx00 ; exit ; fi
  # skip first two values as they will usually be VERY VERY VERY LARGE
  tail -n +4 xx01 | head -n -1 > xxtable
  ctioga2 --title "Energy" --ylabel "Energy" --xlabel "Step" xxtable
  rm xx00 xx01 xxtable
  mv Plot.pdf ${name}_${ff}.pdf
cd ..
