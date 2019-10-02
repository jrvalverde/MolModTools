# submit an MD in vacuo
#
#	(C) José R. Valverde, 2011, EMBnet/CNB, CSIC
#
f=${f:-$1}
dir=`dirname $f`
base=`basename $f .pdb`

echo $f
echo $dir
echo $base

if [ ! -e $base/solution/solution.log ] ; then 
    qsub -q exe-x86_64 -N MD-H2O -l mem=8192Mb -l nodes=1:ppn=1 \
      -v f=$f,p=/home/cnb/jrvalverde/work/britt/simul/bin/sim-water.sh,o=$dir/solution ~/work/britt/simul/bin/batch.sh 
fi


