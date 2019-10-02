# submit an MD in vacuo
#
#	(C) José R. Valverde, 2011, EMBnet/CNB, CSIC
#
f=${f:-$1}
dir=`dirname $f`
base=`basename $f .pdb`

if [ ! -e $base/minim/stdout ] ; then 
    qsub -q exe-x86_64 -N `basename $dir -2Ca++1Mg++`-min -o $base.o -e $base.e -l mem=8192Mb -l nodes=1:ppn=1 \
      -v f=$f,p=/home/cnb/jrvalverde/work/britt/simul/bin/minimize.sh,o=$dir/minim ~/work/britt/simul/bin/batch.sh 
fi

