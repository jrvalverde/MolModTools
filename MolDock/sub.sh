# submit an MD in vacuo
#
#	(C) José R. Valverde, 2011, EMBnet/CNB, CSIC
#
f=${f:-$1}
dir=`dirname $f`
base=`basename $f .pdb`

if [ -e stdout ] ; then 
    qsub -q exe-x86_64 -N `basename $i .pdb`-vac -l mem=8192Mb -l nodes=1:ppn=1 \
      -v f=$i,p=/home/cnb/jrvalverde/work/britt/simul/bin/sim-vacuo.sh,o=`dirname $i` ~/work/britt/simul/bin/batch.sh 
fi

