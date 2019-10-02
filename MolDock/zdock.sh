cmd=~/work/britt/KChIP3/simul/cmd
export ZDOCK=$HOME/zdock

f=${f:-$1}
dir=`dirname $f`
base=`basename $f .pdb`

# Change to working directory and set up link to model data file
cd $dir

if [ -e zdock/DONE ] ; then exit ; fi

if [ ! -d zdock ] ; then mkdir zdock ; fi
cd zdock

ln -s ../$base.pdb model.pdb

ln -s $ZDOCK/uniCHARMM .

$ZDOCK/mark_sur model.pdb model_m.pdb
ln -s model_m.pdb model_l.pdb
$ZDOCK/zdock -R model_m.pdb -L model_l.pdb -o zdock.out
head -14 zdock.out > top10.out

mkdir dimers
cd dimers
ln -s $ZDOCK/create_lig .
ln -s ../model* .
ln -s ../*.out .
perl $ZDOCK/create.pl top10.out
for i in complex* ; do mv $i $i.pdb ; done

# remove extra information (chimera chokes on it, but not rasmol or pymol)
for i in {1..10} ; do
    if [ -e complex.$i.pdb ] ; then
	cat complex.$i.pdb | cut -b 1-63 > complex-`printf %02d $i`.ent
    fi
done

cd ..

touch DONE
