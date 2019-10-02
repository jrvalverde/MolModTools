etc=~/work/gneshich/etc
#export DELPHI=/opt/structure/delphi
export DELPHI=$HOME/contrib/delphi
export PATH=~/bin:$PATH

f=${f:-$1}
dir=`dirname $f`
base=`basename $f .pdb`

cd $dir
if [ ! -d delphi ] ; then mkdir delphi ; fi
cd delphi

cat > params <<END
gsize=65
scale=1.0
in(pdb,file="$f")
in(siz,file="parseres.siz")
in(crg,file="parseres.crg")
indi=4.0
exdi=80.0
prbrad=1.4
salt=0.50
bndcon=2
maxc=0.0001
linit=800
!nonit=800
energy(s,c,g)
END
