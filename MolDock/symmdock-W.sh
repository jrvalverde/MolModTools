etc=~/work/britt/KChIP3/simul/etc
#export GRAMMDAT=/opt/structure/gramm
export SYMMDOCK=$HOME/SymmDock
export PATH=~/bin:$PATH

f=${f:-$1}
dir=`dirname $f`
base=`basename $f .pdb`

# Change to working directory and set up link to model data file
cd $dir

if [ -e SymmDock-W/DONE ] ; then exit ; fi

if [ ! -d SymmDock-W ] ; then mkdir SymmDock-W ; fi
cd SymmDock-W

ln -s ../$base.pdb model.pdb

cp model.pdb modelA.pdb

perl $SYMMDOCK/namechain.pl modelA.pdb A

perl $SYMMDOCK/buildParams.pl 2 modelA.pdb

perl $SYMMDOCK/runMSPoints.pl modelA.pdb

cat > site.txt<<END
50  A
203 A
END

sed params.txt -e 's/#activeSiteParams site.txt 1 0.1/activeSiteParams site.txt 2 0.1/g' > kk
mv kk params.txt

$SYMMDOCK/symm_dock.Linux params.txt output.txt

perl $SYMMDOCK/transOutput.pl output.txt 1 10

touch DONE
