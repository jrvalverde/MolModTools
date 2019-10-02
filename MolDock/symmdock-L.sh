etc=~/work/britt/KChIP3/simul/etc
#export GRAMMDAT=/opt/structure/gramm
export SYMMDOCK=$HOME/SymmDock
export PATH=~/bin:$PATH

f=${f:-$1}
dir=`dirname $f`
base=`basename $f .pdb`

# Change to working directory and set up link to model data file
cd $dir

if [ -e SymmDock-L/DONE ] ; then exit ; fi

if [ ! -d SymmDock-L ] ; then mkdir SymmDock-L ; fi
cd SymmDock-L

ln -s ../$base.pdb model.pdb

cp model.pdb modelA.pdb

perl $SYMMDOCK/namechain.pl modelA.pdb A

perl $SYMMDOCK/buildParams.pl 2 modelA.pdb

perl $SYMMDOCK/runMSPoints.pl modelA.pdb

cat > site.txt<<END
155 A
159 A
251 A
END

sed params.txt -e 's/#activeSiteParams site.txt 1 0.1/activeSiteParams site.txt 2 0.1/g' > kk
mv kk params.txt

$SYMMDOCK/symm_dock.Linux params.txt output.txt

perl $SYMMDOCK/transOutput.pl output.txt 1 10

touch DONE
