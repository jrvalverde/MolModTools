etc=~/work/britt/KChIP3/simul/etc
export HEX=$HOME/contrib/hex
export HEX_ROOT=/home/jr/contrib/hex
export HEX_VERSION=6d
export PATH=${PATH}:${HEX_ROOT}/bin
export HEX_CACHE=/home/jr/contrib/hex/hex6d_cache
export PATH=~/bin:$PATH

f=${f:-$1}
dir=`dirname $f`
base=`basename $f .pdb`

# Change to working directory and set up link to model data file
cd $dir

if [ -e hex-dock/DONE ] ; then exit ; fi

if [ ! -d hex-dock ] ; then mkdir hex-dock ; fi
cd hex-dock

ln -s ../$base.pdb model.pdb
ln -s $etc/hex.mac

hex < hex.mac > hex.log

touch DONE
