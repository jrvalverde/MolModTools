#!/bin/bash
# NAME
#   	score_dsx	-- Score using DSX
#
# SYNOPSIS
#	score_dsx receptor.{mol2|pdb] ligands.mol2 [known.mol2]
#
#	Score the ligands against the receptor and, optionally, compare
#	with a known ligand.
#
# AUTHOR
#	All the code is
#		(C) Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es 2014
#	and
#		Licensed under (at your option) either GNU/GPL or EUPL
#
# LICENSE:
#
#	Copyright 2014 JOSE R VALVERDE, CNB/CSIC.
#
#	EUPL
#
#       Licensed under the EUPL, Version 1.1 or \u2013 as soon they
#       will be approved by the European Commission - subsequent
#       versions of the EUPL (the "Licence");
#       You may not use this work except in compliance with the
#       Licence.
#       You may obtain a copy of the Licence at:
#
#       http://ec.europa.eu/idabc/eupl
#
#       Unless required by applicable law or agreed to in
#       writing, software distributed under the Licence is
#       distributed on an "AS IS" basis,
#       WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
#       express or implied.
#       See the Licence for the specific language governing
#       permissions and limitations under the Licence.
#
#	GNU/GPL
#
#       This program is free software: you can redistribute it and/or modify
#       it under the terms of the GNU General Public License as published by
#       the Free Software Foundation, either version 3 of the License, or
#       (at your option) any later version.
#       
#       This program is distributed in the hope that it will be useful,
#       but WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#       GNU General Public License for more details.
#       
#       You should have received a copy of the GNU General Public License
#       along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


# recursively get the last file pointed to by a symlink
function lastlink() { [ ! -h "$1" ] && echo "$1" || (local link="$(expr "$(command ls -ld -- "$1")" : '.*-> \(.*\)$')"; cd $(dirname $1); lastlink "$link" | sed "s|^\([^/].*\)\$|$(dirname $1)/\1|"); }

# get the real absolute path name of a file
function realfile () {
   if [ -x `which readlink` ] ; then
        readlink -e $1
    elif [ -x `which realpath` ] ; then
        realpath $1
    else
        local target=`lastlink $1`
        local tname=${target##*/}
        local tdir=`dirname $target`
        echo $(cd $tdir ; pwd -P)"/$tname"
    fi
}

function score_dsx {
    local receptor=$1
    local ligands=$2
    local known=$3
    
    # use these on Xistral
    #export ARCH=linux64
    #export EXE_ARCH=linux_64
    #export PATH=/u/jr/contrib/dsx/$ARCH:$PATH
    #export LD_LIBRARY_PATH=/u/jr/contrib/dsx/$ARCH:$LD_LIBRARY_PATH

    # use these on NGS
    #export ARCH=RHEL_linux32
    #export EXE_ARCH=rhel_linux_32
    #export PATH=/u/jr/contrib/dsx/$ARCH:$PATH
    #export LD_LIBRARY_PATH=/u/jr/contrib/dsx/$ARCH:$LD_LIBRARY_PATH

    # use these on ILLUMINA
    local ARCH=linux64
    local EXE_ARCH=linux_64
    local PATH=/home/scientific/contrib/dsx/$ARCH:$PATH
    local LD_LIBRARY_PATH=/home/scientific/contrib/dsx/$ARCH:$LD_LIBRARY_PATH


    local DSX=~/contrib/dsx/$ARCH/dsx_$EXE_ARCH.lnx
    local DSX_POT=~/contrib/dsx/pdb_pot_0511/

    # Process arguments
    #echo "DSX: $receptor $ligands $known"

    local rext="${receptor##*.}"
    local rnam="${receptor%.*}"
    local rok=`realfile $receptor`
    if [ "$rext" != "pdb" -a "$rext" != "mol2" ] ; then
    	echo "ERROR: Receptor $receptor must be a PDB or MOL2 file"
        return 1
    fi
    
    local lext="${ligands##*.}"
    local lnam="${ligands%.*}"
    local lok=`realfile $ligands`
    if [ "$lext" != "mol2" ] ; then
    	echo "ERROR: Ligands $ligands must be in a MOL2 file"
        return 1
    fi

    if [ "$known" = "" -o ! -s "$known" ] ; then
        known=$ligands
    fi
    local kext="${known##*.}"
    local knam="${known%.*}"
    local kok=`realfile $known`
    if [ "$kext" != "mol2" ] ; then
    	echo "ERROR: Reference $known must be in a MOL2 file"
        return 1
    fi

    # Make working directory
    if [ ! -d dsx ] ; then mkdir dsx ; fi
    cd dsx

    if [ ! -s $rok ] ; then echo "ERROR: $receptor ($rok)" ; return 1 ; fi
    if [ ! -e $rnam.mol2 ] ; then 
        if [ "$rext" = "pdb" ] ; then
	    babel -ipdb $rok -omol2 $rnam.mol2
        else
            ln -s $rok $rnam.mol2
        fi
    fi

    if [ ! -s $lok ] ; then echo "ERROR: $ligands ($lok)" ; return 1 ; fi
    if [ ! -s $lnam.mol2 ] ; then 
	ln -s $lok $lnam.mol2
    fi

    if [ ! -s $kok ] ; then echo "ERROR: $known ($kok)" ; return 1 ; fi
    if [ ! -s $knam.mol2 ] ; then 
	ln -s $kok $knam.mol2
    fi

    #
    # set a timeout of 5' (300") on each
    #	$! expands to the PID of the most recently executed background command:
    #	( some-large-command ) & sleep 300 ; kill $!
    # the above has a problem in that if the command ends earlier we still wait
    # the specified timeout.
    #
    # The solution by J. R. Valverde in EMBnet.news is better, but we may also
    # use now the 'timeout' command from coreutils, which returns 124 if the
    # command was terminated by a timeout
    #
    #	Note: for the EMBnet.News solution try the following
    #
    # (submitter=$$ ; (sleep $tmo ; pkill -P $submitter)& large-command)& wait $!
    # or 
    # (submitter=$! ; (sleep $tmo ; kill $submitter)& large-command)& wait $!
    #
    # maximum running time of DSX (m = min., s = sec.)
    local tmo=10m
    # grace timeout (15 sec.)
    local gto=15s
    #
    # this is to deal with inconsistencies in the timeout command
    # across different versions of Ubuntu
    #
    #	This works in Ubuntu 11.10 (e.g. xistral)
    #
    #toopt="$tmo -k $gto"
    #
    #	This works in Ubuntu 10.04.2 [seconds] (e. g. ngs)
    #
    local toopt="300"


    echo -n "Scoring poses with DSX .."

    if [ ! -e DSX_${rnam}_${lnam}.tot.txt ] ; then
        timeout $toopt $DSX \
	    -P $rnam.mol2 -L $lnam.mol2 -R $knam.mol2 \
	    -I 0 -D $DSX_POT -o -v -S 1 \
	    -T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
        if [ $? -ne 124 ] ; then
            mv DSX_${rnam}_${lnam}.txt DSX_${rnam}_${lnam}.tot.txt
        fi
    fi

    if [ ! -e DSX_${rnam}_${lnam}.rmsd.txt ] ; then
        timeout $toopt $DSX \
	    -P ${rnam}.mol2 -L ${lnam}.mol2 -R ${knam}.mol2 \
	    -I 0 -D $DSX_POT -o -v -S 4 \
	    -T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
        if [ $? -ne 124 ] ; then
    	    mv DSX_${rnam}_${lnam}.txt DSX_${rnam}_${lnam}.rmsd.txt
        fi
    fi

    if [ ! -e DSX_${rnam}_${lnam}.txt ] ; then
        timeout $toopt $DSX \
	    -P ${rnam}.mol2 -L ${lnam}.mol2 -R ${knam}.mol2 \
	    -I 0 -D $DSX_POT -o -v -S 0 \
	    -T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
    fi

    echo " done"
    ####
    echo -n "Scoring reference with DSX .."

    if [ ! -e DSX_${rnam}_${knam}.tot.txt ] ; then
        timeout $toopt $DSX \
	    -P ${rnam}.mol2 -L ${knam}.mol2 -R ${knam}.mol2 \
	    -I 0 -D $DSX_POT -o -v -S 1 \
	    -T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
        if [ $? -ne 124 ] ; then
            mv DSX_${rnam}_${knam}.txt DSX_${rnam}_${knam}.tot.txt
        fi
    fi

    if [ ! -e DSX_${rnam}_${knam}.rmsd.txt ] ; then
        timeout $toopt $DSX \
	    -P ${rnam}.mol2 -L ${knam}.mol2 -R ${knam}.mol2 \
	    -I 0 -D $DSX_POT -o -v -S 4 \
	    -T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
        if [ $? -ne 124 ] ; then
    	    mv DSX_${rnam}_${knam}.txt DSX_${rnam}_${knam}.rmsd.txt
        fi
    fi


    if [ ! -e DSX_${rnam}_${knam}.txt ] ; then
        timeout $toopt $DSX \
	    -P ${rnam}.mol2 -L ${knam}.mol2 -R ${knam}.mol2 \
	    -I 0 -D $DSX_POT -o -v -S 0 \
	    -T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
    fi

    echo "done"

    cd ..
}

score_dsx $*
