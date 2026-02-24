#!/bin/bash
#
#	Utility functions
#
#
# AUTHOR(S)
#	'banner' is based on the bash/ksh93 version of 'banner' by jlliagre
#		Apr 15 '12 at 11:52
#		http://stackoverflow.com/questions/652517/whats-the-deal-with-the-banner-command
#
#	All other code is
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

#
# Includes
#
# Checks for definition $COMMON, if not set sources bash script with all common variables
#
if [ -z ${COMMON+x} ]; then 
    . gromacs_funcs_common.bash
fi

#
# NAME
#	make_topolbuild_topology
#
# SYNOPSIS
#	make_topolbuild_topology ligand.pdb charge
#
# DESCRIPTION
#	Create an OPLS-AA topology for a ligand PDB file using topolbuild.
#	Results will be saved in directory $ligand.topolbuild
#
#	Planned for the future:
#
#	topolbuild can also generate gromacs gmx43a1 gmx43a2 gmx43b1
#	gmx45a3 gmx45a5 gmx53a5 and gmx53a6 topologies.
#
#	In addition it can also generate Tripos topologies, and Amber
#	gaff glycam04 glycam 04EP glycam_06c amber91 amber91X amber94
#	amber96 amber98 amber99 amber99EP amberAM1 and amberPM3 topologies 
#	which we could try to convert to gromacs format.
#
# AUTHOR
#	Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es, 2014
#
function make_topolbuild_topology {
    local mol=$1
    
    local dir=`dirname $mol`
    local fin="${mol##*/}"
    local ext="${fin##*.}"
    local lig="${fin%.*}"
    
    local topolbuild=${topolbuild:-"$HOME/contrib/topolbuild1_3/bin/topolbuild"}
    local topolbuilddat=${topolbuilddat:-"$HOME/contrib/topolbuild1_3/dat"}

    if [ ! "$ext" = "mol2" ] ; then
        echo "make_topolbuild_topology ERROR: Input file must be a MOL2 file."
        return $ERROR
    fi
    if [ ! -e $mol ] ; then
    	echo "make_topolbuild_topology ERROR: $mol does not exist"
	return $ERROR
    fi

    if [ -s $lig.mol2 -a ! -e $lig.topolbuild/$lig.itp ] ; then
        echo "make_topolbuild_topology: making OPLSA topology with $topolbuild"
        mkdir -p $lig.topolbuild
        cp $lig.mol2 $lig.topolbuild
        cd $lig.topolbuild
	$topolbuild -n $lig -ff oplsaa -dir $topolbuilddat/gromacs
        # generates
        #	$lig.log
        #	$lig.gro
        #	$ligMOL.mol2
        #	$lig.top
        #	posre$lif.itp
        #	ff$lig.itp
        sed -e "/; Include water topology/,//d" $lig.top > $lig.itp
        cp ff$lig.itp ff$lig.itp.orig
        sed -e "s%ffoplsaanb%oplsaa.ff/ffnonbonded%" ff$lig.itp.orig | \
            sed '/[ defaults ]/,/^ $/d' > ff$lig.itp
        # update $lig.pdb file
        #	we need to because atom numbering and order have changed in .gro .top
        editconf -f $lig.gro -o $lig.pdb
        cd -
    else
        echo ""
        echo "make_topolbuild_topology: using existing topology for $lig"
        echo ""
    fi
    return $OK
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    mol=$1
    if [ "$UNIT_TEST" == "no" ] ; then
	make_topobuild_topology $mol
    else
	make_topobuild_topology testfile.pdb
    fi
fi

