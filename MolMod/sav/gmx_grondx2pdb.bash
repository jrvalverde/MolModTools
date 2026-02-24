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

function grondx2pdb {
    # arguments:
    #	$1 the gromacs format structure file where all chains have been
    #		merged as a series of irregularly connected atoms
    #	$2 an index file that matches atoms to chains
    #
    #	The output will be a file with the same name as the .gro file
    #	but ending in .pdb instead.
    #
    local gro=$1
    local index=$2

    local pdb=`basename $gro .gro`.pdb
    local chid=''

    echo ""
    echo "grondx2pdb: Making a PDB file from $gro using index in $index"
    echo ""

    # for each indexed chain, extract it and add its chain ID
    echo "MODEL" >> $pdb
    for i in `grep "\[ ch" $index | cut -f 2 -d' '` ; do 
        echo "Extracting $i"
        echo $i | \
            trjconv -pbc nojump -f $gro -n $index -o $i.pdb -s $gro
        chid=`echo $i | cut -b3-`
        echo "Adding $chid"
        # change ".(21) " by ".(21)X"
        egrep "(^ATOM)|(^HETATM)|(^TER)" $i.pdb |\
            sed "/\(^ATOM\|^HETATM\)/ s/^\(.\{21\}\) /\1$chid/g" >> $pdb
        rm $i.pdb
    done 
    echo "ENDMDL" >> $pdb


    # A problem still remains with long atom names which start with a
    # number instead of the atom ID (usually H).

    # match first 12 chars, a number and three chars (only in ATOM lines)
    # substitute to place the number at the end
    cat $pdb | sed -e '/^ATOM/ s/^\(.\{12\}\)\([0-9]\)\(...\)/\1\3\2/g' > $pdb.ok
    mv $pdb.ok $pdb
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    gro=$1
    index=$2
    if [ "$UNIT_TEST" == "no" ] ; then
	grondx2pdb $gro $index
    else
	grondx2pdb
    fi
fi
