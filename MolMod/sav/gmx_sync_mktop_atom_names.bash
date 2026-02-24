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
#	sync_mktop_atom_names
#
#
# AUTHOR
#	Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es, 2014
#
function sync_mktop_atom_names {
    # synchronize PDB atom names to match the generated OPLS-AA atom names
    #
    #	NOTE: WE ASSUME ATOMS ARE LISTED IN THE SAME ORDER IN BOTH FILES!!!
    #		WE TRUST ORDER AND DO ONLY MINIMAL CHECK FOR ATOM TYPE MATCH
    #
    #	DOING THIS FOR AMBER WOULD BE EXACTLY THE SAME (BUT FOR THE TOPOLOGY
    #	FILE)
    #
    lig=$1
    ff=$2
    pdb=$lig.pdb
    topol=$lig.$ff.top

    while read -r line ; do 
        # if this is an atom line
        if egrep -q '(^HETATM)|(^ATOM  )' <(echo $line) ; then
            >&2 echo "$line" 
            # get atom number
            atno=`echo "$line" | cut -c7-11`
            atty=`echo "$line" | cut -c77-78`
            >&2 echo "atno='$atno'"
            # look it up in the topology and extract its name
            atnam=`grep ".* $atno .* $lig     $atty" $topol | \
                sed -e "s/.* $atno .* $lig     \(....\).*$/\1/"`
            >&2echo "atnam='$atnam'"
            # if new name found substitute old by new name
            if [ "$atnam" != "" ] ; then
                echo "$line" | \
                    sed -e "s/\(^......$atno \)....\(.*$\)/\1$atnam\2/"
            else
    	        # no match
                echo "$line"
                >&2 echo "sync_mktop_atom_names ERROR: No match for $atno $atty in "
                >&2 echo "$line"
            fi
        else
	    # non atom lines are printed 'as is'
            #	this allows us to preserve CONECT records
            echo "$line"
        fi
    done < $pdb
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    lig=$1
    ff=$2
    if [ "$UNIT_TEST" == "no" ] ; then
	sync_mktop_atom_names $lig $ff
    else
	sync_mktop_atom_names testfile.pdb
    fi
fi
