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
# Checks if function has been declared if not sources the bash script containing the function
#
# Array of dependencies 
# DEPENDENCY = (funtion1 funtion2 funtion3 )
# Each funtion must be in a file named gromacs_funcs_funtion1.bash 
#
DEPENDENCY=( strindex )

for name in "${DEPENDENCY[@]}"
  do
     if [ -z "$(type $name &> /dev/null) " ] || [ "$(type -t $name &> /dev/null)" != function ]; then
         if [ -f gromacs_funcs_$name.bash ]; then
	     . gromacs_funcs_$name.bash
         else
    	     echo File gromacs_funcs_$name.bash not found.
         fi
     fi 
done

# NAME
#	pdb_atom_renumber - renumber atoms in a PDB file
#
# SYNOPSIS
#	pdb_atom_renumber infile.pdb outfile
#
# DESCRIPTION
#	Reads an input PDB file (must have a '.pdb' or '.brk' extension)
#	and renumbers all ATOM, ANISOU and TER records starting from 1
#	saving the result in the specified output file.
#
#	The input file name is mandatory and must refer to an existing file
#	If the output file name is not given, then $1.renum will be used.
#
# AUTHOR
#	Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es, 2014
#
function pdb_atom_renumber {
    # Expect a PDB input file name as first argument and an output file as
    # second argument
    local pdb=$1
    local out=${2:-$pdb.renum}

    echo "pdb_atom_renumber: renumbering $pdb to $out"
    if [ ! -e $pdb ] ; then
        echo "pdb_atom_renumber: $1 not found" 
        return $ERROR
    fi

    local dir=`dirname $pdb`
    local fin="${pdb##*/}"
    local ext="${fin##*.}"
    local nam="${fin%.*}"
    if [ ! "$ext" == "pdb" -a ! "$ext" == "brk" ] ; then
        echo "pdb_atom_renumber ERROR: Input file must be a PDB file."
        return $ERROR
    fi
    
    local entries="(^ATOM  )|(^TER   )|(^TER)|(^ANISOU)|(^HETATM)"
    echo -n "" > "$out"	# truncate any previously existing $pdbout
    i=1
    cat $pdb | while read line ; do
        #echo "LINE! $line"
	echo "$line" | egrep -cq "$entries" > /dev/null 2>&1
        if [ $? -eq 0 ] ; then
            idx=`printf "%5d" $i`
            echo "$line" | sed -e "s/\(.\{6\}\).....\(.*\)/\1$idx\2/" >> "$out"
            i=$((i+1))
        else
            # check ENDMDL and reset counter
	    [[ `strindex $line "ENDMDL"` -eq "0" ]] && i=1 # && echo "BINGO!"
            echo "$line" >> "$out"
        fi
    done
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    pdb=$1
    out=${2:-$pdb.renum}
    if [ "$UNIT_TEST" == "no" ] ; then
	pdb_atom_renumber $pdb $out
    else
	pdb_atom_renumber testfile.pdb
    fi
fi


