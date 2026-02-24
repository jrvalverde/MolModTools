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
# Includes
#
# Checks for definition $COMMON, if not set sources bash script with all common variables
#
if [ -z ${COMMON+x} ]; then 
    . gromacs_funcs_common.bash
fi

# NAME
#	split_complex - split a complex into protein and ligands
#
# SYNOPSIS
#	split_complex input.pdb
#
# DESCRIPTION
#	Split a complex specified by input.pdb into protein and ligands. 
#
#	On exit, two files will have been created: Protein.pdb and Ligands.pdb
#
# AUTHOR
#	Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es, 2014
#
function split_complex {
    # split a PDB file into protein and ligands
    # if a LIGANDS file exists, use it, otherwise try our best
    local pdb=$1
    local ligands=${2:-LIGANDS}

    echo ""
    echo ">>>split_complex: splitting $pdb into protein and ligands"
    echo ""

    if [ ! -e $pdb ] ; then
        echo "split_complex: $1 not found" ; return $ERROR
    fi
    
    local dir=`dirname $pdb`
    local fin="${pdb##*/}"
    local ext="${fin##*.}"
    local nam="${fin%.*}"
    if [ ! "$ext" == "pdb" -a ! "$ext" == "brk" ] ; then
        echo "split_complex ERROR: Input file must be a PDB file."
        return $ERROR
    fi

    cd $dir
    
    # Separate protein and ligands
    if [ ! -e Protein.pdb ] ; then
        egrep '(^ATOM )|(^ TER)|(^ANISOU)' $fin >| Protein.pdb

        # allow to override ligand
        if [ ! -e Ligands.pdb ] ; then
	        grep '^HETATM' $fin.pdb >| Ligands.pdb
        fi 
    else
        echo "split_complex: Using Protein.pdb and Ligands.pdb from previous run."
    fi

    cd -
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    pdb=$1
    ligands=${2:-LIGANDS}
    if [ "$UNIT_TEST" == "no" ] ; then
	split_complex $pdb $ligands
    else
	split_complex testfile.pdb
    fi
fi
