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
# Checks if function has been declared. If not, sources the bash script containing the function
#
# Array of dependencies 
# DEPENDENCY = (funtion1 funtion2 funtion3 )
# Each funtion must be in a file named gromacs_funcs_funtion1.bash 
#
DEPENDENCY=( add_ligand_oplsaa_topolbuild add_ligand_oplsaa_topolgen add_ligand_oplsaa_mktop add_ligand_oplsaa_acpype )

for name in "${DEPENDENCY[@]}"
  do
     if [ -z "$(type $name &> /dev/null)" ] || [ "$(type -t $name &> /dev/null)" != function ]; then
         if [ -f gromacs_funcs_$name.bash ]; then
	     . gromacs_funcs_$name.bash
         else
    	     echo File gromacs_funcs_$name.bash not found.
         fi
     fi 
done


#
# NAME
#	add_ligand_oplsaa -- add a ligand to a Gromacs complex
#
# SYNOPSIS
#	add_ligand_oplsaa complex ligand
#
# DESCRIPTION
#	Expects to get two file datasets, complex and ligand, to
#	merge them.
#
#	Each of the datasets must consist of a PDB file and a 
#	Gromacs topology constellation (the .itp and all required
#	.itp sub-include files).
#
#	If the topology is not available, then it will search for
#	existing topologies that could alternately be used and
#	select one of them based on a hard-coded hierarchy:
#
#	topolbuild > mktop > topolgen > acpype
#
#	If the topology selected does not work (which will be
#	seen only later), then the user simply needs to take 
#	another one and manually install it instead.
#
# CAVEAT
#	The selection of topology is arbitrary. We are not yet
#	sure which is best than the others. Beware!
#
# AUTHOR
#	Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es, 2014
#
function add_ligand_oplsaa {
    local complex=$1
    local lig=$2
    
    # NOTE: We should check argument validity

    echo ""
    echo "add_ligand_oplsaa: Adding ligand $lig to complex $complex"
    echo "add_ligand_oplsaa: topology=$topology"
    echo ""
    
    # if there is already a topology, we should use it:
    if [ -s $lig.itp -a -s ${lig}_top.pdb ] ; then
        echo "add_ligand_oplsaa: Using existing $lig.itp topology"
        # add updated coorfinates to complex
        egrep '(^ATOM  )|(^HETATM)' ${lig}_top.pdb | \
            sed -e 's/HETATM/ATOM  /g' >> $complex.pdb

        # add ligand topology to complex
        cat $complex.top | sed "/forcefield\.itp\"/a\
#include \"$lig.itp\"\
        " >| ${complex}Tmp.top

        # add molecule count to complex
        echo "$lig      1" >> ${complex}Tmp.top
        mv ${complex}Tmp.top $complex.top
        #echo "$lig added to $complex.top"

        return $OK
    fi

    # UNDOCUMENTED: use command-line selected topology
    case $topology in
    	topolbuild)
        	add_ligand_oplsaa_topolbuild $complex $lig
		return $? ;;
        topolgen)
        	add_ligand_oplsaa_topolgen $complex $lig
                return $? ;;
        mktop)
        	add_ligand_oplsaa_mktop $complex $lig
                return $? ;;
        acpype)
        	add_ligand_oplsaa_acpype $complex $lig
                return $? ;;
        *)
        	break ;;	# use default heuristic
    esac

    # first we prefer topolbuild topology
    if [ -e $lig.topolbuild/$lig.itp ] ; then
    	add_ligand_oplsaa_topolbuild $complex $lig
    
    elif [ -e $lig.mktop/$lig.opls.itp ] ; then
	add_ligand_oplsaa_mktop $complex $lig
	
    # next, we prefer topolgen (charge corrected) topology
    elif [ -e $lig.topolgen/$lig.oplsaa.itp ] ; then
	add_ligand_oplsaa_topolgen $complex $lig
    
    # finally we'll use ACPYPE topology
    elif [ -e $lig.acpype/${lig}_GMX_OPLS.itp ] ; then
	add_ligand_oplsaa_acpype $complex $lig
    else
        banner "  ERROR  "
    	echo ""
    	echo "add_ligand_topology ERROR: no valid topology found for $lig"
        echo "add_ligand_topology ERROR: $lig NOT ADDED!!!"
	echo ""
    fi
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    if [ "$UNIT_TEST" == "no" ] ; then
        complex=$1
        lig=$2
 	add_ligand_oplsaa $complex $lig
    else
	add_ligand_oplsaa
    fi
fi

