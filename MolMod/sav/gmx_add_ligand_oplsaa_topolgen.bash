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
DEPENDENCY=( banner )

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

#
# NAME
#	add_ligand_oplsaa_topolgen
#
#
# AUTHOR
#	Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es, 2014
#
function add_ligand_oplsaa_topolgen {
    complex=$1
    lig=$2
    
    if [ -e $lig.topolgen/$lig.oplsaa.itp ] ; then
        echo "add_ligand_oplsaa:   Using topolgen topology"
        # original PDB file which SHOULD HAVE CONTAINED ALL ATOMS
        cp $lig.topolgen/$lig.pdb ${lig}_top.pdb
        # the itp file is self contained and matches the
        cp $lig.topolgen/$lig.oplsaa.itp $lig.itp

        banner " WARNING!"
        if [ ! -e $lig.topolgen/$lig.oplsaa.itp.topolgen ] ; then
            echo ""
            echo "WARNING: Using a non-charge corrected TopolGen topology for $lig"
            echo ""
            echo "The charges assigned are only a rough approximation,"
            echo "you should seriously consider creating a MOL2 file"
            echo "with accurate charges and using it as template to"
            echo "correct topolgen charges."
            echo ""
            echo "Some hints for computing charges:"
            echo "	Use any SQM or QM program (e.g. mopac)"
            echo "	Use Antechamber"
            echo "	Use UCSF Chimera with AM1-BCC"
            echo "	Use babel --partialcharge with any of"
            echo "		qtpie qeq eem mmff94 or gasteiger"
            echo ""
            echo "You may find useful to consult other topologies generated"
            echo "by different programs (stored in specific subdirectories)"
            echo ""
            echo "If that fails, search the Net for existing parameters or"
            echo "ask in specialized mailing lists for help"
            echo ""
        else
            echo ""
            echo "WARNING"
            echo ""
            echo "Using TopolGen topolgy for $lig"
            echo ""
            echo "If it fails, is incomplete or gives errors later on,"
            echo "you may find useful to consult other topologies generated"
            echo "by different programs (stored in specific subdirectories)"
            echo ""
            echo "If that fails, search the Net for existing parameters or"
            echo "ask in specialized mailing lists for help"
            echo ""
	fi
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
    else
        return $ERROR
    fi
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    complex=$1
    lig=$2
    if [ "$UNIT_TEST" == "no" ] ; then
	add_ligand_oplsaa_topolgen $complex $lig
    else
	add_ligand_oplsaa_topolgen
    fi
fi

