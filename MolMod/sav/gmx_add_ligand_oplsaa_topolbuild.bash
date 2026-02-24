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
#	add_ligand_oplsaa_topolbuild
#
#
# AUTHOR
#	Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es, 2014
#
function add_ligand_oplsaa_topolbuild {
    complex=$1
    lig=$2
    
    if [ -e $lig.topolbuild/$lig.itp ] ; then
        echo "add_ligand_oplsaa:  Using topolbuild topology"
        # use updated PDB matching topology files
        cp $lig.topolbuild/$lig.pdb ${lig}_top.pdb
        # install topology files
        cp $lig.topolbuild/$lig.itp $lig.itp
	cp $lig.topolbuild/ff$lig.itp .
        cp $lig.topolbuild/posre$lig.itp .
        
        banner "WARNING!"
        echo ""
        echo "WARNING: Using topolbuild to create the topology for $lig"
        echo ""
        echo "TopolBuild may generate some incomplete or redudant data"
        echo "such as extra '[ defaults ]' sections and wrong dihedrals"
        echo ""
        echo "We have tried to apply some fixes, but if your calculation"
        echo "fails, you may need to comment out (put a ';' before) the"
        echo "offending lines and review carefully the $lig.itp file."
        echo ""
        echo "Look specially for offending atom or parameter definitions"
        echo "and check them against the corresponding force filed parameters"
        echo ""
        echo "You may find useful to consult other topologies generated"
        echo "by different programs (stored in specific subdirectories)"
        echo ""
        echo "If that fails, search the Net for existing parameters or"
        echo "ask in specialized mailing lists for help"
        echo ""
        
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
	add_ligand_oplsaa_topolbuild $complex $lig
    else
	add_ligand_oplsaa_topolbuild 
    fi
fi
