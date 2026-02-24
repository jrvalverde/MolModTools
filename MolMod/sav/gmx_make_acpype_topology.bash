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
#	make_acpype_topology
#
# SYNOPSIS
#	make_acpype_topology ligand.pdb charge
#
# DESCRIPTION
#	Create a topology for a ligand PDB file using acpype.
#	If a mol2 file exists, it will be used to read the charges.
#	Results will be saved in directory $ligand.acpype
#
# AUTHOR
#	Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es, 2014
#
function make_acpype_topology {
    local mol=$1
    local charge=$2
    
    local dir=`dirname $mol`
    local fin="${mol##*/}"
    local ext="${fin##*.}"
    local lig="${fin%.*}"
    
    local acpype=${acpype:-`which acpype`}
    local babel=${babel:-`which babel`}

    if [ ! "$ext" = "pdb" ] ; then
        echo "make_acpype_topology ERROR: Input file must be a PDB file."
        return $ERROR
    fi
    if [ ! -e $mol ] ; then
    	echo "make_acpype_topology ERROR: $mol does not exist"
	return $ERROR
    fi
    
    # do not repeat if already done
    if [ -s $lig.acpype/${lig}_NEW.pdb ] ; then
    	echo "make_acpype_topology: Using existing topology in $lig.acpype"
        return $OK
    fi

    # 	compute multiplicity
    # HEURISTIC: assume all charges are due to e- that yield unpaired spin
    #	ignore minus sign (i.e. take absolute value)
    local mult=$(( ${charge#-} + 1 ))
    echo "make_acpype_topology: $lig $charge $mult"

    # 	If a MOL2 file is available, assume it has charges and use them
    if [ -s $lig.mol2 ] ; then
        echo "make_acpype_topology: Generating parameters using $lig.mol2"
        $acpype -i $lig.mol2 -n $charge -m $mult -b $lig -r -l -a amber -c user
    fi
    
    # if  there was not MOL2, compute charges using SQM from Antechamber
    if [ $? -ne 0 ] ; then
        #
        # the user didn't supply a mol2 file or we couldn't use the one available.
        # compute charges using SQM from Antechamber
        echo "make_acpype_topology: could not use charges from mol2 file, trying SQM"
        $acpype -i $lig.pdb -n $charge -m $mult -b $lig -r -l -a amber
    else
        # we are done
        return $OK
    fi
    
    # if SQM failed, then resort to using babel to compute charges
    # iterating over known babel methods
    if [ $? -ne 0 ] && [ -x "$babel" ] ; then
        echo "make_acpype_topology: COULD NOT USE SQM CHARGES."
        # try successively with each of babel -L charges methods
	for i in qtpie qeq eem mmff94 gasteiger ; do
            echo "make_acpype_topology: assigning charges using babel $i"
            $babel -ipdb $lig.pdb --partialcharge $i -omol2 $lig.$i.mol2
            acpype -i $lig.$i.mol2 -n $charge -m $mult -b $lig -r -l -a amber -c user
            if [ $? -eq 0 ] ; then
                break
            fi
        done
    else
        return $OK
    fi
    
    # if Babel charges failed, then try to use Antechamber Gasteiger charges
    if [ $? -ne 0 ] ; then
        echo "make_acpype_topology: COULD NOT USE BABEL. USING GASTEIGER CHARGES"
        $acpype -i $lig.pdb -n $charge -m $mult -b $lig -r -l -a amber -c gas
    else
        return $OK
    fi
    
    # if everything failed, tell the user
    if [ $? -ne 0 ] ; then
	echo "make_acpype_topology ERROR: COULD NOT GENERATE ACPYPE TOPOLOGIES FOR $lig $charge $mult"
        return $ERROR
    fi
    # ligand topologies are in $lig.acpype/$lig_GMX*.itp and $lig.oplsaa.itp

    return $OK
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    mol=$1
    charge=$2
    if [ "$UNIT_TEST" == "no" ] ; then
	make_acpype_topology $mol $charge
    else
	make_acpype_topology testfile.pdb
    fi
fi

