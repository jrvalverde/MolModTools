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

#
# Checks if function has been declared if not sources the bash script containing the function
#
# Array of dependencies 
# DEPENDENCY = (funtion1 funtion2 funtion3 )
# Each funtion must be in a file named gromacs_funcs_funtion1.bash 
#
DEPENDENCY=( banner make_ligand_topologies )

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
#	extract_ligands - extract ligands from a PDB file
#
# SYNOPSIS
#	extract_ligands input.pdb {LIGANDS]
#
# DESCRIPTION
#	Extract the ligands from a complex specified by input.pdb
#
#	On exit, the file input.pdb will have been depleted from all
#	ligands, and the ligands will each have been saved in a separate
#	file named after the ligand's PDB entry name.
#
#	Currently, it also generates the topology parameter files for
#	each ligand using ACPYPE.
#
#	An optional parameter "LIGANDS" can be used to specify a text file
#	containing a list of the ligands to extract. This file is assumed
#	to have the format "ligand charge", where ligand is the PDB code 
#	of the ligand, and charge a number with the charge that is to be
#	assigned to it. If the charge is missing, it will be assumed to be
#	zero (0).
#
#	If the optional LIGANDS file is provided, then only the ligands
#	specified in this file will be saved, and any others will be
#	deleted.
#
#	In any case, whether the LIGANDS file is provided or not, a
#	LIGANDS.OK file will be generated at the end listing all the
#	ligands extracted and their charges.
#
# AUTHOR
#	Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es, 2014
#
function extract_ligands {
    # go over each ligand, one by one, and extract it from the original file
    # after we finish, the original file contains everything it originally
    # contained EXCEPT for the ligands extracted
    # if a ligands file is provide, only ligands listed in it are
    # extracted, and the charge specified in the file is used

    local pdb=$1		# usually ./Ligands.pdb or ./OrigComplex.pdb or some such
    local ligands=${2:-'LIGANDS'}	# usually ./LIGANDS
    local chargeModel=${3:-default}
    local topology=${4:-default}

    # a list of ligands to ignore 
    #
    #	These ligands will not be removed, will not be entered
    # into the $ligands.ok file, will not be parametrized and will
    # be left as part of the stripped-down $pdb file) as HETATM
    #
    #	We assume the force-field used will already have parameters
    # defined for these.
    #
    local known_ligands='(MG)|(NA)|(CL)|(CA)|(HOH)|(SO4)|(IOD)|(GOL)|(PO4)'
    #local known_ligands='DO_NOT_IGNORE_ANYTHING'
    
    # provide defaults for external variables
    local babel=${babel:-`which babel`}

    if [ ! -e $pdb ] ; then
        echo "extract_ligands: $1 not found" ; return $ERROR
    fi
    local dir=`dirname $pdb`
    local fin="${pdb##*/}"
    local ext="${fin##*.}"
    local nam="${fin%.*}"
    if [ ! "$ext" == "pdb" -a ! "$ext" == "brk" ] ; then
        echo "extract_ligands ERROR: Input file must be a PDB file."
        return $ERROR
    fi


    echo ""
    echo ">>> extract_ligands: extracting ligands from $pdb"
    echo ""

    rm -f LIGANDS.OK	# we'll rebuild it on the go

    if [ ! -e "$ligands" ] ; then
        # try to figure out ligand list
        # avoiding common ions and "boring" ligands
        grep '^HETATM' $pdb | cut -c18-21 | uniq | \
	egrep -v $known_ligands
    else
        # get list of ligands from file provided
        # only ligands listed in the file will be used
        #cut -f1 $ligands
        cat $ligands
    fi | \
    while read lig charge ; do
        # find out charge for the ligand
        if [ "$charge" == '' ] ; then 
            charge="0"
        fi

        echo ""
        banner "$lig $charge"
        echo ""

	if [ ! -e $lig.pdb ] ; then
            # extract ligand with the H already added
            # and remove it from the protein
            echo "extract_ligands: extracting $lig"
            #
            #	CONECT records in the input file refer to all
            #	molecules, and therefore, we would need to 
            #	distinguish which ones belong to this ligand
            #	to extract only those. It is easier to do it
            #	this way: we extract only the (het)atoms, and then
            #	use babel to rebuild the connectivity.
            #
            grep -h "^HETATM.\{10\} $lig" $pdb | \
    	        $babel -ipdb - -opdb - | \
                egrep '(^ATOM)|(^HETATM)|(^CONECT)' > $lig.pdb

    	    # remove ligand from protein
    	    grep -v "^HETATM.\{10\} $lig" $pdb > tmpProt.pdb
            mv tmpProt.pdb $pdb
	fi
	# check if the ligand file is empty (the ligand was not present)
        # and make the topology
	if [ -s $lig.pdb ] ; then
            make_ligand_topologies $lig.pdb $charge $chargeModel $topology
            
            echo ""
            echo "extract_ligands: $lig	$charge ($ligands)"
            echo "$lig	$charge" >> $ligands.OK
            echo "extract_ligands: $lig	$charge ($ligands.OK)"
	else
            echo ""
            echo "extract_ligands ERROR: could not extract $lig"
            echo ""
            # we'll not stop, but allow the extraction to continue
            # with other ligands.
        fi
    done
    # Ensure any remaining, non-wanted ligand is removed
    #	THIS IS NOT TO BE USED IF WE ALLOW KNOWN LIGANDS TO STAY
    #	AS WE DO NOW.
    #
    #echo "Removing any other ligands"
    #egrep '(^ATOM)|(^TER)|(^ANISOU)' $pdb > tmpProt.pdb
    #mv tmpProt.pdb $pdb

    return $OK
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    pdb=$1		# usually ./Ligands.pdb or ./OrigComplex.pdb or some such
    ligands=${2:-'LIGANDS'}	# usually ./LIGANDS
    chargeModel=${3:-default}
    topology=${4:-default}
    if [ "$UNIT_TEST" == "no" ] ; then
	extract_ligands $pdb $ligands $chargeModel $topology
    else
	extract_ligands testfile.pdb
    fi
fi
