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
DEPENDENCY=( sync_mktop_atom_names )

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
#	make_mktop_topology
#
# SYNOPSIS
#	make_mktop_topology ligand.pdb charge
#
# DESCRIPTION
#	Create a topology for a ligand PDB file using mktop.
#	If a mol2 file exists, it will be used to read the charges.
#	Results will be saved in directory $ligand.mktop
#	A new PDB file with atom names matching those of the
#	topology will be saved in $ligand.mktop/$ligand_NEW.pdb
#
# AUTHOR
#	Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es, 2014
#
function make_mktop_topology {
    local mol=$1
    
    local dir=`dirname $mol`
    local fin="${mol##*/}"
    local ext="${fin##*.}"
    local lig="${fin%.*}"
    
    local babel=${babel:-`which babel`}
    local mktop=${mktop:-`which mktop_2.2.1.pl`}

    if [ ! "$ext" = "pdb" ] ; then
        echo "make_mktop_topology ERROR: Input file must be a PDB file."
        return $ERROR
    fi
    if [ ! -e $mol ] ; then
    	echo "make_mktop_topology ERROR: $mol does not exist"
	return $ERROR
    fi
    
    if [ ! -e $lig.mktop/$lig.opls.itp -a -x "$mktop" ] ; then
        echo ""
        echo "make_mktop_topology: making AMBER and OPLS topologies for $lig using mktop"
        echo ""
        mkdir -p $lig.mktop
        cp $lig.pdb $lig.mktop
        if [ ! -s $lig.mol2 ] ; then
            echo ""
            echo "make_mktop_topology WARNING: no $lig.mol2 file exists"
            echo "	a charge of 0 will be used for all atoms!"
            echo ""
            touch $lig.mktop/$lig.mol2
        else
            cp $lig.mol2 $lig.mktop
        fi
        cd $lig.mktop
        # create charges file from mol2
        #	ectract ATOM section and output the corresponding fields
        #	NOTE:
        #	ATOMS IN THE MOL2 FILE MUST CORRESPOND NUMERICALLY WITH
        #	THE ATOMS IN THE PDB FILE!
        sed -n '/@<TRIPOS>ATOM/,/@<TRIPOS>/{/@<TRIPOS>ATOM/d; /@<TRIPOS>/d;p}' \
        	$lig.mol2 | \
                while read num atom x y z atyp resno res chg ; do 
                    echo "$num $chg" 
                done > $lig.charges
	$mktop -i $lig.pdb -c $lig.charges -ff amber -o $lig.amber.top -conect yes
        if [ $? -eq 1 ] ; then
            # something went wrong. Just in case, sometimes there may
            # be missing CONECT records for one or more atoms (go figure)
            $mktop -i $lig.pdb -c $lig.charges -ff amber -o $lig.amber.top -conect no
	fi
        if [ -e $lig.amber.top ] ; then
            # remove system definition, comment forcefield includes
            #	and give molecule its actual name
            sed -e '/\[ system \]/,//d' -e 's/^#/;#/g' \
                -e "s/^MOL/$lig/g" $lig.amber.top > $lig.amber.itp
        fi
	$mktop -i $lig.pdb -c $lig.charges -ff opls -o $lig.opls.top -conect yes
        if [ $? -eq 1 ] ; then
            # something went wrong. Just in case, sometimes there may
            # be missing CONECT records for one or more atoms (go figure)
            $mktop -i $lig.pdb -c $lig.charges -ff opls -o $lig.opls.top -conect no
	fi
        if [ -e $lig.opls.top ] ; then
            # remove system definition, comment forcefield includes
            #	and give molecule its actual name
            sed -e '/\[ system \]/,//d' -e 's/^#/;#/g' \
                -e "s/^MOL/$lig/g" $lig.opls.top > $lig.opls.itp
        fi
        #
        # MKTOP generates new atom names for the topology
        #	these will not likely match those in the PDB file
        #	and we need to synchronize them for the coordinates
        #	and topology to match
        #
        sync_mktop_atom_names $lig opls > ${lig}_NEW.pdb
        cd -
    fi

    return $OK
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    mol= $1
    if [ "$UNIT_TEST" == "no" ] ; then
	make_mktop_topology $mol
    else
	make_mktop_topology testfile.pdb
    fi
fi
