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
DEPENDENCY=( make_ion_config )

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
#	solvate - Solvate a Gromacs structure file
#
# SYNOPSIS
#	solvate coordinates method
#
# DESCRIPTION
#	Coordinates can be any of a Gromacs GRO file or a PDB file ending in
#	.pdb or .brk
#
#	In addition to the coordinates, a corresponding file with the same
#	name, but ending in .top and containing the Gromacs topology is also
#	needed.
#
#	The method can be any of "none" (for no solvation), "water" for
#	filling up the PBC box with water only, "counterions" for filling
#	the PBC box with water and substituting enough water molecules by
#	ions as needed to neutralize the system total charge, or "saline"
#	for saline serum at a concentration of 0.15 M.
#
#	On exit there will be new files with the same name and an
# 	appropriate extension for the new solvated coordinates and topology:
#	for a 'coordinates' file of the form name.ext we get either one of
#		name+water.ext
#		name+ion.ext
#		name+saline.ext
#	depending on the method selected.
#
# AUTHOR
#	Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es, 2014
#
function solvate {
    local pdb=$1
    local sol=$2

    local dir=`dirname $pdb`
    local fin="${pdb##*/}"
    local ext="${fin##*.}"
    local nam="${fin%.*}"
    
    echo ""
    echo "solvate: solvating $pdb using $sol"
    echo ""
    if [ ! "$ext" == "pdb" -a ! "$ext" == "brk" -a ! "$ext" == "gro" ] ; then
        echo "solvate ERROR: Input file must be a PDB or GRO file."
        return $ERROR
    fi
    if [ ! -e $nam.top ] ; then
    	echo "solvate ERROR: $nam.top does not exist"
    fi
    cd $dir

    # The first parameter is the molecular system to solvate.
    #
    # The second parameter indicates whether to solvate or not
    # and the type of solvation to be done:
    #	none or water or counterions or saline
    #
    if [ "$sol" != 'none' ] ; then
        # By default, anything other than 'none' implies solvation
        echo ""
	echo "solvate: Soaking in water."
        genbox -cp $fin -cs tip4p -o $nam+water.$ext -p $nam.top 
        if [ $? -ne 0 ] ; then return $ERROR ; fi
        # we want to keep files in synchrony
        mv $nam.top $nam+water.top
        mv \#$nam.top.1\# $nam.top

	# Create an ION.MDP config file for subsequent steps
        make_ion_config

        grompp -f ion.mdp -c $nam+water.$ext -p $nam+water.top -o $nam+water.tpr
        if [ $? -ne 0 ] ; then return $ERROR ; fi
        mv mdout.mdp ion.out.mdp
	
        # now add ions if needed
        if [ "$sol" = 'saline' ] ; then
            echo ""
            echo "solvate: Converting water to saline serum."

            #echo 13| genion -s Complex_water.tpr -o Complex_saline.pdb -neutral -conc 0.15 -p Complex_water.top -norandom -nname CL- -pname NA+
            #echo "SOL" | genion -s $nam+water.tpr -o $nam+saline.$ext -neutral \
	    #        -conc 0.15 -p $nam+water.top -norandom -nname CL -pname NA
            echo "SOL" | genion -s $nam+water.tpr -o $nam+saline.$ext -neutral \
	            -conc 0.15 -p $nam+water.top -nname CL -pname NA
            if [ $? -ne 0 ] ; then return $ERROR ; fi
            # make newly created top file match output pdb file name
            mv $nam+water.top $nam+saline.top
            # and recover old top file for the old pdb file name
	    mv ./\#${nam}+water.top.1\# $nam+water.top

        elif [ "$sol" = 'counterions' ] ; then
            echo ""
            echo "solvate: Inserting ions in the water."
	    # Apparently -neutral only works with -conc
	    # But we can let Gromacs compute how many ions are needed
            # by using -conc 0.0000001 
            # <:-O
            #echo 13| genion -s Complex_water.tpr -o Complex_ion.pdb -neutral -conc 0.0000001 -p Complex_water.top -norandom -nname CL- -pname NA+ 
            echo "SOL" | genion -s $nam+water.tpr -o $nam+ion.$ext -neutral -conc 0.0000001 \
	    	    -p $nam+water.top -norandom -nname CL -pname NA
            if [ $? -ne 0 ] ; then return $ERROR ; fi
	    # keep topology and pdb file names in synchrony
            mv $nam+water.top $nam+ion.top
	    # recover the old top file for the old pdb
	    mv ./\#$nam+water.top.1\# $nam+water.top

        fi

    fi 
    
    cd -
    return $OK
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    pdb=$1
    sol=$2
    if [ "$UNIT_TEST" == "no" ] ; then
	solvate $pdb $sol
    else
	solvate
    fi
fi
