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
#	add_ligand_oplsaa_acpype
#
#
# AUTHOR
#	Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es, 2014
#
function add_ligand_oplsaa_acpype {
    complex=$1
    lig=$2
    
    if [ -s $lig.acpype/${lig}_GMX_OPLS.itp ] ; then
        if [ ! -e $lig.acpype/${lig}_NEW.pdb ] ; then
            echo ""
            echo "add_ligand_oplsaa ERROR: topology of $lig not computed successfully"
            return $ERROR
        fi
        
	# Add updated $lig_NEW.pdb to the Complex
        #	NOTE: $lig_NEW contains ALL the hydrogens, it
        #	matches the ITP file, but may not have the 
        #	ionization state that we want.
        grep -h ATOM $lig.acpype/${lig}_NEW.pdb >> ${lig}_top.pdb

	# install the ligand's topology file
	cp $lig.acpype/${lig}_GMX_OPLS.itp $lig.itp
        
	# DO NOT USE. IT IS BETTER TO LET THE SYSTEM FAIL AND
        #	FORCE THE USER TO DO THE RIGHT THING
        #
        # OPLS-AA specific tweak
        #
        # We may need to comment out some dih parameters in Ligand.itp that 
        # OPLS/AA does not recognise.
        # Note that the parameters values are derived for AMBER99SB/GAFF and NOT for OPLS/AA.
        # A proper solution would involve finding the right OPLS atom types and parameters for the
        # calculation according to [http://dx.doi.org/10.1021%2Fja9621760 Jorgensen et al. (1996)].
        # Be aware of it!
	if [ "not" = "yes" ] ; then
	    sed -E 's/([0-9]+ +[0-9]+ + [0-9]+ );/\1 /g'$lig.itp > TmpFiLe; mv -b TmpFiLe $lig.itp
        fi

        # extract new atomtypes (we only want them included once
        # at the beginning, hence we'll process them separately)
        cat $lig.acpype/${lig}_GMX_OPLS.itp | \
            sed -n '1,/atomtypes/p;/moleculetype/,$p' | \
            grep -v " atomtypes " > $lig.itp
        sed -n -e '/atomtypes/,/^$/p' $lig.acpype/${lig}_GMX_OPLS.itp >> atomtypes
        #
        # or perhaps it would be better to remove any new atomtype
        # and let the run fail later?
        #
        #sed -e '/atomtypes/,/^$/d' $lig.acpype/${lig}_GMX_OPLS.itp > $lig.itp

        banner " WARNING!"
        echo ""
        echo "WARNING: Using ACPYPE topology for $lig"
        echo ""
        echo "most likely some parameters or atom types will be"
        echo "unavailable."
        echo ""
        echo "If simulation fails, you may be able to repair it"
        echo "by uncommenting missing parameters (removing the ';')"
        echo "but this will likely enable approximate parameters"
        echo "derived from AMBER that may be far from ideal."
        echo ""
        echo "Better than that, you should review your original"
        echo "ligands, ensure they have all needed H and bonds"
        echo "and save them as separate PDB files with CONECT"
        echo "records."
        echo ""
        echo "Even better yet: in addition you should consider"
        echo "computing charges by an accurate method and saving"
        echo "them also in a .mol2 file"
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

	# Rebuild atomtypes.itp:
        # now insert any new atom types introduced by the new topologies
        # before the ligands' definitions.
        #
        #	With ACPYPE we may have many chances of getting an
        #	incomplete topology with new atom types that should be 
        #	reviewed.
        #	New atomtypes can only be included once, which is why
        #	we need to rebuild atomtypes.itp and check if it has
        #	been already included
	echo "[ atomtypes ]" > atomtypes.itp
        #echo "; name    at.num      mass     charge   ptype  sigma         epsilon" >> atomtypes.itp
        # first ensure there are no duplicates
        newatoms=0
        while IFS='' read line ; do 
            atom=`echo $line | cut -c 1-3 | tr -d ' '`
            echo "${atom}=${line}"
            grep -q "^$atom " $GMX_TOP/oplsaa.ff/ffnonbonded.itp
            if [ $? -ne 0 ] ; then 
                echo "NOT FOUND: $line" 
                echo "$line" >> atomtypes.itp
                newatoms=$(($newatoms+1))
            fi
        done < <(sort atomtypes | uniq | grep "^ ")
        echo "" >> atomtypes.itp
        if [ $newatoms -gt 0 ] ; then
            banner "IMPORTANT"
            banner " WARNING "
            echo "New atom types defined"
            echo "Edit the file 'atomtypes.itp' to revise new atom num, mass and charge"
            echo "See \$GMX_TOP/oplsaa.ff/ffnonbonded.itp as reference"
            exit 1
        fi
        # add atomtypes.itp to the complex if not already included
	if ! grep -q '#include "atomtypes.itp"' Complex.top ; then
            cat Complex.top | sed "/forcefield\.itp\"/a\
#include \"atomtypes.itp\"\
            " >| Complex2.top
            mv Complex2.top Complex.top
	fi
        
	return $OK
    else
        echo "add_ligand_oplsaa_acpype ERROR: no valid topology found for $lig"
        echo "add_ligand_oplsaa_acpype ERROR: $lig.acpype/${lig}_GMX_OPLS.itp not found"
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
	add_ligand_oplsaa_acpype $complex $lig
    else
	add_ligand_oplsaa_acpype
    fi
fi

