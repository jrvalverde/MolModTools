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

# NAME
#	pdb2mol2 - convert a PDB file to Mol2 format assigning charges
#
# SYNOPSIS
#	pdb2mol2 file.pdb totalCharge chargeModel
#
# DESCRIPTION
#	Convert a PDB filt to mol2 format trying as many charge-computation
#	methods as possible.
#
#	if model is specified, then the selected charge model will be
#	installed, otherwise, a default heuristic will be used to select
#	the most likely best one. All supported charge models will be
#	computed anyway.
#
#	Results will be saved as both, MOL2 and PDB files to ensure that
#	atom positions and names are kept in synchrony. The output files
#	will ne named according to the input file name followed by
#		.$method.pdb and $method.mol2
#
#	Currently supported methods (outside CNB/CSIC) include
#	    ab initio
#		none yet
#	    semiempirical
#		openmopac
#		chimera AM1/BCC
#	    empirical
#		babel (with --partialcharges, use babel -L charges for a list)
#
# AUTHOR
#	(C) Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es 2014
#
function pdb2mol2 {
    local mol=$1
    local charge=$2
    local model=${3:-default}

    local openmopac=${openmopac:-`which MOPAC2009.exe`}
    local babel=${babel:-`which babel`}
    local babel="/home/scientific/contrib/openbabel/bin/babel"
    local chimera=${chimera:-`which chimera`}
    local ergoSCF=${ergoSCF:-`which ErgoSCF.bash`}
    local freeON=${freeON:-`which freeon.bash`}
    local gamessUS=${gamessUS:-`which gamess.sh`}
    local NWchem=${NWchem:-`which nwchem.bash`}
    local abinit=${abinit:-`which abinit.bash`}
    
    local dir=`dirname $mol`
    local fin="${mol##*/}"
    local ext="${fin##*.}"
    local lig="${fin%.*}"
    
    if [ ! "$ext" = "pdb" ] ; then
        echo "make_ligand_topology ERROR: Input file must be a PDB file."
        return $ERROR
    fi
    if [ ! -e $mol ] ; then
    	echo "make_ligand_topology ERROR: $mol does not exist"
	return $ERROR
    fi
    
    # Try to make a .mol2 file with charges from a PDB file
    if [ -e $lig.mol2 ] ; then 
        echo "Using existing $lig.mol2"
        return $OK
    fi

    pushd `pwd`
    cd $dir
    # save all computed mol2 files in a subdirectory
    mkdir -p $lig.mmooll22
    cp $lig.pdb $lig.mmooll22
    cp $lig.pdb $lig.pdb.orig
    cd $lig.mmooll22

    # OpenMopac (this loses TRIPOS atomtypes and may pose a problem later).
    #
    # 	compute multiplicity
    # HEURISTIC: assume all charges are due to e- that yield unpaired spin
    #	ignore minus sign (i.e. take absolute value)
    local mult=$(( ${charge#-} + 1 ))
    #
    
    echo ""
    if [ ! -s "$lig.mopac.mol2" -a -x "$openmopac" -a -x "$babel" ] ; then
        echo "pdb2mol2 trying to make a mol2 file with $openmopac"
        $babel -ipdb $lig.pdb -omopcrt $lig.mopcrt
    	sed -e "s/PUT KEYWORDS HERE/1SCF CHARGE=$charge MULLIK/g" $lig.mopcrt > $lig.mop
        #$openmopac $lig.mop
        # HACK HACK HACK
        #	we need to use openmopac2009 or earlier since the latest
        #	openmopac2012 has changed the output format and makes babel
        #	crash. Next is a hack to continue using an expired 2009 or
        #	earlier version until a fix is available.
        $openmopac $lig.mop <<END


END
        # babel sets the molecule name to $lig.out, fix it
        $babel -imopout $lig.out -omol2 - | \
            sed -e "s/$lig.out/$lig/" > $lig.mopac.mol2
        # some topology builders choke on unterminated babel mol2
        echo "@<TRIPOS>" >> $lig.mopac.mol2
        # openmopac may have changed atom order
	$babel -imopout $lig.out -opdb - | \
            sed -e "s/^COMPND    $lig.out/COMPND    $lig/g" \
                -e "s/ LIG / $lig /g" >$lig.mopac.pdb
    fi
    
    # Try to make mol2 using Chimera and Antechamber:
    # add AM1-BCC charges with GAFF atom types and save as .mol2
    #
    if [ ! -s $lig.am1-bcc.mol2 -a -x "$chimera" ] ; then
        # try to assign charges using Chimera and antechamber
        echo "pdb2mol2: Attempting to asssign charges using Chimera with AM1/BCC"
        $chimera --nogui <<END
            open $lig.pdb
            #addh inisolation true hbond true
            addcharge all chargeModel 99sb method am1
            write format mol2 0 $lig.am1-bcc.mol2
            write format pdb  0 $lig.am1-bcc.pdb
            stop now
END
	# Chimera saves the molecule named as "$lig.pdb" in the mol2 file
        sed -e "s/$lig.pdb/$lig/g" $lig.am1-bcc.mol2 > $lig.tmp
        mv $lig.tmp $lig.am1-bcc.mol2
	# NOTE: review the output to check that all went as expected.
        #	If there is something wrong, CORRECT THE ORIGINAL PDB FILE.
    fi

    if [ -x "$babel" ] ; then
    	# We cannot get list of supported models and try them one by one
        # because they are not listed in order of accuracy.
        # Instead we'll hardcode the values in an arbitrary order and
        # try each whether it is supported or not.
        for i in qtpie qeq eem mmff94 gasteiger ; do
            echo "pdb2mol2: assigning charges using babel $i"
            $babel -ipdb $lig.pdb --partialcharge $i -omol2 $lig.$i.mol2
            echo "@<TRIPOS>" >> $lig.$i.mol2
            if [ -s $lig.$i.mol2 ] ; then
                $babel -imol2 $lig.$i.mol2 -opdb $lig.$i.pdb
                # babel sets the molecule name to $lig.pdb
                sed -e "s/$lig.pdb/$lig/" $lig.$i.pdb > $lig.tmp
                mv $lig.tmp $lig.$i.mol2
	    else
                rm -f $lig.$i.mol2
            fi
        done
    fi
    
    if [ -x "$ergoSCF" ] ; then
        # DISABLED. Will potentially take a very long time to run
        #	and raises the problem of choosing an appropriate 
        #	basis set
        echo "pdb2mol2: SORRY $ergoSCF support is only available at CNB/CSIC"
        exit
        
        # you need CNB/CSIC ergoSCF.bash to run this
        #	and must make sure the mol2 and xyz files correspond to each other
        $babel -ipdb $lig.pdb -oxyz $lig.xyz
        $ergoSCF -i $lig.xyz -c $chargea -m HF -g STO3-G -b aug-cc-pCVDZ -u -p long
	# Note that the calculation may fail if any atom is not supported
        # by any of the force fields. That can be found out by using
        #	$ergoSCF -i $lig.xyz -b help
        #
	# After the calculation has been done, Mulliken charges will be 
        # ready on the output file $lig/$lig.STO-3G.ug-cc-pCV5Z.ergoscf
        # we must extract them and add them to a mol2 file.
        $babel -ipdb $lig.xyz -oxyz $lig.mol2
        grep 'INSC Mulliken charge of atom' $lig/$lig.STO-3G.aug-cc-pCV5Z.ergoscf | \
    	sed -e 's/INSC Mulliken charge of atom //g' | \
        tr -d '=' | while read i ch ; do
            i=$((i + 1))
            echo $i $ch
            sed  -e "/ \+ $i .* LIG/ s/\(.*\) LIG1.*/\1 $lig       $ch/"  $lig.mol2 > tmpFile.mol2
            mv tmpFile.mol2 $lig.mol2
        done
    fi
    if [ -x "$freeON" ] ; then
        echo "pdb2mol2: SORRY $freeON support is only available at CNB/CSIC"
    fi
    if [ -x "$gamessUS" ] ; then
        echo "pdb2mol2: SORRY $gamessUS support is only available at CNB/CSIC"
    fi
    if [ -x "$NWchem" ] ; then
        echo "pdb2mol2: SORRY $NWchem support is only available at CNB/CSIC"
    fi
    if [ -x "$orca" ] ; then
        echo "pdb2mol2: SORRY $orca support is only available at CNB/CSIC"
    fi
    
    # Now try to select the possibly best mol2 file and
    # copy it -and the matching PDB file- to the target directory
    #
    # we'll select them in the following hardcoded precedence
    #
    #	NOTE that ab initio do not normally take into account Dirac 
    #	relativistic effects, and so may be inaccurate for heavy metals
    if [ -s $lig.$model.mol2 -a -s $lig.$model.pdb ] ; then
        # install user-selected model
        cp $lig.$model.mol2 ../$lig.mol2
        cp $lig.$model.pdb ../$lig.pdb
        return $OK
    else
        echo "pdb2mol2 ERROR: cannot find files for selected model $model"
        echo "pdb2mol2 ERROR: reverting to default selection"
    fi

    echo ""
    if [ -s $lig.aug-cc-pCV5Z.mol2 ] ; then
        cp $lig.aug-cc-pCV5Z.mol2 ../$lig.mol2
        cp $lig.aug-cc-pCV5Z.pdb  ../$lig.pdb
        echo "pdb2mol2: setting $lig.mol2 to $lig.aug-cc-pCV5Z.mol2"
        
    elif [ -s $6-311++Gss.mol2 ] ; then
        cp $lig.6-311++Gss.mol2 ../$lig.mol2
        cp $lig.6-311++Gss.pdb  ../$lig.pdb
        echo "pdb2mol2: setting $lig.mol2 to $lig.6-311++Gss.mol2"
    
    # below this level, accuracy of DFT and SME QM often go hand in hand
    # semiempirical parametrization implicitly includes relativistic effects
    # but it does not support all elements in the periodic table
    elif [ -s $lig.mopac.mol2 ] ; then
	cp $lig.mopac.mol2 ../$lig.mol2
        cp $lig.mopac.pdb  ../$lig.pdb
        echo "pdb2mol2: setting $lig.mol2 to $lig.mopac.mol2"
    
    elif [ -s $lig.am1-bcc.mol2 ] ; then
        cp $lig.am1-bcc.mol2 ../$lig.mol2
        cp $lig.am1-bcc.pdb  ../$lig.pdb
        echo "pdb2mol2: setting $lig.mol2 to $lig.am1-bcc.mol2"
    
    # next come empirical methods implemented in babel
    elif [ -s $lig.qtpie.mol2 ] ; then
        cp $lig.qtpie.mol2 ../$lig.mol2
        cp $lig.qtpie.pdb  ../$lig.pdb
        echo "pdb2mol2: setting $lig.mol2 to $lig.qtpie.mol2"
    
    elif [ -s $lig.qeq.mol2 ] ; then
        cp $lig.qeq.mol2 ../$lig.mol2
        cp $lig.qeq.pdb  ../$lig.pdb
        echo "pdb2mol2: setting $lig.mol2 to $lig.qeq.mol2"
    
    elif [ -s $lig.eem.mol2 ] ; then
        cp $lig.eem.mol2 ../$lig.mol2
        cp $lig.eem.pdb  ../$lig.pdb
        echo "pdb2mol2: setting $lig.mol2 to $lig.eem.mol2"
    
    elif [ -s $lig.mmff94.mol2 ] ; then
        cp $lig.mmff94.mol2 ../$lig.mol2
        cp $lig.mmff94.pdb  ../$lig.pdb
        echo "pdb2mol2: setting $lig.mol2 to $lig.mmff94.mol2"
    
    elif [ -s $lig.gasteiger.mol2 ] ; then
        cp $lig.gasteiger.mol2 ../$lig.mol2
        cp $lig.gasteiger.pdb  ../$lig.pdb
        echo "pdb2mol2: setting $lig.mol2 to $lig.gasteiger.mol2"
    
    else
        echo "pdb2mol2 ERROR: Couldn't make a mol2 file for $lig"
        echo "pdb2mol2 ERROR: this should be exceedingly rare"
        return 1
    fi
    echo ""
    
    cd ..

    popd
    echo ""
    return 0
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    mol=$1
    charge=$2
    model=${3:-default}
    if [ "$UNIT_TEST" == "no" ] ; then
	pdb2mol2 $mol $charge $model
    else
	pdb2mol2 testfile.pdb
    fi
fi

