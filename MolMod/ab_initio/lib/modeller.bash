#!/bin/bash
#

function modeller(){
    # Call the python script that harbours the needed functions for the fusion
    # of protein fragments whose structure was previously predicted and obtain 
    # an approximate model of the overall protein by using the MODELLER software
    # usage: 
    #       modellerscr [seq.fasta]
    # The argument is optional, if it is not provided "sequence.fasta" will be
    # used by default
    sequence=${1:-sequence.fasta}
    
    a=0 
    if [ -e $out/cabs ] ; then
        for i in $out/cabs/*++.pdb ; do
            cp $i model$a ./pdbsdirectory
            a=$(( $a + 1 ))
        done
    fi

    python3 $myhome/modellerscr.py -i $sequence -o sequence.ali \
    -pd `pwd` -t templatesalign.pir -mf multiplealign.pir
}


#[[ $0 != $BASH_SOURCE ]] && echo "Script is being sourced" || echo "Script is being run"
#check if we are being sourced:
# as an included script we do not want to execute anything
if [[ $0 == $BASH_SOURCE ]] ; then
    # if we are not being included by other file, then we are being
    # called as an independent program. Set "INCLUDE=yes" to include
    # all the necessary files and do our work.
    LIB=`dirname $0`
    # this may be either a command or a library function
    if [ -d "$LIB/lib" ] ; then
        LIB="$LIB/lib"
    fi
    [[ -v FS_FUNCS ]] || source $LIB/fs_funcs.bash
    [[ -v UTIL_FUNCS ]] || source $LIB/util_funcs.bash

    modellerscr "$@"
else
    export MODELLERSCR='MODELLERSCR'
fi

