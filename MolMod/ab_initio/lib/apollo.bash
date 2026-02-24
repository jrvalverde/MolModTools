#!/bin/bash


function apollo() {
# This function gives a score to a group of models and determines which one 
# is the best
# It has two arguments. The first one is the protein sequence and the second 
# is the directory where the models are to be found
# Usage:
#        apollo sequence.fasta modelfolder

    # Check if the correct number of arguments was given
    if [ $# -lt 2 ] ; then errexit "seqfile.faa modelfolder" 
    else notecont $* ; fi

    # Get the arguments of the function
    local sequence=$1		# path to the sequence
    local modelfolder="$2"	# directory with models to score
    

    # this may happen if $2 is not given or empty
    if [ ! -d "$modelfolder" ] ; then return $ERROR ; fi


    local name=${sequence%.*}		# remove last suffix
    name=${name##*/}				# remove path
    local apollo_out=${modelfolder}.apollo/
    local mlist=${apollo_out}/${name}.mlist

    # Create the directory where the results will be stored
    mkdir -p ${apollo_out}
    ls -1 ${modelfolder}/*.pdb \
        > ${apollo_out}/${name}.mlist	# mlist is the model list
    if [ $? -ne $OK ] ; then
        # there are no models to check
        warncont "No PDB models to score in $modelfolder"
        return $ERROR
    fi

    # Define the path where apollo is installed
    local APOLLO_CMD=~/contrib/multicom_toolbox/apollo_pairwise.64bit/pairwise_qa_apollo
    # Define the path were the TM score is installed
    local TMSCORE_CMD=~/contrib/multicom_toolbox/apollo_pairwise.64bit/tm_score/TMscore_64
    # ensure we find obsolete needed libraries
    export LD_LIBRARY_PATH=~/contrib/multicom/lib:$LD_LIBRARY_PATH

    # Check if the apollo file exists and is executable
    if [ ! -x "$APOLLO_CMD" ] ; then
        errexit "$APOLLO_CMD does not exist or is not executable"
    fi
    # Check if the TM score file exists and is executable
    if [ ! -x "$TMSCORE_CMD" ] ; then
        errexit "$TMSCORE_CMD does not exist or is not executable"
    fi

    # Run apollo
    $APOLLO_CMD \
        ${mlist}	   	${comment# List with the models to be scored} \
        ${sequence}     ${comment# Protein sequence} \
        ${TMSCORE_CMD}  ${comment# Path to the TM score executable} \
        ${apollo_out}   ${comment# Directory where the results will be stored} \
        ${modelfolder}  ${comment# Directory with the models to be scored}

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

    apollo "$@"
else
    export APOLLO='APOLLO'
fi
