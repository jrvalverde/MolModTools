#!/bin/bash
#
#	Our protocol for ab-initio protein structure prediction.
#
# set -x

input_sequence=$1
if [ ! -s $input_sequence ] ; then echo "ERROR: $input_sequence does not exist!" ; exit ; fi

VERBOSE=1
maxmodels=5			    # Maximum number of models to carry forward

# Load utility functions
# ----------------------
# we know that $myname and $myhome must exist because we are running
# and $myname is the command name used to run us
myname=`basename "${BASH_SOURCE[0]}"`	# identify oue source command
mydir=`dirname "${BASH_SOURCE[0]}"`		# identify our directory
myhome=`dirname "$0"`
MYHOME=$myhome							# global variable
if [ ! -d "$myhome"/lib ] ; then echo "ERROR: $myhome/lib does not exist!"; exit ; fi

export FUNCS_BASE="$myhome"/lib
source "$FUNCS_BASE"/fs_funcs.bash
source "$FUNCS_BASE"/util_funcs.bash
source "$FUNCS_BASE"/split_fasta.bash       # defines split_fasta_seq_to_dir()
source "$FUNCS_BASE"/i-tasser.bash          # defines i-tasser()
source "$FUNCS_BASE"/alphafold.bash         # defines alphafold()
source "$FUNCS_BASE"/dncon2.bash            # defines dncon2()
source "$FUNCS_BASE"/apollo.bash            # defines apollo()
source "$FUNCS_BASE"/pepbuild.bash          # defines pepbuild()
source "$FUNCS_BASE"/cabs_cm_00.bash        # defines cabs_cm_00()
source "$FUNCS_BASE"/fix_3D_templates.bash  # defines fix_3D_templates()
source "$FUNCS_BASE"/martini.bash           # defines martini()
source "$FUNCS_BASE"/modeller.bash			# defines modeller()

# DEBUG CODE: check what has been defined
#declare -F			       # list all known functions for debugging
#declare -F | awk '{print $NF}' | sort | egrep -v "^_" 

# get sequence name without path and extension
sequence=`basename $input_sequence`
name=${sequence%.*}		# remove suffix (.fasta/.fas/.faa/...)
name=${name##*/}		# remove pathname (leave only sequence name)

# convert to absolute path names; these should never fail
myhome=`abspath $myhome`
if [ $? -ne $OK ] ; then exit "ERROR: cannot find command folder $myhome" ; fi
input_sequence=`abspath $input_sequence`	# use full path name to avoid trouble
if [ $? -ne $OK ] ; then exit "ERROR: cannot find input sequence $input_sequence" ; fi

# start
# -----
echo "Processing $name"
if [ "$name" = "" ] ; then
    errexit "$input_sequence results in an empty sequence name"
fi

# we will save all output in a directory named after the sequence
out="$name"

# create initial output folders
mkdir -p "$out"
mkdir -p "$out"/starting_models
mkdir -p "$out/log"

# send all output to a log file inside the output folder
redirect_std_out_err "$out/log" "$myname"."$out"

# save a copy of the sequence in the output folder
cp "$input_sequence" "$out"

# if the sequence is too large, split it into pieces
# XXX JR XXX: NOTE, THIS STILL NEEDS TO BE REFINED
if [ "YES" = "NO" ] ; then
    split_fasta_seq_to_dir $out/$sequence $out
    nchunks=$?
    if [ $nchunks -gt 1 ] ; then
        # if we got several overlapping pieces, process each separately
        cd $out
        # run this script ($0) on each of the subsequences    
        for i in *overlap.fasta ; do
            $0 $i
        done
        # at this point we should have models for each of the overlapping
        # subsequences
        # now we need to run modeller using as templates the models selected
        # for each of the subsequences to get a full model for the whole 
        # protein
        # Create the directory where modeller will be run
        mkdir -p modeller
        # Copy the protein sequence in that folder
        cp $sequence modeller/sequence.fasta
        for i in *.overlap.fasta ; do	# copy templates to modeller folder
            piece_out=`basename $i .fasta`
            cp $piece_out/model/model1.pdb modeller/
        done
        cd modeller
    #    eval modeller modeller/sequence.fasta 
        exit
    fi
fi


######################################################################
#                                                                    #
# First, generate some (hopefully) realistic/useful models           #
#                                                                    #
######################################################################

# we will use at this step a set of tools that have proven valid in CASP
# i-tasser
#	(ideally we would like to also run Sparks and RaptorX)
# DNCon2
#	only with CONFOLD2 as it works better than CONFOLD1 and UNICON3D
# Alphafold2
# Multicom
#	(ideally we would include Quark, C-Quark and Rosetta as well)
# as well as
# PepBuild
#	To ensure we at least have some naïve initial models to refine
# 	Phi-Psi-prediction
# 	Secondary structure prediction
# 	naïve structures (alpha, beta, coiled-coil, extended, random)
#

# I-TASSER
# --------
#
# First, check if we already have a model
#	This is useful if we already have one or more models that we obtained 
# previously from somewhere else: we simply copy them to
#	$out/starting_models/i-tasser-model_$n.pdb
#
if [ ! -s $out/starting_models/i-tasser-model_1.pdb ] ; then

    # Try to generate an homology model using I-TASSER inside
    # the $out directory
    # try as long asthere is not at least one model
    if [ ! -e "$out/i-tasser/$out/model1.pdb" ] ; then
        cd $out
        # run i-tasser on the copy of the sequence
        # LOG is defined as a string in "util_funcs.bash"
        eval i-tasser $sequence $LOG
        cd ..
    else
        notecont "Using already existing I-TASSER predictions"
    fi
    
    # try to copy any generated models to the starting models folder
    # Note: I-TASSER has already ranked the models
    # Note: I-TASSER does not assign chain ID, we'll do it now
    for (( i=1; i <= $maxmodels; i++ )); do
        if [ -s $out/i-tasser/$out/model$i.pdb ] ; then
            #cp $out/i-tasser/$out/model$i.pdb   $out/starting_models/i-tasser-model_$i.pdb
            # add chain ID
            cat $out/i-tasser/$out/model$i.pdb \
            | sed -e '/^ATOM  / s/^\(.\{21\}\) /\1A/' \
            > $out/starting_models/i-tasser-model_$i.pdb
        fi
    done
fi


# ALPHAFOLD
# ---------
# Try to generate an ab-initio model using alphafold inside
# the $out directory
# The same trick as for i-tasser applies if we have pre-existing alphafold
# models: we'll just copy them to
#	$out/starting_models/$out/af_model_$i.pdb
#
if [ ! -s $out/starting_models/$out/af_model_1.pdb ] ; then
    # If alphafold hasn't been run yet there will be no directory
    if [ ! -e $out/alphafold/$out ] ; then
        if [ -x ~/contrib/alphafold/bin/alphafold ] ; then
            cd $out
	        eval alphafold $sequence $LOG
            cd ..
        else
	    warncont "Not using AlphaFold2 because it is not installed"
            #exit 1
        fi
    else
        notecont "Using already existing AlphaFold2 predictions"
    fi

    # copy resulting models (AlphaFold produces up to three sets of varying quality)
    # We prefer the ranked, optimized models whenever possible
    for (( i=0; i < $maxmodels; i++ )); do
        if [ -s $out/starting_models/af_model_$((i + 1)).pdb ] ; then
            continue
        fi
        for m in ranked relaxed unrelaxed ; do
            # try successively less processed models
            if [ -s  $out/alphafold/$out/${m}_$i.pdb ] ; then
                 cat $out/alphafold/$out/${m}_$i.pdb   \
                | sed -e '/^ATOM  / s/^\(.\{21\}\) /\1A/' \
                >  $out/starting_models/af_model_$((i + 1)).pdb
                break
            fi
        done
           
#        if [ -s $out/alphafold/$out/ranked_$i.pdb ] ; then
#            # if alphafold refined and ranked the models, use them
#            # and add chain ID
#            cat $out/alphafold/$out/ranked_$i.pdb   \
#            | sed -e '/^ATOM  / s/^\(.\{21\}\) /\1A/' \
#            >  $out/starting_models/af_model_$((i + 1)).pdb
#        elif [ -s $out/alphafold/$out/relaxed_model_$i.pdb ] ; then
#            # if there were no ranked models, then try again: maybe 
#            # the models could not be ranked by alphafold but 
#            # could be optimized
#            cat $out/alphafold/$out/relaxed_model_$i.pdb   \
#                | sed -e '/^ATOM  / s/^\(.\{21\}\) /\1A/' \
#                >  $out/starting_models/af_model_$((i + 1)).pdb
#        elif [ -s $out/alphafold/$out/unrelaxed_model_$i.pdb ] ; then
#            # if there were no ranked nor optimized models produced,
#            # try again: maybe they could not be optimized but some were still 
#            # generated (we do not care because we will optimize them anyway)
#            cat $out/alphafold/$out/unrelaxed_model_$i.pdb   \
#                | sed -e '/^ATOM  / s/^\(.\{21\}\) /\1A/' \
#                >  $out/starting_models/af_model_$((i + 1)).pdb
#        fi
    done
fi

# DNCON2
# ------
# Ditto.
#
if [ -s $out/starting_models/dncon2_model_1.pdb ] ; then
    echo "Using already existing DNCON2 model(s)"
else
    # dncon2 will create a directory named $out/dncon2 to save its output
    # that's why we start it from above $out (no need to enter into it)
    if [ ! -e $out/dncon2 ] ; then
        notecont "Running DNCON2"
        cd $out
        eval dncon2 $sequence $LOG
        #dncon2 $sequence CONFOLD2	     # works best, and is now the default
        #dncon2 $sequence CONFOLD1       # we considered doing these too
        #dncon2 $sequence UNICON3D       # but they were worse
        cd ..
    else
        notecont "Using already existing DNCON2 predictions (if any)"
    fi

    # the resulting models will be in $out/dncon2/$out
    # dncon2 has already ranked the models
    for (( i=1; i <= $maxmodels; i++ )); do
        if [ -s $out/dncon2/$out/$i.dncon2.*.pdb ] ; then
            # add chain ID
            cat $out/dncon2/$out/$i.dncon2.*.pdb \
            | sed -e '/^ATOM  / s/\(.\{21\}\) /\1A/' \
            > $out/starting_models/dncon2_model_$i.pdb
            # was $out/starting_models/`basename $out/dncon2/$out/$i.dncon2.*.pdb`
        fi
    done

    # fix dncon2 models
    for i in $out/starting_models/*dncon2*.pdb ; do
        if [ ! -s $i ] ; then continue ; fi
        cp $i $i.orig
            if grep -q 'ATOM.................AA' $i.orig ; then
                cat $i.orig \
                | sed -e 's/ AA\([ 0-9]\)/ A \1/' \
                >  $i
            fi
    done
fi

# basic models built with PepBuild
# this is very fast
if isempty $out/pepbuild/$out ; then
    cd $out
    eval pepbuild $sequence $LOG
    cd ..
fi
if ! isempty $out/pepbuild/$out ; then
    cp $out/pepbuild/$out/*pdb $out/starting_models/
fi



######################################################################
#                                                                    #
# Prepare everything for CABS                                        #
#                                                                    #
######################################################################

# we will work inside $out to keep everything contained in a single directory
if [ ! -d $out/cabs ] ; then 
    mkdir -p $out/cabs
fi

# add the sequence if not already there
if [ ! -s $out/cabs/`basename $sequence` ] ; then
    cp $sequence $out/cabs/`basename $sequence`
fi

# Select a suitable contact matrix to be used as distance constrains 
# with CABS
if [ ! -s $out/contactmap.rr5 ] ; then
    if [ -s $out/NeBcon/$out.nebcon.rr ] ; then
        # we prefer NeBcon (as it is more modern and -supposedly- reliable)
        cm=$out/NeBcon/$out.nebcon.rr
    elif [ -s $out/dncon2/$out.rr.raw ] ; then
        # check if dncon2 generated a contact matrix 
        cm=$out/dncon2/$out.rr.raw
    elif [ -s $out/dncon2/freecontact/$out.freecontact.rr ] ; then
        # try with freecontact
        cm=$out/dncon2/freecontact/$out.freecontact.rr
    elif [ -s $out/dncon2/psicov/$out.psicov.rr ] ; then
        # try with psicov
        cm=$out/dncon2/psicov/$out.psicov.rr
    else
        cm=''
        warncont "Could not find a suitable a Contact Matrix $out.rr.raw"
        #exit 1   #comment this line in case you detect scarce alignments 
        #during the execution of dncon2 for your protein
    fi

    cm=`abspath $cm`
    if [ -s "$cm" ] ; then
        ln -s $cm $out/contactmap.rr5	# so we know which one we used
    fi
fi

# copy all starting models to the CABS folder
cp $out/starting_models/*pdb $out/cabs/


######################################################################
#                                                                    #
# Now do a structure refinement using CABS                           #
#                                                                    #
######################################################################


cd $out
# we call it with no structures to refine so that it will refine
# anything that is already inside the $out/cabs directory
eval cabs_cm_00 `basename $sequence` $cm $LOG
cd ..

echo "Scoring starting and CABS-refined models"
if [ ! -d $out/cabs/models_unscored ] ; then 
    mkdir -p $out/cabs/models_unscored
fi
cp $out/cabs/*.pdb $out/cabs/models_unscored

eval apollo $out/cabs/`basename $sequence` $out/cabs/models_unscored $LOG
# this should generate $out/cabs/models_unscored.apollo

# Next, we should copy the best models based on the apollo scores


# This should probably be in the CABS script
#cp */solution-top/*pdb models_unscored
#cp [1-5].*.pdb models_unscored
#apollo $sequence models_unscored

######################################################################
#                                                                    #
# Now do a structure refinement using UNRES                          #
#                                                                    #
######################################################################



######################################################################
#                                                                    #
# Now do a structure refinement using Rosetta                        #
#                                                                    #
######################################################################


######################################################################
#                                                                    #
# Prepare everything for MARTINI                                     #
#                                                                    #
######################################################################

exit		# stop here for now

# Create a directory inside $out where we will run Martini for the 5 best
# best models obtained from the cabs simulation, scored by apollo 
if [ ! -d $out/martini ] ; then 
    mkdir -p $out/martini
fi

#Check if apollo worked and copy the 5 best models to the martini directory
cd $out
if [ -s cabs/${name}.apollo/models_unscored.avg ] ; then
    cat cabs/${name}.apollo/models_unscored.avg \
    | tail -n+5 \
    | head -n5 \
    | while read model score ; do
          if [ -s cabs/models_unscored/$model ] ; then
              cp cabs/models_unscored/$model ./martini
          else
              warncont "$model does not exist or is empty"
          fi
      done
fi
cd ..

# copy the five best models models to the MARTINI folder
#cp $out/best_models/*pdb $out/martini/

######################################################################
#                                                                    #
# Now do a structure refinement using MARTINI                        #
#                                                                    #
######################################################################

cd $out
eval martini $LOG
cd ..


######################################################################
#                                                                    #
# Now do a structure refinement using an atomistic force field       #
#                                                                    #
######################################################################


