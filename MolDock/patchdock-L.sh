#!/bin/bash
#
#export PATCH_DOCK=/opt/structure/PatchDock
#export FIRE_DOCK=/opt/structure/FireDock
export PATCH_DOCK=~/PatchDock
export FIRE_DOCK=~/FireDock

cd `dirname $1`

if [ -e PatchDock-L/DONE ] ; then exit ; fi

if [ ! -d PatchDock-L ] ; then mkdir PatchDock-L ; fi

cd PatchDock-L

if [ ! -e model_l.pdb ] ; then ln -s $1 model_l.pdb ; fi
if [ ! -e model_r.pdb ] ; then ln -s $1 model_r.pdb ; fi

if [ ! -e DONEPD ] ; then
    # Do PtachDock docking if not done yet (or not completed)
    # NOTE: right now we do not check for erroneous termination

    # Create parameter file
    $PATCH_DOCK/buildParams.pl model_r.pdb model_l.pdb 

    # Create molecular surfaces
    $PATCH_DOCK/buildMS.pl model_r.pdb model_l.pdb 

    # Add our own known contrains ("active site" residues)
    cat > site.txt <<END
155 A
159 A
251 A
END

    echo "# known interacting amino acids (this is a dimer)" >> params.txt
    echo "receptorActiveSite site.txt" >> params.txt
    echo "ligandActiveSite site.txt" >> params.txt

    # Dock
    $PATCH_DOCK/patch_dock.Linux params.txt output > std.log 2>&1

    # Generate models
    $PATCH_DOCK/transOutput.pl output 1 100

    mkdir models
    for i in {1..100} ; do 
        if [ -e output.$i.pdb ] ; then 
            mv output.$i.pdb models/output-`printf %03d $i`.pdb
        fi
    done

    touch DONEPD
fi


if [ ! -e DONEFD ] ; then
    # Do FireDock refinement if not done yet or not completed
    # NOTE: right now we do not check for erroneous termination

    if [ ! -d FireDock ] ; then mkdir FireDock; fi

    cd FireDock

    if [ ! -e model_l.pdb ] ; then ln -s ../model_l.pdb . ; fi
    if [ ! -e model_r.pdb ] ; then ln -s ../model_r.pdb . ; fi

    export LD_LIBRARY_PATH=$FIRE_DOCK/lib:$LD_LIBRARY_PATH

    # Prepare PDB files for refinement
    $FIRE_DOCK/preparePDBs.pl model_r.pdb model_l.pdb All
    

    #$FIRE_DOCK/addHydrogens.pl model_r.pdb model_l.pdb

    # Translate PatchDock output to FireDock input
    if [ ! -e output ] ; then ln -s ../output . ; fi
    $FIRE_DOCK/PatchDockOut2Trans.pl output > output.trans

    # run coarse refinement (RISCO, 50 cycles of RBO and radiiScale=0.8)
    $FIRE_DOCK/buildFireDockParams.pl model_r.pdb.CHB.pdb model_l.pdb.CHB.pdb U U Default output.trans coarse 0 50 0.8 0 coarse.txt

    if [ ! -e site.txt ] ; then ln -s ../site.txt . ; fi
    echo "# known interacting amino acids (this is a dimer)" >> coarse.txt
    echo "receptorFixedResiduesFile site.txt" >> coarse.txt
    echo "ligandFixedResiduesFile site.txt" >> coarse.txt 

    $FIRE_DOCK/runFireDock.pl coarse.txt &> fdcoarse.log

    # get 25 top ranking complexes: first get complexes and sort them by global score
    grep "|" coarse.ref | sort -nk6 -t"|" | head -n 25 > coarse-top-25.sort

    # run the full refinement stage (FISCO, 50 cycles of RBO and radiiScale=0.85)
    $FIRE_DOCK/buildFireDockParams.pl model_r.pdb.CHB.pdb model_l.pdb.CHB.pdb U U Default coarse-top-25.sort refined 1 50 0.85 0 refined.txt

    echo "# known interacting amino acids (this is a dimer)" >> refined.txt
    echo "receptorFixedResiduesFile site.txt" >> refined.txt
    echo "ligandFixedResiduesFile site.txt" >> refined.txt 

    $FIRE_DOCK/runFireDock.pl refined.txt &> fdrefined.log

    # choose the final top 10 hypothesis
    grep "|" refined.ref | sort -nk6 -t"|" | head -n 10 > refined-top-10.sort

    # run full side-chain optimization again, without RBO) and print the complexes
    $FIRE_DOCK/buildFireDockParams.pl model_r.pdb.CHB.pdb model_l.pdb.CHB.pdb U U Default refined-top-10.sort solution 1 0 0.85 1 final.txt

    echo "# known interacting amino acids (this is a dimer)" >> final.txt
    echo "receptorFixedResiduesFile site.txt" >> final.txt
    echo "ligandFixedResiduesFile site.txt" >> final.txt 

    $FIRE_DOCK/runFireDock.pl final.txt &> fdfinal.log

    cd ..
    touch DONEFD

fi

touch DONE

cd ..
