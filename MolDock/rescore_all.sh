for i in 8ogtp gtp Mg8OGTP-- MgGTP-- ; do 
    echo "Entering $i" 
    cd $i 
    for j in * ; do 
        echo "$i/$j :" 
        cd $j 
        echo -n "    pbsa"
        bash ../../sh/pbsa_rescore.sh 
        echo " (done)"
        echo -n "    Hawkins GB/SA"
        bash ../../sh/gbsa_hawkins.sh 
        echo " (done)"
        echo -n "    DSX"
        bash ../../sh/dsx_rescore.sh 
        echo " (done)"
        echo -n "    score"
        bash ../../sh/score_rescore.sh 
        echo " (done)"
        echo -n "    Xscore"
        bash ../../sh/xscore_rescore.sh 
        echo " (done)"
        cd .. 
    done 
    cd .. 
done
