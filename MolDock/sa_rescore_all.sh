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
        bash ../../sh/gbsa_hawkins_rescore.sh 
        echo " (done)"
        cd .. 
    done 
    cd .. 
done
