for j in * ; do 
    cd $j
    if [ ! -s rec.sph ] ; then
    	echo -n "$j .. "

	rm -f rec.sph temp* OUTSPH
    	for i in *.dms ; do
    	    cat > INSPH<<END
$i
R
X
0.0
4.0
1.4
rec.sph
`basename $i .dms`.sph
END
    	    jrsphgen 
#    	    mv OUTSPH `basename $i .dms`.OUTSPH
    	done

    	echo "done"
    fi
    cd ..
done &> ../log
