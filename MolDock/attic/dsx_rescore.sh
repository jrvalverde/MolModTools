if [ ! -d dsx ] ; then
    mkdir -p dsx
fi
cd dsx
ln -s ../receptor.mol2 .
ln -s ../known.mol2 .

for i in \
	../flex-*_secondary_conformers.mol2 \
        ../flex-*_primary_conformers.mol2 \
        ../flexible_scored.mol2 \
        ../rigid_scored.mol2 \
        ../rec+lig_*.mol2 \
        ../rigid_scored.mol2 ; do
    # if nothing matches a wildcard, the wilcard is passed as argument
    if [ ! -e "$i" ] ; then continue ; fi
    if [ -s "$i" ] ; then
    	rm -f docked.mol2
    	ln -s $i docked.mol2
	nam=`basename $i .mol2`
        # ligand
	~/contrib/dsx/linux64/dsx_linux_64.lnx \
	    -P receptor.mol2 -L docked.mol2 -R known.mol2 \
	    -I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 1 \
	    -T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
	mv DSX_receptor_docked.txt ${nam}_DSX_receptor_docked.tot.txt
	~/contrib/dsx/linux64/dsx_linux_64.lnx \
	    -P receptor.mol2 -L docked.mol2 -R known.mol2 \
	    -I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 4 \
	    -T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
	mv DSX_receptor_docked.txt ${nam}_DSX_receptor_docked.rmsd.txt
	~/contrib/dsx/linux64/dsx_linux_64.lnx \
	    -P receptor.mol2 -L docked.mol2 -R known.mol2 \
	    -I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 0 \
	    -T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
	mv DSX_receptor_docked.txt ${nam}_DSX_receptor_docked.txt
	# known
	~/contrib/dsx/linux64/dsx_linux_64.lnx \
	    -P receptor.mol2 -L known.mol2 -R known.mol2 \
	    -I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 1 \
	    -T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
	mv DSX_receptor_known.txt ${nam}_DSX_receptor_known.tot.txt
	~/contrib/dsx/linux64/dsx_linux_64.lnx \
	    -P receptor.mol2 -L known.mol2 -R known.mol2 \
	    -I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 4 \
	    -T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
	mv DSX_receptor_known.txt ${nam}_DSX_receptor_known.rmsd.txt
	~/contrib/dsx/linux64/dsx_linux_64.lnx \
	    -P receptor.mol2 -L known.mol2 -R known.mol2 \
	    -I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 0 \
	    -T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
	mv DSX_receptor_known.txt ${nam}_DSX_receptor_known.txt

    fi
done

exit

### THIS IS IGNORED ###

~/contrib/dsx/linux64/dsx_linux_64.lnx \
	-P receptor.mol2 -L docked.mol2 -R known.mol2 \
	-I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 1 \
	-T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c

mv DSX_receptor_docked.txt DSX_receptor_docked.tot.txt

~/contrib/dsx/linux64/dsx_linux_64.lnx \
	-P receptor.mol2 -L docked.mol2 -R known.mol2 \
	-I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 4 \
	-T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c

mv DSX_receptor_docked.txt DSX_receptor_docked.rmsd.txt

~/contrib/dsx/linux64/dsx_linux_64.lnx \
	-P receptor.mol2 -L docked.mol2 -R known.mol2 \
	-I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 0 \
	-T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c

####
	
~/contrib/dsx/linux64/dsx_linux_64.lnx \
	-P receptor.mol2 -L known.mol2 -R known.mol2 \
	-I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 1 \
	-T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c

mv DSX_receptor_known.txt DSX_receptor_known.tot.txt

~/contrib/dsx/linux64/dsx_linux_64.lnx \
	-P receptor.mol2 -L known.mol2 -R known.mol2 \
	-I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 4 \
	-T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c

mv DSX_receptor_known.txt DSX_receptor_known.rmsd.txt

~/contrib/dsx/linux64/dsx_linux_64.lnx \
	-P receptor.mol2 -L known.mol2 -R known.mol2 \
	-I 0 -D ~/contrib/dsx/pdb_pot_0511/ -o -v -S 0 \
	-T0 1.0 -T1 1.0 -T2 0.0 -T3 1.0 -c
	
