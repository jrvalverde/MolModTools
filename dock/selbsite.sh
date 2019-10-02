
for i in * ; do
    cd $i
    echo $i
    
    if [ -e knl.mol2 ] ; then
    	sphere_selector rec.sph knl.mol2 10.0
    	showsphere <<END
selected_spheres.sph
1
N
selected_spheres.pdb
END
    	ln -s selected_spheres.sph rec_site.sph
    	ln -s selected_spheres.pdb rec_site.pdb

    else
    	showsphere <<END
rec.sph
1
N
selected_spheres.pdb
END
	ln -s selected_spheres.sph rec_site.sph
    	ln -s selected_spheres.pdb rec_site.pdb
    fi

  cd ..
done

