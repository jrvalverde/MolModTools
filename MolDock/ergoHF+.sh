#!/bin/bash

export PATH=$HOME/contrib/ergoSCF/bin:$PATH

xyz=`basename $1`
name=`basename $xyz .xyz`

initial_basis="STO-3G"
refined_basis="aug-cc-pVDZ"
#refined_basis="6-311++Gss"

##unused
#guess_input="
#basis = \"$initial_basis\";
#use_simple_starting_guess=1;
#J_K.use_fmm = 1;
#J_K.threshold_2el_J = 1e-6;
#J_K.threshold_2el_K = 1e-6;
#J_K.fmm_box_size = 4.4;
#scf.convergence_threshold = 1e-2;
#run \"HF\";
#"

echo
echo "Computing $1 HF/$initial_basis + $refined_basis"
echo

pushd `pwd` > /dev/null
cd `dirname $1`

if [ -e $name.$refined_basis ] ; then 
    if [ ! -d $name.$refined_basis ] ; then
	echo "a file named $name.$refined_basis already exists"
	exit
    fi
else
	mkdir $name.$refined_basis
fi
cd $name.$refined_basis

if [ ! -e $xyz ] ; then ln -s ../$xyz . ; fi

if [ ! -e $name.$initial_basis.density.bin ] ; then
  date -u 
  echo "  getting starting guess with $initial_basis..."
  cat > $name.$initial_basis.prm <<END
	basis = "$initial_basis";
	use_simple_starting_guess=1;
	J_K.use_fmm = 1;
	J_K.threshold_2el_J = 1e-6;
	J_K.threshold_2el_K = 1e-6;
	J_K.fmm_box_size = 4.4;
	scf.convergence_threshold = 1e-2;
	scf.output_homo_and_lumo_eigenvectors = 1
	spin_polarization = 1
	run "HF";
END
  ergo -m $xyz < $name.$initial_basis.prm
  mv ergoscf.out $name.$initial_basis.ergoscf
  mv density.bin $name.$initial_basis.density.bin
fi

if [ ! -e $name.$refined_basis.ergoscf ] ; then
    date -u
    echo "  running UHF-CI/$refined_basis"
    cat <<EOINPUT > $name.$refined_basis.prm
#	no_initial_density = "$name.$initial_basis.density.bin"
	basis = "$refined_basis"
	charge = 0
	do_ci_after_scf = 0
	enable_memory_usage_output = 1
	spin_polarization = 1
	use_6_d_functions = 1
	scf.calculation_identifier="$name"
	scf.convergence_threshold = 1e-6
	scf.create_mtx_files_dipole = 1
	scf.create_mtx_files_D = 1
	scf.create_mtx_files_F = 1
	scf.create_mtx_file_S = 1
	scf.output_homo_and_lumo_eigenvectors = 1
	scf.force_unrestricted = 1
	scf.save_final_potential = 1
	scf.output_mulliken_pop = 1
	scf.write_overlap_matrix = 1
	run "HF"
EOINPUT
ergo -m $xyz < $name.$refined_basis.prm

    grep CONVERGED ergoscf.out

    mv ergoscf.out $name.$refined_basis.ergoscf
    if [ -e density.bin ] ; then 
    	mv density.bin $name.$refined_basis.density.bin 
    fi

    date -u
fi

cd ..

echo
echo $1 Hartree-Fock calculation completed successfully!
echo

popd

exit
