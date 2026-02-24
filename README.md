# MolModTools
A series of tools for Molecular Modelling that I have been developing/using over the last 30+ years.

These tools are a work in progress that is developed as we face novel needs or identify changes to
the base-tools.

Directory *MolMod* is devoted to Macromolecular Structure modeling, in it you will find 
- a series of
driver scripts for many GROMACS tools, which we routinely use to drive Molecular Dynamics simulations,
analyze trajectories, make plots, etc..
- a series of scripts for performing various tasks using UCSF Chimera
- a series of scripts that we use to estimate affinity between proteins and proteins and ligands
  measuring a number of parameters and running MD simulations to perform VERY LIMITED alchemical
  estimates
- a series of scripts to run various protein docking tools (we routinely use several docking
  programs to cover a broader spectrum of poses
- a series of scripts to make _ab-initio_ molecular structure predictions that we developped
  before AlphaFold was publicly available

Directory *Jgnu80web* is a relatively modernized version of 'gnu80web' that compiles with
relatively modern (2000) Fortran compilers.

 Directory *MolDock* contains scripts for protein docking, specially centered on DOCK-6 (but not
 limited to it).

 Directory *ErgoSCF* is a driver script that helps run and explains option choices for the 
 _ergoSCF_ parallel scaling Quantum Mechanics package.

 Directory *FreeON* is a driver script that helps run and explains how to use the _freeON_ linearly
 scaling Quantum Mechanics package.

 Directory *mopac* contains a few scripts that we developed to make movies of frontier orbitals
 from _mopac_ Quantum Dynamics simulations in order to understand the changes in molecular reactivity
 of various NTP (nucleotide-tri-phosphates) when changing from solution to a proteic toxin moiety.
 
