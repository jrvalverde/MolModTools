#!/bin/bash
#
#	Utility functions
#
#
# AUTHOR(S)
#	'banner' is based on the bash/ksh93 version of 'banner' by jlliagre
#		Apr 15 '12 at 11:52
#		http://stackoverflow.com/questions/652517/whats-the-deal-with-the-banner-command
#
#	All other code is
#		(C) Jos´e R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es 2014
#	and
#		Licensed under (at your option) either GNU/GPL or EUPL
#
# LICENSE:
#
#	Copyright 2014 JOSE R VALVERDE, CNB/CSIC.
#
#	EUPL
#
#       Licensed under the EUPL, Version 1.1 or \u2013 as soon they
#       will be approved by the European Commission - subsequent
#       versions of the EUPL (the "Licence");
#       You may not use this work except in compliance with the
#       Licence.
#       You may obtain a copy of the Licence at:
#
#       http://ec.europa.eu/idabc/eupl
#
#       Unless required by applicable law or agreed to in
#       writing, software distributed under the Licence is
#       distributed on an "AS IS" basis,
#       WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
#       express or implied.
#       See the Licence for the specific language governing
#       permissions and limitations under the Licence.
#
#	GNU/GPL
#
#       This program is free software: you can redistribute it and/or modify
#       it under the terms of the GNU General Public License as published by
#       the Free Software Foundation, either version 3 of the License, or
#       (at your option) any later version.
#       
#       This program is distributed in the hope that it will be useful,
#       but WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#       GNU General Public License for more details.
#       
#       You should have received a copy of the GNU General Public License
#       along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#
# Includes
#
# Checks for definition $COMMON, if not set sources bash script with all common variables
#
if [ -z ${COMMON+x} ]; then 
    . gromacs_funcs_common.bash
fi

function make_md_01_config {
    #	Takes as argument the number of groups, either 1 or 2
    #	We have two groups when we solvate the system, and we
    #	must heat the solvent but not the protein
    #
    local tau_t=''
    local comm_grps=''
    local tc_grps=''
    local ref_t=''
    if [ "$1" = "2" ] ; then
    # There will be two simulation groups, Solvent and Solute
        tau_t='0.1 0.1'
        # comm_grps might lead to conflicts between protein/non-protein
        #	it would possibly be better if it were protein+ligand solvent
        comm_grps='Solute Solvent'
        tc_grps='Solute Solvent'
        e_grps='Solute Solvent'
        ref_t='300 300'
    else
        tau_t='0.1'
        comm_grps='System'
        tc_grps='System'
        e_grps=''
        ref_t='300'
    fi

    cat <<EOF >| md_01.mdp
; MD PRODUCTION RUN

; RUN CONTROL PARAMETERS
integrator              = md            ; md integrator
; Start time and timestep in ps
tinit                   = 0             ; [ps] starting time for run
dt                      = 0.001         ; [ps] time step for integration
nsteps                  = 1000000	; maximum number of steps to integrate, 0.001 * 1,000,000 = 1,000 ps
; For exact run continuation or redoing part of a run
init_step                = 0
continuation		= yes		; Restarting after NPT 
; Part index is updated automatically on checkpointing (keeps files separate)
;simulation_part          = 1

; control center of mass motion
comm_mode               = Linear        ; remove center of mass translation
nstcomm                 = 1             ; [steps] frequency of center of mass motion removal
comm_grps               = $comm_grps	; group(s) for center of mass motion removal

; LANGEVIN DYNAMICS OPTIONS
; Friction coefficient (amu/ps) and random seed
bd-fric                  = 0
;ld-seed                  = 1963

; OUTPUT CONTROL OPTIONS
; Output frequency for coords (x), velocities (v) and forces (f)
nstxout                 = 100          ; [steps] freq to write coordinates to trajectory
nstvout                 = 100          ; [steps] freq to write velocities to trajectory
nstfout                 = 100          ; [steps] freq to write forces to trajectory
; Output frequency for energies to log file and energy file
nstlog                  = 100          ; [steps] freq to write energies to log file
nstcalcenergy           = 100          ; [steps] freq to calculate energies
nstenergy               = 100          ; [steps] freq to write energies to energy file
; Output frequency and precision for .xtc file
nstxtcout               = 100          ; [steps] freq to write coordinates to xtc trajectory
xtc_precision           = 100          ; [real] precision to write xtc trajectory
; This selects the subset of atoms for the .xtc file. You can
; select multiple groups. By default all atoms will be written.
xtc_grps                = System       ; group(s) to write to xtc trajectory
; Selection of energy groups
energygrps              = $e_grps      ; group(s) to write to energy file

; NEIGHBORSEARCHING PARAMETERS
;cutoff-scheme = (verlet or group)
; nblist update frequency
nstlist                 = 1             ; [steps] freq to update neighbor list
; ns algorithm (simple, or grid)
ns-type                 = grid          ; update nb using neighbour grid cells only
; Periodic boundary conditions: xyz, no, xy
pbc                     = xyz           ; periodic boundary conditions in all directions
periodic_molecules       = no
; nblist cut-off        
rlist                    = 1.6		; 1.3-1.5 for opls-aa 
; long-range cut-off for switched potentials
;;rlistlong                = -1
rlistlong               = -1           ; [nm] Cut-off distance for the long-range neighbor list. This parameter is only relevant for a twin-range cut-off setup with switched potentials
				       ; provides additional size over rlist to allow the largest sized charge group to be considered

; OPTIONS FOR ELECTROSTATICS AND VDW
; Method for doing electrostatics
; 	Electrostatics
coulombtype             = PME-Switch    ; Particle-Mesh Ewald electrostatics
;;rcoulomb                = 1.1           ; [nm] distance for Coulomb cut-off
coulomb-modifier         = Potential-shift-Verlet
rcoulomb                = 0.9           ; [nm] distance for Coulomb cut-off
;;rcoulomb-switch         = 0
rcoulomb-switch         = 0.8
; Relative dielectric constant for the medium and the reaction field
epsilon_r                = 1
epsilon_rf               = 1

; 	VdW
vdwtype                 = shift        ; twin-range shift with rlist where rvdw >= rlist
vdw-modifier             = Potential-shift-Verlet
rvdw                    = 1.2           ; [nm] distance for LJ cut-off
rvdw-switch             = 1.0		; JCTC, Siu, Pluhackova, B¨ockmann 2012
; Apply long range dispersion corrections for Energy and Pressure
DispCorr                = EnerPres      ; apply long range dispersion corrections for energy
; Extension of the potential lookup tables beyond the cut-off
table-extension          = 1
; Seperate tables between energy group pairs
;energygrp_table          = $e_grps

;	Spacing for the PME/PPPM FFT grid
fourierspacing          = 0.12          ; [nm] grid spacing for FFT grid when using PME
; FFT grid size, when a value is 0 fourierspacing will be used
fourier_nx               = 0
fourier_ny               = 0
fourier_nz               = 0

; 	EWALD/PME/PPPM parameters
pme_order               = 4             ; interpolation order for PME, 4 = cubic
ewald_rtol              = 1e-5          ; relative strength of Ewald-shifted potential at rcoulomb
ewald_geometry           = 3d
epsilon_surface          = 0
optimize_fft             = yes

; IMPLICIT SOLVENT ALGORITHM
implicit_solvent         = No

; OPTIONS FOR WEAK COUPLING ALGORITHMS
; 	Temperature coupling  
;tcoupl                 = nose-hoover    ; temperature coupling with Nose-Hoover ensemble
;tcoupl			= berendsen	 ; Berendsen thermostat
tcoupl                  = V-rescale	 ; modified Berendsen thermostat
; Groups to couple separately
tc_grps                 = $tc_grps	; groups to couple seperately to temperature bath
; Time constant (ps) and reference temperature (K)
tau_t                   = $tau_t        ; [ps] time constant for coupling
ref_t                   = $ref_t        ; [K] reference temperature for coupling

; 	Pressure Coupling
pcoupl                  = Parrinello-Rahman     ; pressure coupling where box vectors are variable
pcoupltype              = isotropic             ; pressure coupling in x-y-z directions
nstpcouple              = 10			; -1 = nstlist
; Time constant (ps), compressibility (1/bar) and reference P (bar)
tau_p                   = 2.0                   ; [ps] time constant for coupling
;;tau_p                   = 0.5			; [ps] time constant for coupling
ref_p                   = 1.0                   ; [bar] reference pressure for coupling
compressibility         = 4.5e-5                ; [bar^-1] compressibility

; Scaling of reference coordinates, No, All or COM (Center Of Mass)
;   contrains particle movement away from reference coordinates
;refcoord_scaling         = No
refcoord_scaling        = com
; Random seed for Andersen thermostat
;andersen_seed            = 19631008

; GENERATE VELOCITIES FOR STARTUP RUN
gen_vel                 = no            ; velocity generation turned off
;gen_temp                = $ref_t        ; [K] temperature for Maxwell distribution
;gen_seed                = -1            ; [integer] used to initialize random generator for random velocities
					; we start from equilibration velocities
; OPTIONS FOR BONDS    
constraints             = all-bonds     ; convert all bonds to constraints
; Type of constraint algorithm
constraint-algorithm    = LINCS         ; LINear Constraint Solver
; Use successive overrelaxation to reduce the number of shake iterations
Shake-SOR                = no
; Relative tolerance of shake
shake-tol                = 0.0001
; Highest order in the expansion of the constraint coupling matrix
lincs_order             = 4             ; highest order in the expansion of the contraint coupling matrix
; Number of iterations in the final step of LINCS. 1 is fine for
; normal simulations, but use 2 to conserve energy in NVE runs.
; For energy minimization with constraints it should be 4 to 8.
;;lincs_iter              = 1             ; number of iterations to correct for rotational lengthening
lincs-iter             = 2
; Lincs will write a warning to the stderr if in one step a bond
; rotates over more degrees than
lincs_warnangle         = 30            ; [degrees] maximum angle that a bond can rotate before LINCS will complain
; Convert harmonic bonds to morse potentials
morse                    = no

EOF

}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    groups=$1
    if [ "$UNIT_TEST" == "no" ] ; then
	make_md_01_config $groups
    else
	make_md_01_config
    fi
fi
