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

function make_eq_npt_200ps_config {
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
        comm_grps='Solute Solvent'
        tc_grps='Solute Solvent'
        ref_t='300 300'
    else
        tau_t='0.1'
        comm_grps='System'
        tc_grps='System'
        ref_t='300'
    fi

    # Create equilibration run NPT file
    cat <<EOF >| eq_npt_200ps.mdp
; NPT EQUILIBRATION
; Following equilibration to a target temperature, you must next relax your 
; system into a constant pressure ensemble. 
; This parameter file calls for a run of 250,000 steps with a 2 fs timestep 
; (a total of 500 ps). Position restraints are still being applied to the 
; backbone. The Berendsen thermostat has been replaced with a Nose-Hoover 
; thermostat, which will produce a more correct ensemble of kinetic energies. 
; The Nose-Hoover thermostat, however, must be implemented only after the 
; system is already near its target temperature (which was achieved by the 
; Berendsen thermostat), or the kinetic energy of the system will fluctuate 
; wildly. Because this run will be a continuation of the previous run, 
; 'gen_vel' has been set to 'no'. Finally, the Parrinello-Rahman method is 
; used to couple pressure isotropically (the same in all directions) to a 
; value of 1.0 bar.

; 7.3.2 Preprocessing
define                  = -DPOSRES      ; position restrain the protein

; 7.3.3 Run Control
continuation		= yes		 ; Restarting after NVT
integrator              = md             ; md integrator
tinit                   = 0              ; [ps] starting time for run
dt                      = 0.002          ; [ps] time step for integration
nsteps                  = 100000         ; maximum number of steps to integrate, 0.002 * 100,000 = 200 ps
comm_mode               = Linear         ; remove center of mass translation
nstcomm                 = 1              ; [steps] frequency of mass motion removal
comm_grps               = $comm_grps     ; group(s) for center of mass motion removal
;continuation            = no            ; no = apply constraints to the start configuration

; 7.3.8 Output Control
nstxout                 = 25000         ; [steps] freq to write coordinates to trajectory
nstvout                 = 25000         ; [steps] freq to write velocities to trajectory
nstfout                 = 25000         ; [steps] freq to write forces to trajectory
nstlog                  = 100           ; [steps] freq to write energies to log file
nstenergy               = 100           ; [steps] freq to write energies to energy file
nstxtcout               = 100           ; [steps] freq to write coordinates to xtc trajectory
xtc_precision           = 1000          ; [real] precision to write xtc trajectory
xtc_grps                = System        ; group(s) to write to xtc trajectory
energygrps              = System        ; group(s) to write to energy file

; 7.3.9 Neighbor Searching
nstlist                 = 1             ; [steps] freq to update neighbor list
ns_type                 = grid          ; method of updating neighbor list
pbc                     = xyz           ; periodic boundary conditions in all directions
rlist                   = 1.0           ; [nm] cut-off distance for the short-range neighbor list

; 7.3.10 Electrostatics
coulombtype             = PME           ; Particle-Mesh Ewald electrostatics
rcoulomb                = 1.0           ; [nm] distance for Coulomb cut-off

; 7.3.11 VdW
vdwtype                 = cut-off       ; twin-range cut-off with rlist where rvdw >= rlist
rvdw                    = 1.0           ; [nm] distance for LJ cut-off
DispCorr                = EnerPres      ; apply long range dispersion corrections

; 7.3.13 Ewald
fourierspacing          = 0.12          ; [nm] grid spacing for FFT grid when using PME
pme_order               = 4             ; interpolation order for PME, 4 = cubic
ewald_rtol              = 1e-5          ; relative strength of Ewald-shifted potential at rcoulomb

; 7.3.14 Temperature Coupling
tcoupl                  = berendsen    ; temperature coupling with Berendsen-thermostat
;tcoupl			= V-rescale     ; modified Berendsen thermostat
tc_grps                 = $tc_grps      ; groups to couple seperately to temperature bath
tau_t                   = $tau_t	; [ps] time constant for coupling
ref_t                   = $ref_t	; [K] reference temperature for coupling

; Pressure Coupling
pcoupl			= Parrinello-Rahman	; Pressure coupling on in NPT
pcoupltype		= isotropic		; uniform scaling of box vectors
tau_p			= 2.0			; time constant, in ps
ref_p			= 1.0			; reference pressure, in bar
compressibility 	= 4.5e-5		; isothermal compressibility of water, bar^-1
refcoord_scaling 	= com

; 7.3.17 Velocity Generation
;	OFF (continuation run)
gen_vel                 = no           ; generate velocities according to Maxwell distribution of temperature
;gen_temp                = 310           ; [K] temperature for Maxwell distribution
;gen_seed                = -1            ; [integer] used to initialize random generator for random velocities

; 7.3.18 Bonds
constraints             = all-bonds     ; convert all bonds to constraints
constraint_algorithm    = LINCS         ; LINear Constraint Solver
lincs_order             = 4             ; highest order in the expansion of the contraint coupling matrix
lincs_iter              = 1             ; number of iterations to correct for rotational lengthening
lincs_warnangle         = 30            ; [degrees] maximum angle that a bond can rotate before LINCS will complain
EOF
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    $groups$1
    if [ "$UNIT_TEST" == "no" ] ; then
	make_eq_npt_200ps_config $groups
    else
	make_eq_npt_200ps_config
    fi
fi

