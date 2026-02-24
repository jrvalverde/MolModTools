#!/bin/bash
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

#
# NAME
# 	banner - print large banner
#
# SYNOPSIS
#	banner text
#
# DESCRIPTION
#	banner prints out the first 10 characters of "text" in large letters
#

function banner {
    #
    # Taken from http://stackoverflow.com/questions/652517/whats-the-deal-with-the-banner-command
    #
    #	Msg by jlliagre
    #		Apr 15 '12 at 11:52
    #
    # ### JR ###
    #	Input:	A text up to 10 letter wide
    # This has been included because banner(1) is no longer a standard
    # tool in many Linux systems. This way we avoid having a dependency
    # that might not be met.
    # It is often installed through package 'sysvbanner'
    #	npm has an ascii-banner tool (npm -g install ascii-banner)
    # Other alternatives are toilet(1) and figlet(1)

    typeset A=$((1<<0))
    typeset B=$((1<<1))
    typeset C=$((1<<2))
    typeset D=$((1<<3))
    typeset E=$((1<<4))
    typeset F=$((1<<5))
    typeset G=$((1<<6))
    typeset H=$((1<<7))

    function outLine
    {
      typeset r=0 scan
      for scan
      do
        typeset l=${#scan}
        typeset line=0
        for ((p=0; p<l; p++))
        do
          line="$((line+${scan:$p:1}))"
        done
        for ((column=0; column<8; column++))
          do
            [[ $((line & (1<<column))) == 0 ]] && n=" " || n="#"
            raw[r]="${raw[r]}$n"
          done
          r=$((r+1))
        done
    }

    function outChar
    {
        case "$1" in
        (" ") outLine "" "" "" "" "" "" "" "" ;;
        ("0") outLine "BCDEF" "AFG" "AEG" "ADG" "ACG" "ABG" "BCDEF" "" ;;
        ("1") outLine "F" "EF" "F" "F" "F" "F" "F" "" ;;
        ("2") outLine "BCDEF" "AG" "G" "CDEF" "B" "A" "ABCDEFG" "" ;;
        ("3") outLine "BCDEF" "AG" "G" "CDEF" "G" "AG" "BCDEF" "" ;;
        ("4") outLine "AF" "AF" "AF" "BCDEFG" "F" "F" "F" "" ;;
        ("5") outLine "ABCDEFG" "A" "A" "ABCDEF" "G" "AG" "BCDEF" "" ;;
        ("6") outLine "BCDEF" "A" "A" "BCDEF" "AG" "AG" "BCDEF" "" ;;
        ("7") outLine "BCDEFG" "G" "F" "E" "D" "C" "B" "" ;;
        ("8") outLine "BCDEF" "AG" "AG" "BCDEF" "AG" "AG" "BCDEF" "" ;;
        ("9") outLine "BCDEF" "AG" "AG" "BCDEF" "G" "G" "BCDEF" "" ;;
        ("a") outLine "" "" "BCDE" "F" "BCDEF" "AF" "BCDEG" "" ;;
        ("b") outLine "B" "B" "BCDEF" "BG" "BG" "BG" "ACDEF" "" ;;
        ("c") outLine "" "" "CDE" "BF" "A" "BF" "CDE" "" ;;
        ("d") outLine "F" "F" "BCDEF" "AF" "AF" "AF" "BCDEG" "" ;;
        ("e") outLine "" "" "BCDE" "AF" "ABCDEF" "A" "BCDE" "" ;;
        ("f") outLine "CDE" "B" "B" "ABCD" "B" "B" "B" "" ;;
        ("g") outLine "" "" "BCDEG" "AF" "AF" "BCDE" "F" "BCDE" ;;
        ("h") outLine "B" "B" "BCDE" "BF" "BF" "BF" "ABF" "" ;;
        ("i") outLine "C" "" "BC" "C" "C" "C" "ABCDE" "" ;;
        ("j") outLine "D" "" "CD" "D" "D" "D" "AD" "BC" ;;
        ("k") outLine "B" "BE" "BD" "BC" "BD" "BE" "ABEF" "" ;;
        ("l") outLine "AB" "B" "B" "B" "B" "B" "ABC" "" ;;
        ("m") outLine "" "" "ACEF" "ABDG" "ADG" "ADG" "ADG" "" ;;
        ("n") outLine "" "" "BDE" "BCF" "BF" "BF" "BF" "" ;;
        ("o") outLine "" "" "BCDE" "AF" "AF" "AF" "BCDE" "" ;;
        ("p") outLine "" "" "ABCDE" "BF" "BF" "BCDE" "B" "AB" ;;
        ("q") outLine "" "" "BCDEG" "AF" "AF" "BCDE" "F" "FG" ;;
        ("r") outLine "" "" "ABDE" "BCF" "B" "B" "AB" "" ;;
        ("s") outLine "" "" "BCDE" "A" "BCDE" "F" "ABCDE" "" ;;
        ("t") outLine "C" "C" "ABCDE" "C" "C" "C" "DE" "" ;;
        ("u") outLine "" "" "AF" "AF" "AF" "AF" "BCDEG" "" ;;
        ("v") outLine "" "" "AG" "BF" "BF" "CE" "D" "" ;;
        ("w") outLine "" "" "AG" "AG" "ADG" "ADG" "BCEF" "" ;;
        ("x") outLine "" "" "AF" "BE" "CD" "BE" "AF" "" ;;
        ("y") outLine "" "" "BF" "BF" "BF" "CDE" "E" "BCD" ;;
        ("z") outLine "" "" "ABCDEF" "E" "D" "C" "BCDEFG" "" ;;
        ("A") outLine "D" "CE" "BF" "AG" "ABCDEFG" "AG" "AG" "" ;;
        ("B") outLine "ABCDE" "AF" "AF" "ABCDE" "AF" "AF" "ABCDE" "" ;;
        ("C") outLine "CDE" "BF" "A" "A" "A" "BF" "CDE" "" ;;
        ("D") outLine "ABCD" "AE" "AF" "AF" "AF" "AE" "ABCD" "" ;;
        ("E") outLine "ABCDEF" "A" "A" "ABCDE" "A" "A" "ABCDEF" "" ;;
        ("F") outLine "ABCDEF" "A" "A" "ABCDE" "A" "A" "A" "" ;;
        ("G") outLine "CDE" "BF" "A" "A" "AEFG" "BFG" "CDEG" "" ;;
        ("H") outLine "AG" "AG" "AG" "ABCDEFG" "AG" "AG" "AG" "" ;;
        ("I") outLine "ABCDE" "C" "C" "C" "C" "C" "ABCDE" "" ;;
        ("J") outLine "BCDEF" "D" "D" "D" "D" "BD" "C" "" ;;
        ("K") outLine "AF" "AE" "AD" "ABC" "AD" "AE" "AF" "" ;;
        ("L") outLine "A" "A" "A" "A" "A" "A" "ABCDEF" "" ;;
        ("M") outLine "ABFG" "ACEG" "ADG" "AG" "AG" "AG" "AG" "" ;;
        ("N") outLine "AG" "ABG" "ACG" "ADG" "AEG" "AFG" "AG" "" ;;
        ("O") outLine "CDE" "BF" "AG" "AG" "AG" "BF" "CDE" "" ;;
        ("P") outLine "ABCDE" "AF" "AF" "ABCDE" "A" "A" "A" "" ;;
        ("Q") outLine "CDE" "BF" "AG" "AG" "ACG" "BDF" "CDE" "FG" ;;
        ("R") outLine "ABCD" "AE" "AE" "ABCD" "AE" "AF" "AF" "" ;;
        ("S") outLine "CDE" "BF" "C" "D" "E" "BF" "CDE" "" ;;
        ("T") outLine "ABCDEFG" "D" "D" "D" "D" "D" "D" "" ;;
        ("U") outLine "AG" "AG" "AG" "AG" "AG" "BF" "CDE" "" ;;
        ("V") outLine "AG" "AG" "BF" "BF" "CE" "CE" "D" "" ;;
        ("W") outLine "AG" "AG" "AG" "AG" "ADG" "ACEG" "BF" "" ;;
        ("X") outLine "AG" "AG" "BF" "CDE" "BF" "AG" "AG" "" ;;
        ("Y") outLine "AG" "AG" "BF" "CE" "D" "D" "D" "" ;;
        ("Z") outLine "ABCDEFG" "F" "E" "D" "C" "B" "ABCDEFG" "" ;;
        (".") outLine "" "" "" "" "" "" "D" "" ;;
        (",") outLine "" "" "" "" "" "E" "E" "D" ;;
        (":") outLine "" "" "" "" "D" "" "D" "" ;;
        ("!") outLine "D" "D" "D" "D" "D" "" "D" "" ;;
        ("/") outLine "G" "F" "E" "D" "C" "B" "A" "" ;;
        ("\\") outLine "A" "B" "C" "D" "E" "F" "G" "" ;;
        ("|") outLine "D" "D" "D" "D" "D" "D" "D" "D" ;;
        ("+") outLine "" "D" "D" "BCDEF" "D" "D" "" "" ;;
        ("-") outLine "" "" "" "BCDEF" "" "" "" "" ;;
        ("*") outLine "" "BDF" "CDE" "D" "CDE" "BDF" "" "" ;;
        ("=") outLine "" "" "BCDEF" "" "BCDEF" "" "" "" ;;
        (*) outLine "ABCDEFGH" "AH" "AH" "AH" "AH" "AH" "AH" "ABCDEFGH" ;;
        esac
    }

    function outArg
    {
      typeset l=${#1} c r
      for ((c=0; c<l; c++))
      do
        outChar "${1:$c:1}"
      done
      echo
      for ((r=0; r<8; r++))
      do
        printf "%-*.*s\n" "${COLUMNS:-80}" "${COLUMNS:-80}" "${raw[r]}"
        raw[r]=""
      done
    }

    for i
    do
      outArg "$i"
      echo
    done
}

#
# To INCLUDE set $LIBRARY=""
# To INCLUDE and TEST with defaults set $LIBRARY="no" and $UNIT_TEST=""
# To INCLUDE and RUN with custom arguments set $LIBRARY="no" and $UNIT_TEST="no"
#
if [ "$LIBRARY"=="no" ] ; then
    text=$1
    if [ "$UNIT_TEST" == "no" ] ; then
	banner $text
    else
	banner "TEST"
    fi
fi
