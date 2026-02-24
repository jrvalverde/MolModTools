#!/bin/bash
#
# AUTHOR
#	José R. Valverde, CNB/CSIC. jrvalverde@cnb.csic.es, 2014
#
#	Licensed under (at your option) either GNU/GPL or EUPL
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
INCLUDE='INCLUDE'
TRUE=${TRUE:-0}
FALSE=${FALSE:-1}
#
OK=${OK:-0}
ERROR=${ERROR:-1}
#
VERBOSE=${VERBOSE:-0}	# default value for verbose *level*
DEBUG=${DEBUG:-1}

# check if include already exists as a function
if declare -f -F include >/dev/null ; then return ; fi

#ignored, overseed by later redefinition
# function include() {
#     if [ $# -lt 1 ] ; then echo "Err: ${FUNCNAME[0]} includefile" ; exit 1 
#     else echo ">>> ${FUNCNAME[0]}" $* ; fi
#     
#     while [ $# -gt 0 ] ; do
#         if [ ! -s "$1" ] ; then
#             echo "Warning ${FUNCNAME[0]}: '$1' does not exist or is empty!"
#         else
#             if [ -z $"$1" ] ; then
# 	        source $1.bash
# 	    fi
# 	    if [ $? -ne 0 ] ; then
# 	        echo ">>> ERROR (include): could not source" $*
# 		exit
# 	    fi
# 	    "$1"='ok'
#         fi
#         shift
#     done
# }
# 
function include() {
    # we expect the name of a bash file
    # if it has been included, a variable with the same name in capitals
    # must exist
    if  [ $# != 1 ] ; then echo "error: include file.bash" ;return $ERROR ; fi
    
    file=$1
    inc_name=`basename $file`
    NAME=${inc_name%.bash}		# remove .bash from the end
    NAME=${NAME^^}		# convert to all caps
    LIB=`dirname $file`
    
    if [ $LIB = '' ] ; then
        LIB=`dirname ${BASH_SOURCE[0]}`	# located in our same directory
    fi
    if [ ! -s $LIB/$inc_name ] ; then echo "$LIB/$inc_name doesn't exist!" ; return $ERROR ;fi
    #echo "Checking $NAME: ${!NAME}"
    #[[ -v ${!NAME} ]] && echo "$file already included" 
    [[ -z ${!NAME} ]] && source $LIB/$inc_name
    # in case the included script does not define its presence we will
    [[ -z ${!NAME} ]] && export $NAME=$NAME
    return $OK
}

#[[ $0 != $BASH_SOURCE ]] && echo "Script is being sourced" || echo "Script is being run"
#check if we are being sourced:
# as an included script we do not want to execute anything
if [[ $0 == $BASH_SOURCE ]] ; then
    trap 'rm -f "$TMPFILE"' 2 3 4 6 7 9 13 15 17

    TMPFILE=$( mktemp TESTXXXX --suffix=.bash ) || exit 1
    echo $TMPFILE
    echo "echo OK from $TMPFILE" > $TMPFILE
    include ./$TMPFILE
    rm $TMPFILE
fi
