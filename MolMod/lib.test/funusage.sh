TRUE=${TRUE:-0}
FALSE=${FALSE:-1}
DEBUG=${DEBUG:-0}

function funusage() {
    # this function takes the number of arguments provided to the
    # caller and the list of correct command arguments, and then 
    # checks if the number of arguments provided is less than the
    # number of mandatory arguments expected.
    # If it doesn't, it returns $TRUE, otherwise it prints an error and
    # optionally a backtrace and returns $FALSE
    if [ $# -eq 0 ] ; then return $TRUE ; fi	# caller expects no arguments
    given_arg_nb=$1 ; shift
    #echo gan= $given_arg_nb

    max_arg_nb=$#
    #echo man= $max_arg_nb
    if [ $max_arg_nb -le $given_arg_nb ] ; then return $TRUE ; fi
    
    arguments=( "$@" )
    #echo args= $arguments
    # if $# is zero then that's it
    # if $# is more than 1 then we assume the cmdline to have been
    # given as multiple separate arguments and then that's it
    # if $# is 1 then maybe the cmdLine was given as a single quoted
    # string, so the minimum number of elements would be the number
    # of words in the line that do not start with '['
    if [ $max_arg_nb -eq 1 ] ; then 
        arguments=( $arguments )
	max_arg_nb=${#arguments[@]}
    fi
    #echo gan=$given_arg_nb
    #echo Man=$max_arg_nb
    #echo args=${arguments[@]}
    min_arg_nb=0
    for arg in "${arguments[@]}" ; do
        #echo $arg
	if [ ${arg:0:1} != '[' ] ; then
	    (( min_arg_nb++ ))
	fi
    done
    #echo man= $min_arg_nb
    if [ $given_arg_nb -ge $min_arg_nb ] ; then return $TRUE ; fi
    # else it is an error: print the usage
    echo "" >&2
    echo "Error: in ${FUNCNAME[1]}() usage" >&2
    echo "       ${FUNCNAME[1]} ${arguments[@]}" >&2
    echo "" >&2
    if [ $DEBUG = $TRUE ] ; then
	echo "Traceback (last call first)" >&2
	local frame=1
	while caller $frame; do
            local lineno=${BASH_LINENO[frame]}
            printf >&2 '  File "%s", line %d, in %s\n' \
        	"${BASH_SOURCE[frame+1]}" "$lineno" "${FUNCNAME[frame+1]}"

            sed >&2 -n "${lineno}s/^[   ]*/    /p" "${BASH_SOURCE[frame+1]}"
            ((frame++));
	done
	echo "" >&2
    fi
    return $FALSE
}



function funcusage() {
    #echo "$@"
    nargs=0
    minargs=0
    maxargs=0
    given=$TRUE		# we start with the given arguments
    declare -a arguments
    for ar in "$@" ; do
        if [ "$ar" == "/" ] ; then given=$FALSE ; continue ; fi
	if [ $given -eq $TRUE ] ; then
            (( nargs++ ))
	else
	    (( maxargs++ ))
	    arguments+=( "$ar" )
	    #echo "$ar"
	    #echo "${arguments[@]}"
	    if [ ${ar:0:1} != '[' ] ; then
	        (( minargs++ ))
	    fi
	fi
    done
    #echo "nargs	minargs	maxargs"
    #echo "$nargs	$minargs	$maxargs"
    if [ $nargs -ge $minargs ] ; then return $TRUE ; fi
    # else it is an error: print the usage
    echo "" >&2
    echo "Error: in ${FUNCNAME[1]}() usage" >&2
    echo "       ${FUNCNAME[1]} ${arguments[@]}" >&2
    echo "" >&2
    if [ $DEBUG = $TRUE ] ; then
	echo "Traceback (last call first)" >&2
	local frame=1
	while caller $frame; do
            local lineno=${BASH_LINENO[frame]}
            printf >&2 '  File "%s", line %d, in %s\n' \
        	"${BASH_SOURCE[frame+1]}" "$lineno" "${FUNCNAME[frame+1]}"

            sed >&2 -n "${lineno}s/^[   ]*/    /p" "${BASH_SOURCE[frame+1]}"
            ((frame++));
	done
	echo "" >&2
    fi
    return $FALSE
}


function f() {
    if ! funusage $# one two '[t t]' ; then return 2 ; fi
    echo "    DOING f()'s work"
}

function g() { 
    if ! funcusage "$@" / one two '[t t]' ; then return 3 ; fi
    echo "    DOING g()'s work"
}

function h() {
    echo 'calling g one two three'
    g one two three	# should have no problem
    echo 'calling g one two'
    g one two		# neither
    echo 'calling g "one two"'
    g 'one two' 	# this should fail
    
    echo 'calling f one two three'
    f one two three	# should have no problem
    echo 'calling f one two'
    f one two		# neither
    echo 'calling f "one two"'
    f 'one two' 	# this should fail
    
}  

h
