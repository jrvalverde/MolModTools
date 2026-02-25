#!/bin/bash

echo "Using \"\$*\":"
for a in "$*"; do
    echo $a;
done

echo -e "\nUsing \$*:"
for a in $*; do
    echo $a;
done

echo -e "\nUsing \"\$@\":"
for a in "$@"; do
    echo $a;
done

echo -e "\nUsing \$@:"
for a in $@; do
    echo $a;
done              

echo -e "\nUsing \"\$args\":"
args=( "$@" )
for a in "${args[@]}" ; do
    echo $a;
done              

echo -e "\nUsing \$args:"
args=$@
for a in $args ; do
    echo $a;
done              
