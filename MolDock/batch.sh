#!/bin/bash
#
#	Trueno does not honor saving stdout/stderr to my home
# so I need to ensure I get them.
#
#	Usage:
#	f=file,p=program,o=outdir batch.sh
#
echo "bash $p $f > $o/`basename $o`.log 2>&1" > /home/cnb/jrvalverde/ppp
bash $p $f > $o/`basename $o`.log 2>&1

