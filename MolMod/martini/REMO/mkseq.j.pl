#!/usr/bin/perl
use Math::Trig;
########### setup  the environment for PSIPRED ###
$libdir ="$path";  #####Chainge to specify path 
$dbname ="$libdir/nr/nr.filter"; # The name of the BLAST data bank
$ncbidir="$libdir/blast/bin"; # Where the NCBI legacy blast programs have been installed
$execdir="$libdir/psipred24/bin"; # Where the PSIPRED programs have been installed
$datadir="$libdir/psipred24/data"; # Where the PSIPRED data files have been installed

$libdir ="~/contrib/ANGLOR/library/bin";  #####Chainge to specify path 
$dbname ="~/contrib/ANGLOR/library/data/nr/nr.filter"; # The name of the BLAST data bank
$ncbidir="$libdir/blast/bin"; # Where the NCBI legacy blast programs have been installed
$execdir="$libdir/psipred24/bin"; # Where the PSIPRED programs have been installed
$datadir="$libdir/psipred/24data"; # Where the PSIPRED data files have been installed
# use the stock runpsipred script instead of inline one
$psipredhome="~/contrib/ANGLOR/library/bin/psipred24";


##this perl program using PSIPRED to make sequence based secondary structure prediction
%ts=(
     'GLY'=>'G',
     'ALA'=>'A',
     'VAL'=>'V',
     'LEU'=>'L',
     'ILE'=>'I',
     'SER'=>'S',
     'THR'=>'T',
     'CYS'=>'C',
     'MET'=>'M',
     'PRO'=>'P',
     'ASP'=>'D',
     'ASN'=>'N',
     'GLU'=>'E',
     'GLN'=>'Q',
     'LYS'=>'K',
     'ARG'=>'R',
     'HIS'=>'H',
     'PHE'=>'F',
     'TYR'=>'Y',
     'TRP'=>'W',

     'ASX'=>'B',
     'GLX'=>'Z',
     'UNK'=>'X',
     'MSE'=>'M',

     'G'=>'GLY',
     'A'=>'ALA',
     'V'=>'VAL',
     'L'=>'LEU',
     'I'=>'ILE',
     'S'=>'SER',
     'T'=>'THR',
     'C'=>'CYS',
     'M'=>'MET',
     'P'=>'PRO',
     'D'=>'ASP',
     'N'=>'ASN',
     'E'=>'GLU',
     'Q'=>'GLN',
     'K'=>'LYS',
     'R'=>'ARG',
     'H'=>'HIS',
     'F'=>'PHE',
     'Y'=>'TYR',
     'W'=>'TRP',

     'a'=>'CYS',
     'b'=>'CYS',
     'c'=>'CYS',
     'd'=>'CYS',
     'e'=>'CYS',
     'f'=>'CYS',
     'g'=>'CYS',
     'h'=>'CYS',
     'i'=>'CYS',
     'j'=>'CYS',
     'k'=>'CYS',
     'l'=>'CYS',
     'm'=>'CYS',
     'n'=>'CYS',
     'o'=>'CYS',
     'p'=>'CYS',
     'q'=>'CYS',
     'r'=>'CYS',
     's'=>'CYS',
     't'=>'CYS',
     'u'=>'CYS',
     'v'=>'CYS',
     'w'=>'CYS',
     'x'=>'CYS',
     'y'=>'CYS',
     'z'=>'CYS',

     'B'=>'ASX',
     'Z'=>'GLX',
     'X'=>'CYS',
    );


##if you have PSIPRED results, copy it to protein.horiz and run this mkseq.pl  
goto pos22 if(-s "protein.horiz" >20);  ##if you have the PSIPRED results

#################################################################################################

$ff=$ARGV[0];  ## the fasta or the structure file
$f2=$ARGV[1];  ##give 1 if $ff is a structure

if(-s "$ff"<20){
printf "usage: ./mkseq.pl xx.fasta
  or   ./mkseq.pl xx.pdb 1\n";
exit();
}


#$ENV{'PATH'}="/usr/local/bin:/bin:/usr/bin:/usr/X11R6/bin:/usr/pgi/linux86/bin";
#$ENV{'LD_LIBRARY_PATH'}="/usr/local/lib:/usr/lib:/lib";
##input two options, either a structure or a fasta file

if($f2 == 1){  ##make fasta from a structure
    $seq="";
    $j=0;
    open(pdb,"$ff");
    while($line1=<pdb>){
	goto pos2 if($line1=~/^TER/|| $line1=~/^END/);
	goto pos1 if($line1!~/^ATOM/);
	goto pos1 if(substr($line1,13,2) ne 'CA'); 
	$seq=$seq.$ts{substr($line1,17,3)};
	$j++;
	$seq{$j}=substr($line1,17,3);
      pos1:
    }
  pos2:
    close(pdb);   
    $L1=length $seq;
    open(fa,">protein.fasta");
    printf fa ">: $ff\n";
    for($j=1;$j<=$L1;$j++){
	printf fa "%s",substr($seq,$j-1,1);
	if(int($j/60)*60==$j){
	    printf fa "\n";
	}
    }
    printf fa "\n";
    close(fa);
#    system("cat protein.fasta");
}else{
#########make fasta input from 'seq.txt' #########################
    open(fasta,">protein.fasta");
    printf fasta ">: $ff\n";
    open(seqtxt,"$ff");
    $i=0;
    $j=0;
    while($line=<seqtxt>){
	goto pos1 if($line=~/^>/);
	if($line=~/(\S+)/){
	    $sequence=$1;
	    $Lch=length $sequence;
	    for($k=1;$k<=$Lch;$k++){
		$i++;
		$j++;
		$seq1=substr($sequence,$k-1,1);
		$seq{$j}=$ts{$seq1};
		printf fasta "$seq1";
		if($i==60){
		    printf fasta "\n";
		    $i=0;
		}
	    }
	}
      pos1:;
    }
    if($i != 60){
	printf fasta "\n";
    }
    close(seqtxt);
    close(fasta);
}

##define the PSIPRED run environment and runing parameters

$PSIPREmod="#!/bin/tcsh

# This is a simple script which will carry out all of the basic steps
# required to make a PSIPRED V2 prediction. Note that it assumes that the
# following programs are in the appropriate directories:
# blastpgp - PSIBLAST executable (from NCBI toolkit)
# makemat - IMPALA utility (from NCBI toolkit)
# psipred - PSIPRED V2 program
# psipass2 - PSIPRED V2 program

# The name of the BLAST data bank
set dbname = $dbname

# Where the NCBI programs have been installed
set ncbidir = $ncbidir

# Where the PSIPRED V2 programs have been installed
set execdir = $execdir

# Where the PSIPRED V2 data files have been installed
set datadir = $datadir

set basename = \$1:r
set rootname = \$basename:t
set outname = \$basename.chk

/bin/cp -f \$1 psitmp.fasta

\$ncbidir/blastpgp -b 0 -j 3 -h 0.001 -d \$dbname -i psitmp.fasta -C psitmp.chk >& \$rootname.blast

echo psitmp.chk > psitmp.pn
echo psitmp.fasta > psitmp.sn
\$ncbidir/makemat -P psitmp

echo Pass1 ...

\$execdir/psipred psitmp.mtx \$datadir/weights.dat \$datadir/weights.dat2 \$datadir/weights.dat3 \$datadir/weights.dat4 > \$rootname.ss

echo Pass2 ...

\$execdir/psipass2 \$datadir/weights_p2.dat 1 1.0 1.0 \$rootname.ss2 \$rootname.ss > \$rootname.horiz

/bin/rm -f psitmp.* error.log
\n";

open(rp,">runpsipred");
printf rp "$PSIPREmod";
close(rp);
`chmod a+x runpsipred`;

########### run PSIPRED ####################
`$psipredhome/runpsipred protein.fasta`;
sleep(1);

pos22:
########### make 'seq.dat' ########################
open(psipred,"protein.horiz");
open(yan,">seq.dat"); ##the third column, 1, 2 and 4 denotes the coil, helix and strand respectively, are in real use
$j=0;
while($line=<psipred>){
    if($line=~/Conf:\s+(\d+)/){
	$conf=$1;
	<psipred>=~/Pred:\s+(\S+)/;
	$pred=$1;
	<psipred>=~/AA:\s+(\S+)/;
	$aa=$1;
	$num=length $aa;
	for($i=1;$i<=$num;$i++){
	    $j++;
	    $conf1=substr($conf,$i-1,1);
	    $pred1=substr($pred,$i-1,1);
	    $aa1=substr($aa,$i-1,1);
	    $sec{$j}=1;
	    $sec{$j}=2 if($conf1 >=1 && $pred1 eq 'H');
	    $sec{$j}=4 if($conf1 >=1 && $pred1 eq 'E');
	    printf yan "%5d   %3s%5d%5d\n",$j,$ts{$aa1},$sec{$j},$conf1;
	}
    }
}
close(yan);
close(psipred);

print "PSIPRED finished\n";
`rm -f protein.blast`;
`rm -f protein.horiz`;
`rm -f protein.ss`;
`rm -f protein.ss2`;
