#!/usr/bin/perl

##include files: FABR.pl, FABR, FABRcomm,BBdat(11,12,13)

use File::Basename;
use Cwd 'abs_path';

    $path0=".";
if(!-s "$path0/FAMRcomm"){
    #$path0="/home/yunqi/REMO";
    $path0=dirname(abs_path(__FILE__));
}
#$mode="fast";
$mode="normal";


####do not change the following part **************************************************

$contrl=$ARGV[0];
$seqk= $ARGV[1];   ##structure file
if($contrl ==0){
    $seqdat=$ARGV[2];
    $template=$ARGV[3];
    $prhb=$ARGV[4];
}elsif($contrl == 1){
    $fasta=$ARGV[2];
    chomp $fasta;
}
#print "$seqdat,$prhb >>>>";

#print ">>>*** Bug report to yunqi\@ku\.edu\.\n";
printf "rm Hdd and PRHB before run different proteins at the same folder\n" if($mode eq "fast");

if(-s "$seqk" <20){
print "Usage::
 ./REMO.p1 0 xx.pdb seq.dat [template] <PRHB> 
    build atomic model from xx.pdb, When using PRHB, template is obligatory. output: xx.pdb.fix
 ./REMO.p1 1 xx.pdb seq.txt 
    build full length model according to seq.txt (fasta file), output: xx.pdb.fix\n";
exit();
}

$path=`pwd`;
chomp $path;
chomp $seqk;

$t=substr($seqk,0,1);
$seqk="$path\/$seqk"  if($t ne '/' && $t ne '~');
if(length $template >1){
    $t=substr($template,0,1);
    $template="$path\/$template"  if($t ne '/' && $t ne '~');
}

%AA=(    #number of heavy atoms
       'GLY'=>4,
       'ALA'=>5,
       'SER'=>6,
       'CYS'=>6,
       'VAL'=>7,
       'THR'=>7,
       'ILE'=>8,
       'PRO'=>7, 
       'MET'=>8,
       'ASP'=>8,
       'ASN'=>8,
       'LEU'=>8,
       'LYS'=>9,
       'GLU'=>9,
       'GLN'=>9,
       'ARG'=>11,
       'HIS'=>10,
       'PHE'=>11,
       'TYR'=>12,
       'TRP'=>14,

       'ASX'=>8,
       'GLX'=>9,
       );
%AAH=(    #number of all atoms and IC lines
       'GLY'=>7,
       'ALA'=>10,
       'SER'=>11,
       'CYS'=>11,
       'VAL'=>16,
       'THR'=>14,
       'ILE'=>19,
       'PRO'=>14,
       'MET'=>17,
       'ASP'=>12,
       'ASN'=>14,
       'LEU'=>19,
       'LYS'=>22,
       'GLU'=>15,
       'GLN'=>17,
       'ARG'=>24,
       'HIS'=>17,   ##neutral #charged
       'PHE'=>20,
       'TYR'=>21,
       'TRP'=>24,
       
       'NTR'=>6,    #Nterm
       'CTR'=>2,    #Cterm
       );
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

     'B'=>'ASX',
     'Z'=>'GLX',
     'X'=>'CYS',
    );
%csec=(
       'H'=>'2',   #alpha helix
       'G'=>'2',   #3-helix (3/10 helix)
       'I'=>'2',   #5-helix (pi helix)

       'E'=>'4',   #extended strand, participates in beta ladder

       'B'=>'1',   #residue in isolated beta-bridge
       'T'=>'1',   #hydrogen bonded turn
       'S'=>'1',   #bend
       'C'=>'1',
       ' '=>'1',
      );
    $chi1gap=0.523598776;  ##30o gap for chi1
    $cut="30o";

######generate force field
    undef @seq;
    $j=0;
if($contrl != 1 || ($contrl ==1 && -s "$fasta" <20)){
    open(tt,"$seqk");
    while($ln=<tt>){
	if($ln=~/^ATOM/ && $ln=~/CA/){
	    $j++;
	    $seq[$j]=substr($ln,17,3);
	    $seqn[$j]=substr($ln,22,4);  ##residue seires
	}
    }
    close(tt);
}else{ # if($contrl eq "1"){
    if(-s "$fasta" <20){
	printf	"WARNING!!!, $fasta  is not available,no missing CA build\n";
	goto pos03;
    }
    $j=0;
    open(seqtxt,"$fasta");   #a fasta file
    while($line1=<seqtxt>){
	next if($line1=~/^>/);
	if($line1=~/(\S+)/){
	    $squ=$1;
	    $Lch=length $squ;
	    for($k=1;$k<=$Lch;$k++){
		$j++;
		$seq1=substr($squ,$k-1,1);
		$seq[$j]=$ts{$seq1};
		$seqn[$j]=$j; #substr($ln,22,4);  ##residue seires
	    }
	}
    }
    close(seqtxt);
}
 pos03:    
    $Nlen=$j;
if($Nlen<5){
    printf "The chain too short, quit\n";
    exit();
}


#for($i=1;$i<=$Nlen;$i++){
#    print "$i, $seq[$i]\n";
#}

#print "$seq[$Nlen], $Nlen >>>\n";
if($mode eq "fast"){
    &FGEN(@seq,$Nlen) if(-s "Hdd"<30);
}else{
    &FGEN(@seq,$Nlen);
}

#exit();
##add bb atoms if needed
$keep=0;
`grep -a ATOM $seqk |wc`=~/(\d+)/;
$keep=1 if($1 >=5*$Nlen);

`cp $seqk pPL1`;
if($contrl == 2){
    system("$path0/FAMR 1 pPL1");
    `mv pPL1.h $seqk\.h`;
    goto PEND;
}
#2014,Jan 21, clean the residue series
$rs0=1;
open(ind,"pPL1");
while($line=<ind>){
 if($line=~/^ATOM/){
  $rs0=substr($line,22,4)+0;
  goto opp;
}}
opp:;
close(ind);
if($rs0>1){  ###need shift
   open(ind,"pPL1");
   open(ot,">chil");
   while($line=<ind>){
      if($line=~/^ATOM/){
	  $a=substr($line,0,22);
          $bb=substr($line,22,4)-$rs0+1;
          $b=sprintf("%4s",$bb);
          $c=substr($line,26,28);
          print ot "$a$b$c\n";
   }}
   close(ot);
   `mv chil pPL1`;
}

$gjj=0;
#printf "$seqk2\n";
if(-s "$template" >50){
#    $fg="$seqk2";
#    &shift($fg); ##################
    $j=0;
    open(tt,"$template");
    while($ln=<tt>){
	if($ln=~/^ATOM/ && $ln=~/CA/){
	    $j++;
	    $seq2[$j]=substr($ln,17,3);
	    $seqn2[$j]=substr($ln,22,4);  ##residue seires
	}
    }
    close(tt);
    $Nlen2=$j;
    if($Nlen2<5){
	printf "The chain2 too short, quit\n";
	exit();
    }
    if($Nlen!=$Nlen2){  ##need to generate Hdd for $seqk2
	$gjj=1;
    }else{
	for($i=0;$i<=$Nlen;$i++){
	    if($seqn[$i] != $seqn2[$i]){
		$gjj=1;
		goto ppo;
	    }
	}
    }
  ppo:
    if($gjj>0 && $contrl < 11){
	for($i=0;$i<=$Nlen;$i++){
	    for($j=0;$j<=$Nlen2;$j++){
		if($seq[$i] == $seq2[$j]){
		    $sqq{$i,$j}=1;
		    goto ppm;
		}
	    }
	  ppm:
	}
    }
}

#printf ">>>>length $Nlen $Nlen2\n";

#create full atomic model
    $ff="pPL1";
    &caclash($ff,$nclash,@x,@y,@z,@fu);
    $gtype="easy";
    $gtype="hard" if($nclash/$Nlen >1.3);  ##only for 22
    if($gtype eq "hard"){
	for($i=1;$i<=$Ld;$i++){
	    $weight{$i}=1.0;
	}
    }
    $ircut=2;  ##nclash and nbroken cutoff

    if($nclash>10 && $contrl != 1){  ##need remove CA clash
	$fk=0.7;
	$Ld=$Nlen;
	for($ik=1;$ik<=100;$ik++){  ##45
	    &relax($ik,$Ld,$fk,$nc,$nb);
	    $fk-=0.007 if($ik>12);
	    goto poskk if($nb+$nc<$ircut);  ##for modeller use 10, else use this
	}
      poskk:; 
	open(pdb,">$ff");
	for($i=1;$i<=$Ld;$i++){
	    printf pdb "%29s %8.3f%8.3f%8.3f\n",$fu[$i],$x[$i],$y[$i],$z[$i];
	}
	printf pdb "%-3s\n","TER";
	close(pdb);
    }

    if($contrl == 0){
	`cp $seqdat seq.dat` if(-s "seq.dat" <20 && -e $seqdat);
	if(-s "$prhb" >20){
	    `cp $prhb PRHB`;
	}else{
	    `$path0/FAMR 23 $ff $path0/BBdat`;
	    $m2="pPL1.h";
	    &genPRHB($m2,$m2);
	}
	system("$path0/FAMR 4 $ff $path0/BBdat $template");
        #print "pPL1.fix exists"  if ( -e "pPL1.fix" );
	`mv pPL1.fix $seqk\.fix` if ( -e "pPL1.fix" );
	`mv pPL1.h $seqk\.h` if (-e "pPL1.h");
    }elsif($contrl == 1){
	system("$path0/FAMR 31 $ff $path0/BBdat");
	`mv pPL1.fix $seqk\.fix` if ( -e "pPL1.fix" );
	`mv pPL1.h $seqk\.h` if ( -e "pPL1.h" );
    }
PEND:
    `rm -f $path/pPL1`;
    `rm -f chi1`;
if($mode eq "normal" || $contrl eq "7"){
    `rm -f $path/Hdd`;  
   # `rm -f $path/PRHB`;
}
    exit();
#######################################################################

##remove CA clash
sub relax($ik,$Ld,$fk,$nc,$nb){
    $nc=0;
    $nb=0;
    if($ik<32){
	$gk=0.9*$fk;
	$rclash=3.66;
	$rclashU=3.95;
    }elsif($ik<62){
	$gk=($fk+0.19)**2;
	$rclash=3.67;
	$rclashU=3.9;
    }elsif($ik<82){
	$gk=0.79*exp(-1.0*($fk+1.0));
	$rclash=3.68;
	$rclashU=3.89;
    }else{
	$gk=5*$fk*$fk;
	$rclash=3.7;
	$rclashU=3.88;
    }
    undef @vx;
    undef @vy;
    undef @vz;
    for($i=1;$i<$Ld;$i++){
	$nca=0;
	for($j=$i+1;$j<=$Ld;$j++){
	    $ax=$x[$j]-$x[$i];
	    $ay=$y[$j]-$y[$i];
	    $az=$z[$j]-$z[$i];
	    $r=sqrt($ax*$ax+$ay*$ay+$az*$az);
	    $rc0=3.65;
	    $rc0=2.8 if($j-$i == 1 && $seq[$i] eq "PRO" && $seq[$j] eq "PRO"); ##PRO in turn
	  #  printf "$i $rc0 $nc\n";
	    if($r < $rc0){
		$r=0.001 if($r<0.001);
		$nc++;
		$nca++;
		$dr=$fk*($rclash-$r)/$r;
		$dx=$dr*$ax;
		$dy=$dr*$ay;
		$dz=$dr*$az;
		$vx[$j]+=$dx;
		$vy[$j]+=$dy;
		$vz[$j]+=$dz;
		$vx[$i]-=$dx;
		$vy[$i]-=$dy;
		$vz[$i]-=$dz;		
	    }elsif($j-$i == 1 && ($r >3.85 || $r<3.75)){
		$nb++;
		if($r>3.85){
		    $dr=($fk+0.05)*($r-$rclashU)/$r;
		}else{
		    $dr=($fk+0.05)*($r-3.8)/$r;
		}
		$dx=$dr*$ax;
		$dy=$dr*$ay;
		$dz=$dr*$az;
		$vx[$j]-=$dx;
		$vy[$j]-=$dy;
		$vz[$j]-=$dz;
		$vx[$i]+=$dx;
		$vy[$i]+=$dy;
		$vz[$i]+=$dz;
	    }
	} ###for $j
    }
  
    for($i=1;$i<=$Ld;$i++){
	$ax=$vx[$i];
	$ay=$vy[$i];
	$az=$vz[$i];	
	$r=sqrt($ax*$ax+$ay*$ay+$az*$az);
	goto pln if($r<0.001);
	$fb=1;
	$fb=$gk/$r if($r>$gk);
	$x[$i]=$x[$i]+$fb*$ax*$weight{$i};
	$y[$i]=$y[$i]+$fb*$ay*$weight{$i};
	$z[$i]=$z[$i]+$fb*$az*$weight{$i};
      pln:
    }
#		printf " $ik, cycle $nc,$nb,$fk $x[174]\n";
} ##end of subroutine


##calculate the CA clash for ca STRUCTURE
##get the wright for Ca move
sub caclash($ff,$nclash,@x,@y,@z,@fu){
    my @st;
    my $i,$j;

    $nclash=0;
    $natom=0;
    undef @x;
    undef @y;
    undef @z;
    undef @fu;
   open(tt,"$ff");
    $i=0;
    while($line=<tt>){
	chomp $line;
	$natom++ if($line=~/^ATOM/); 
	if($line=~/CA/){
	    $i++;
	    $x[$i]=substr($line,30,8);
	    $y[$i]=substr($line,38,8);
	    $z[$i]=substr($line,46,8);	
	    $fu[$i]=substr($line,0,29);
#	    print "$i,$line\n";
	}    
    }   ##logfile read finish
    close(tt);
	$Ld=$i;
    undef %weight;
    for($i=1;$i<$Ld;$i++){
	for($j=$i+1;$j<=$Ld;$j++){
	    $r=($x[$i]-$x[$j])**2+($y[$i]-$y[$j])**2+($z[$i]-$z[$j])**2;
	    if($r < 13.69){ #12.96){
		$nclash++;
		$weight{$i}++;
		$weight{$j}++;
#		print "$nclash,$r,$i,$j\n";
	    }
	    if($j-$i ==1 && $r >15.6){  ##broken
		$nclash+=10;
		$weight{$i}++;
		$weight{$j}++;
	    }
	}
    }
    @st=sort{$weight{$b} <=> $weight{$a} } keys %weight;
    $mw=$weight{$st[0]};
    $mw=100 if($mw>100);
    if($mw>1){
	for($i=1;$i<=$Ld;$i++){
	    $a=$weight{$i}/$mw;
	    $a=1.0 if($a>1.0);
	    $weight{$i}=$a;
	}
    }
#printf "$ff has CAclashes: %5d length %4d\n",$nclash,$Ld;
}


#this subroutine generate the force field for calculation
sub FGEN(@seq,$Nlen){
###############################read comm files #########################################################
    open(out,"$path0/FAMRcomm");   #atom name and charge density all 20 types of residues
$line=<out>;
$line=~/\s+(\d+)\s+(\d+)\s+(\d+)/;
$nao=$1;
$nang=$2;
$nhbond=$3;
#    print "<><><> $path0\n";
for($nt=0;$nt<$nao;$nt++){
    $line=<out>;
    chomp($line);
    goto p5 if($line=~/^\#/);
    if($line=~/\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/){
	$atom[$nt][0]=$2;  #atom
	$atom[$nt][1]=$3;  #atomt type
	$atom[$nt][2]=$4;  # residue
	$atom[$nt][3]=$5;  # atom charge density
	$atom[$nt][4]=$6;  #van der Waals E
	$atom[$nt][5]=$7;  #van der Waals r
	$atom[$nt][6]=$8;  #hbond donor and acceptor -1 donor +1 accptor
#	$atom[$nt][7]=$9;  #hbond bias connection of heavy atoms (both for donor and acceptor)
    }
  p5:
}

for($nt=0;$nt<$nao;$nt++){
    $line=<out>;
    chomp($line);
    goto p6 if($line=~/^\#/);
    if($line=~/\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/){
	$bond[$nt][0]=$2;  #connect
	$bond[$nt][1]=$3;  #connect
	$bond[$nt][2]=$4;  #connect
	$bond[$nt][3]=$5;  #connect
	$bond[$nt][4]=$6;  #con1 bond kb
	$bond[$nt][5]=$7;  #con1 bond rB
	$bond[$nt][6]=$8;  #con1 bond kb
	$bond[$nt][7]=$9;  #con1 bond rB
	$bond[$nt][8]=$10;  #con1 bond kb
	$bond[$nt][9]=$11;  #con1 bond rB
	$bond[$nt][10]=$12;  #con1 bond kb
	$bond[$nt][11]=$13;  #con1 bond rB
    }
  p6:
}
$nao--;

for($nt=0;$nt<$nang;$nt++){
    $line=<out>;
    chomp($line);
    goto p7 if($line=~/^\#/);
    if($line=~/\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/){
	$ang{$1,$2,$3,1}=$4; ##ktheta
	$ang{$1,$2,$3,2}=$5; ##theta
	$ang{$1,$2,$3,3}=$6; ##K Urey-Breadley
	$ang{$1,$2,$3,4}=$7; ##S0 of UB	
	$ang{$3,$2,$1,1}=$4; ##ktheta
	$ang{$3,$2,$1,2}=$5; ##theta
	$ang{$3,$2,$1,3}=$6; ##K Urey-Breadley
	$ang{$3,$2,$1,4}=$7; ##S0 of UB	
    }
  p7:
}
$nang--;

for($nt=0;$nt<$nhbond;$nt++){
    $line=<out>;
    chomp($line);
    $hbnd[$nt]=$line;
}
    <out>;
    $nRAM=1370;
for($nt=0;$nt<$nRAM;$nt++){
    $line=<out>;
    chomp($line);
    $RAM[$nt]=$line;
}
$nhbnd;
close(out);
##############generate atom force field and bond matrix ##########################################
$natom=0;
$hbdon=0;
$hbacc=0;
$nbond=0;
for($i=1;$i<=$Nlen;$i++){
    $nheavy=$AA{$seq[$i]};
    $nall=$AAH{$seq[$i]};    
    $nheavypre=$AA{$seq[$i-1]} if($i>1);  
    $nheavy=$nheavy+1 if($i>$Nlen-1);  ##C-term
    $nall=$nall+1 if($i>$Nlen-1);  ##C-term
    $nall=$nall+2 if($i<2);  ##N-term 
    for($ii=1;$ii<=$nao;$ii++){
	if($atom[$ii][2] eq $seq[$i]){
	    $natom++;
	    $bdnum[$natom]=0;
	    for($k=0;$k<=6;$k++){
		$ch[$natom][$k]=$atom[$ii][$k];   #chain atom
	    }
	    if($ch[$natom][0] eq 'CA'){   ##record the ca ip
		$caip[$i]=$natom;
	    }
#	    $ch[$natom][7]=$atom[$ii][7]+$natom if($atom[$ii][6]!=0);  #Hbond conerned heavy atoms
	    for($k=0;$k<=11;$k++){
		$bd[$natom][$k]=$bond[$ii][$k] if($k>=4&&$bd[$natom][$k]<0.01);

		if($k<4 &&$bond[$ii][$k]>0.5){
		     if($bond[$ii][$k]<99990){   #chain bond
			 $bd[$natom][$k]=$natom+$bond[$ii][$k]-$ii;  }
		     elsif($bond[$ii][$k]>99990){
			 if($k!=2){   ##C
			     $bd[$natom][$k]=$natom+$nall-2;
			     $bd[$natom+$nall-2][2]=$natom;  #N
			     if($seq[$i+1] eq "PRO"){
				 $bd[$natom][6]=260.0;   ##C
				 $bd[$natom][7]=1.300;				 
				 $bd[$natom+$nall-2][8]=260.0;  ##N
				 $bd[$natom+$nall-2][9]=1.30;
			     }else{
				 $bd[$natom][6]=370.0;   ##C
				 $bd[$natom][7]=1.345;
				 $bd[$natom+$nall-2][8]=370.000;
				 $bd[$natom+$nall-2][9]=1.3450;    ##N-C
			     }
				 print "error in residues connect <> $ii,$seq[$i],$ch[$natom][0]\n" if($k!=1);  }
		     }
		     $bdnum[$natom]++ if($bond[$ii][$k]!=$bond[$ii][$k+1]);   
		     }  #k<4
		 
	    }   #end of k
#	             $bdnum[$natom]++ if($atom[$ii][0] eq 'N');
	}
    }  #end of ii loop   
###begin of N-term###########################################
    if($i==1){ ##treat N-term  left zero
	if($seq[$i] eq 'PRO'){
	   $ntt=2; 
	   $ch[1][1]='NP'; 
	   $ch[2][1]='CP1';
	   $ch[7][1]='CP3';
	   $ch[1][3]=-0.07;
           $ch[2][3]=0.16;  
           $bd[1][0]=$nheavy;
           $bd[1][1]=2;
	   $bd[1][2]=$nheavy+1;
	   $bd[1][3]=$nheavy+2;
	   $bd[1][4]=320.0;   ##N-CD
	   $bd[1][5]=1.502;
	   $bd[1][6]=320.0;   ##N-CA
	   $bd[1][7]=1.485;
	   $bd[1][8]=460.0;   ##N-1H
	   $bd[1][9]=1.006;
	   $bd[1][10]=460.0;   ##N-2H
	   $bd[1][11]=1.006;
	   $bd[2][6]=320.0;   ##N-CA
	   $bd[2][7]=1.485;	   
       }else{
	    $ntt=2; 
	    $ch[1][1]='NH3';
	    $ch[1][3]=-0.30;
	    $ch[2][3]=0.21;  
	    $ch[2][3]=0.13 if($seq[$i] eq 'GLY');
	    $bd[1][0]=$nheavy+1;
	    $bd[1][1]=2;
	    $bd[1][2]=$nheavy+2;
            $bd[1][3]=$nheavy+3;
	    $bd[1][4]=403.0;   ##N-1H
	    $bd[1][5]=1.04;
	    $bd[1][6]=200.0;   ##N-CA
	    $bd[1][7]=1.48;
	    $bd[1][8]=403.0;   ##N-2H
	    $bd[1][9]=1.04;
	    $bd[1][10]=403.0;   ##N-3H
	    $bd[1][11]=1.04;
	    $bd[2][6]=200.0;   ##N-CA
	    $bd[2][7]=1.48;	   
	}
	   $bdnum[1]=4;
	$kp=$natom-$nheavy;
	for($kk=$natom;$kk>=$nheavy+1;$kk--){  #shift H atoms
	    for($k=0;$k<=7;$k++){
		$ch[$kk+$ntt][$k]=$ch[$kk][$k]; 
	        $ch[$kk][$k]="0";                  } #chain atom
	    
	    for($k=0;$k<=11;$k++){
		$bd[$kk+$ntt][$k]=$bd[$kk][$k];  
	        $bd[$kk][$k]="0";                  } #chain bond
	    $bdnum[$ntt+$kk]=$bdnum[$kk];
	    $bdnum[$kk]="0";
	}   
	for($k=$nheavy+1;$k<=$nheavy+$ntt+1;$k++){  #insert H
	    $ch[$k][1]='HC';
	    $ch[$k][2]=$seq[$i];
	    $ch[$k][6]=-1;  #donor
#	    $ch[$k][7]=1; #Hbond
	    $bd[$k][0]=1; #connection
	    $bdnum[$k]=1;
	if($seq[$i] eq 'PRO'){      #PRO
	    $ch[$k][3]=0.24;
	    if($k==7){  ##CD  #NP-CP3 N-CD
		$ch[$k][3]=0.16;
		$bd[$k][4]=320.0;   ##NP-HC
		$bd[$k][5]=1.502; 
	    }else{
		$bd[$k][4]=460.0;
		$bd[$k][5]=1.006;    
	    }
	}else{
	    $ch[$k][3]=0.33;   
	    $bd[$k][4]=403.0;  ##NH3-HC
	    $bd[$k][5]=1.040; 
	}
	}  ##Hatoms finish
	if($seq[1] eq 'GLY' || $seq[1] eq 'PRO'){
	}else{
	    $ch[$k][3]=0.10;    ##HA
	}
	$bd[1][8]=403.0;
	$bd[1][10]=403.0;
	$bd[1][9]=1.04;
	$bd[1][11]=1.04;
	if($seq[$i] eq 'PRO'){  #PRO
	    $bd[10][0]=2;
	    $bd[10][4]=320.0;
	    $bd[10][5]=1.502;
	}
	$k--;
	if($seq[$i] eq 'PRO'){
	    $ch[$k-2][0]='1H';
	    $ch[$k-1][0]='2H'; 
	    $ch[$k][0]='HA'; 
	    for($noo=-1;$noo<=0;$noo++){
		$ch[$k+$noo][4]=-0.046;
		$ch[$k+$noo][5]=0.2245;  ##vdw r
		}
	}
	
	else{
	    $ch[$k-2][0]='1H';
	    $ch[$k-1][0]='2H'; 
	    $ch[$k][0]='3H'; 
	    for($noo=-2;$noo<=0;$noo++){
		$ch[$k+$noo][4]=-0.046;
		$ch[$k+$noo][5]=0.2245;  ##vdw r
	    }
	}	
      #end of H atoms

	for($kk=2;$kk<=$nheavy;$kk++){   #heavy atoms
	    for($k=0;$k<$bdnum[$kk];$k++){
		if($seq[$i] eq 'PRO'){
		    $bd[$kk][$k]=$bd[$kk][$k]+$ntt if($bd[$kk][$k]>$nheavy && $bd[$kk][$k]<99990); }
		else{
		    $bd[$kk][$k]=$bd[$kk][$k]+$ntt if($bd[$kk][$k]>$nheavy && $bd[$kk][$k]<99990);  }
	        }
	}  #end of heavy atoms
      $bd[3][1]=$nall+1;    #connect 2nd residue
      $bd[$nall+1][2]=3;
	$bdnum[1]=4;
	$natom=$natom+$ntt;
	if($seq[$i] eq 'PRO'){
	   $bd[1][0]=7;
           $bd[1][1]=2;
	   $bd[1][2]=8;
	   $bd[1][3]=9;
       }
    }     #end of $i==1 
############end of N term ####################################+++++++++++++++++++++++++   
############ begin of C-term #########################################################
    if($i==$Nlen){
	for($kk=$natom;$kk>=$natom-$AAH{$seq[$i]}+$nheavy;$kk--){  #shift H atoms
	    for($k=0;$k<=7;$k++){
		$ch[$kk+1][$k]=$ch[$kk][$k]; 
	        $ch[$kk][$k]="0";                  } #chain atom	    
	    for($k=0;$k<=11;$k++){
		$bd[$kk+1][$k]=$bd[$kk][$k];  
	        $bd[$kk][$k]="0";                  } #chain bond
	    $bdnum[$kk+1]=$bdnum[$kk];
	    $bdnum[$kk]="0";
	}  
	$isC=$natom-$AAH{$seq[$i]}+3;
	$isOT=$natom-$AAH{$seq[$i]}+$nheavy; 
	$ch[$isC][1]="CC";
	$ch[$isC][3]=0.34;
	$ch[$isC][4]=-0.07;	
	$ch[$isC][5]=2.00;  #vdw radius
	$bd[$isC][1]=$isOT;   #connect with OXT
	$ch[$isC+1][1]="OC";    #change the name of last O
	$ch[$isOT][0]="OXT";	
	$ch[$isOT][1]="OC";	
	$ch[$isOT][2]=$seq[$i];	
	$ch[$isC+1][3]=-0.67;
	$ch[$isOT][3]=-0.67;	
	$ch[$isOT][4]=-0.12;	
	$ch[$isOT][5]=1.7;	 ##Vr
	$ch[$isOT][6]=1;    #Hbond acceptor	
#	$ch[$isOT][7]=$isC;
	$bd[$isOT][0]=$isC;
	$bd[$isOT][4]=525.0;
	$bd[$isOT][5]=1.26;
	if($seq[$i] eq 'PRO'){  ##CP1-CC
	    $bd[$isC][4]=250.0;
	    $bd[$isC][5]=1.49;   
	    $bd[$isC-1][8]=250.0;
	    $bd[$isC-1][9]=1.49; 
	    $bd[$isC-1][10]=309.00;
	    $bd[$isC-1][11]=1.111;
	}else{
	    $bd[$isC][4]=200.0;
	    $bd[$isC][5]=1.522;   ##CA-C CT1-CC/CT2-CC
	    $bd[$isC-1][8]=200.0;
	    $bd[$isC-1][9]=1.522;   ##CA-C CT1-CC/CT2-CC
	}
	$bd[$isC][6]=525.0;
	$bd[$isC][7]=1.26;
	$bd[$isC][8]=525.0;
	$bd[$isC][9]=1.26;
	$bd[$isC][10]=525.0;
	$bd[$isC][11]=1.26;
	$bdnum[$isOT]=1;
	$natom=$natom+1;
	$bd[$isC-2][0]=$isOT+1 if($seq[$i] ne 'PRO');   #Cterm N
	for($k9=1;$k9<$nheavy;$k9++){
	    for($k9j=0;$k9j<=3;$k9j++){
		$bd[$isC-2+$k9][$k9j]++ if($bd[$isC-2+$k9][$k9j]>$isOT);
	    }
	}
	$bd[$isC-1][3]=$isOT+1 if($seq[$Nlen] eq 'PRO');   #Cterm N
    }
###########end of C-term#####################################################################
}   ##end of i
$nbond=$nbond/2;

#####begin assgin angle and torsion angle###############################################################
$ir=0;
$ktt=0;
$kangerg=0;
$ki=$AAH{$ch[1][2]};
$ii=0;
  for($i=1;$i<=$natom;$i++){             #atom 1
      if($ch[$i][0] eq 'N'){   #########??????????????????????????????
	   if($ii>$ki){
	       $ki=$ii;
	       $rm=$ch[$i-2][2]; 
	       $krm=$i0+$ii+1;}
	  $ii=0;
	  $ir++;
	  $i0=45*($ir-1)+1;
       }
      $knkn=$i;  ##HIS special cases, to here, noe angle find
    for($k=0;$k<$bdnum[$i];$k++){
	$i2=$bd[$i][$k];   #center atom     2
	goto pos21 if($bdnum[$i2]<2 || $i2==$i);
	    $a0=$ch[$i][1];
	    $a1=$ch[$i2][1];
	for($k2=0;$k2<$bdnum[$i2];$k2++){
	    $i3=$bd[$i2][$k2];     #atom     3
	    goto pos2 if($i3==$i2 || $i3<=$i);     #avoid replical count
	    $ag[$i0+$ii][0]=$i;   ##start of angle point
	    $ag[$i0+$ii][1]=$i2;   #center of angle, vexter
	    $ag[$i0+$ii][2]=$i3;    #last point
	    $a2=$ch[$i3][1];   
	  pos111:
	    $ag[$i0+$ii][3]=$ang{$a0,$a1,$a2,1}; ##Ktheta
	    $ag[$i0+$ii][4]=$ang{$a0,$a1,$a2,2};  #theta0
	    $ag[$i0+$ii][5]=$ang{$a0,$a1,$a2,3}; #K Urey-Bradley
	    $ag[$i0+$ii][6]=$ang{$a0,$a1,$a2,4};  #S0 of UB		
	    $ii++;
	    $ktt++;
	  pos2:
	}  #end of k2
      pos21:
    }  ##end of k
}  #end of angle
    $nag=45*$Nlen;
$nang=$ktt;

##change the format of atom name
for($i=1;$i<=$natom;$i++){
    $n=length $ch[$i][0];
    if($n ==4){
	if($ch[$i][0]=~/(\S+)(\d)(\d)/){
	   $ch[$i][0]="$3"."$1"."$2";
       } 
    }elsif($n ==3){
	if($ch[$i][0]=~/(\d+)(\S+)/){
	    $ch[$i][0]=$ch[$i][0]." ";
	}else{
	    $ch[$i][0]=" ".$ch[$i][0];
	}
    }elsif($n == 2){
	if($ch[$i][0]=~/(\d+)(\S+)/){
	    $ch[$i][0]=$ch[$i][0]."  ";
	}else{
	    $ch[$i][0]=" ".$ch[$i][0]." ";
	}
    }elsif($n == 1){
	    $ch[$i][0]=" ".$ch[$i][0]."  ";
	}  
}

open(ff,">Hdd");
printf ff "%6d %6d %6d\n",$natom,$nag,$nhbond-1;
# printf out1 "#SsNo. atom contyp res charge  vdwE  vdwR HBOND\n";   ##atm file
# printf out2 "#es_No.  con1  con2  con3   con4  con1E  con1r  con2E   con2r   con3E  con3r  con4E  con4r nbonds\n"; $bnd
for($nt=1;$nt<=$natom;$nt++){ 
    $kg=$nt;
    $kg=9999 if($nt>9999);   ###for previous bug
   printf ff "%4d %-4s %-4s %-4s %6.3f %7.4f %7.4f %4d\n",
            $kg,$ch[$nt][0],$ch[$nt][1],$ch[$nt][2],$ch[$nt][3],$ch[$nt][4],$ch[$nt][5],$ch[$nt][6];
}
for($nt=1;$nt<=$natom;$nt++){  
    $kg=$nt;
    $kg=9999 if($nt>9999);  ##for fix bug
   printf ff "%4d %6d %6d %6d %6d %8.3f %7.4f %8.3f %7.4f %8.3f %7.4f %8.3f %7.4f %2d\n",
            $kg,$bd[$nt][0],$bd[$nt][1],$bd[$nt][2],$bd[$nt][3],$bd[$nt][4],$bd[$nt][5],$bd[$nt][6],$bd[$nt][7],
            $bd[$nt][8],$bd[$nt][9],$bd[$nt][10],$bd[$nt][11],$bdnum[$nt];   
}
#    printf out3 "#NO.s  atom  atom  atom  Ktheta theta    kub    s0\n";
for($i=1;$i<=$nag;$i++){   ##write ang
    $kg=$i;
    $kg=99999 if($i>99999);
  printf ff "%5d %5d %5d %5d %7.3f %7.3f %7.3f %7.3f\n",
             $kg,$ag[$i][0],$ag[$i][1],$ag[$i][2],$ag[$i][3],$ag[$i][4],$ag[$i][5],$ag[$i][6];
}
for($nt=1;$nt<$nhbond;$nt++){
    printf ff "$hbnd[$nt]\n";
}
for($nt=0;$nt<$nRAM;$nt++){
    printf ff "$RAM[$nt]\n";
}
close(ff);
    `sync`;
}

##$m2=pPL1 is no template, else use template
sub genPRHB($m1,$m2){

$i=0;
undef %sec1;
undef %sec2;
undef %seq0;
if(-s "seq.dat" >20){
    print "use original predicted seq.dat\n";
#    `cp seq.dat seq0.dat`;
    open(seq,"seq.dat");
    while($line=<seq>){
	if($line=~/(\S+)\s+(\S+)\s+(\S+)/){
	    $i++;
	    $sec1{$i}=$3;
	}
    }
    close(seq);
}
##make seq0.dat
    `$path0/FAMR 8 $m1`;  ##produce seq0.dat base on  current structure m1(need full atomic model)
 #   system("wc seq0.dat");
    open(seq,"seq0.dat");
    $j=0;
    while($line=<seq>){
	if($line=~/(\S+)\s+(\S+)\s+(\S+)/){
	    $j++;
	    $seq0{$j}=$2;
	    $sec2{$j}=$3;
	}
    }
    close(seq);
#print "$path0/FAMR 8 $m1 seq0.dat >>\n";
    if(-s "$m2" >50){
	`$path0/FAMR 8 $m2`; 
#    system("wc seq0.dat");
	open(seq,"seq0.dat");
	$j=0;
	while($line=<seq>){
	    if($line=~/(\S+)\s+(\S+)\s+(\S+)/){
		$j++;
		$seq0{$j}=$2;
		$sec3{$j}=$3;
	    }
	}
	close(seq);
    }
`rm -f seq0.dat`;
    if($i>0 && $i != $j){
	printf "seq.dat is different to $seqk, $i,$j\n";
#	exit();
    }
    $Lch=$j;

    ####### decide $sec{$i}----------------->
	for($i=1;$i<=$Lch;$i++){
	    $sec{$i}=1;
	    if($sec1{$i} eq "2" || $sec2{$i} eq "2" || $sec3{$i} eq "2"){ #closc+seq
		$sec{$i}=2;
	    }
	    if($sec1{$i} eq "4" || ($sec2{$i} eq "4" || $sec3{$i} eq "4")){ #stick+seq
		$sec{$i}=4;
	    }
	}  
    ######## smooth------------------------->
    for($i=1;$i<=$Lch;$i++){
	if($sec{$i} ne "1"){
	    if($sec{$i-2} ne $sec{$i}){
	    if($sec{$i-1} ne $sec{$i}){
	    if($sec{$i+1} ne $sec{$i}){
	    if($sec{$i+2} ne $sec{$i}){
		$sec{$i}="1";
	    }
	    }
	    }
	    }
	}
    }    
    ####### decided SS fragments ----------->
    $na=0; #number of helix 
    for($i=1;$i<=$Lch;$i++){
	if($sec{$i} eq "2"){
	    if($sec{$i} ne $sec{$i-1}){
		$na++;
		$nai{$na}=$i;
	    }
	    if($sec{$i} ne $sec{$i+1}){
		$naf{$na}=$i;
	    }
	}
    }
    $nb=0; #number of strands
    for($i=1;$i<=$Lch;$i++){
	if($sec{$i} eq "4"){
	    if($sec{$i} ne $sec{$i-1}){
		$nb++;
		$nbi{$nb}=$i;
	    }
	    if($sec{$i} ne $sec{$i+1}){
		$nbf{$nb}=$i;
	    }
	}
    }
    
    ####################################################
    ###### identify sheet locations ------------------->
    $HB_cutp=6.8; #6.1; ##parellel
    $HB_cutap=6.1;
    $L_cut=1; ##3 for parellel only; changed here

    open(modd,"$m1"); #for distance of O-N for H-bond in sheet
	while($line=<modd>){
	    chomp $line;
	    if($line=~/^ATOM/){
		substr($line,12,4)=~/(\S+)/;
		$atom=$1;
		substr($line,22,4)=~/(\d+)/;
		$nr=$1;
		substr($line,6,5)=~/(\d+)/;
		$n_atom=$1;
		if($atom eq "N"){
		    $xN{$nr}=substr($line,30,8);
		    $yN{$nr}=substr($line,38,8);
		    $zN{$nr}=substr($line,46,8);
		}
		if($atom eq "O"){
		    $xO{$nr}=substr($line,30,8);
		    $yO{$nr}=substr($line,38,8);
		    $zO{$nr}=substr($line,46,8);
		}
		if($atom eq "CA"){  ##CAADDDDD
		    $xCA{$nr}=substr($line,30,8);
		    $yCA{$nr}=substr($line,38,8);
		    $zCA{$nr}=substr($line,46,8);
		}
		if($atom eq "C"){   ###CCCADDDDD
		    $xCC{$nr}=substr($line,30,8);
		    $yCC{$nr}=substr($line,38,8);
		    $zCC{$nr}=substr($line,46,8);
		}
	    }
	}
#	}
	close(modd);
        if(-s "$m2" >50){
	    open(combo,"$m2"); #for judge sheet based on CA-CA
	}else{
	    open(combo,"$m1"); #for judge sheet based on CA-CA, best is combo
	}
	$L=0;
	while($line=<combo>){
	    if(substr($line,12,4)=~/CA/){
		$L++;
		$xc{$L}=substr($line,30,8);
		$yc{$L}=substr($line,38,8);
		$zc{$L}=substr($line,46,8);
	    }
	}
	close(combo);
	##### decide parallel sheet ------------>
	$M_sheet=0;
	for($i=1;$i<$nb;$i++){ #stand1
	    next if($nbf{$i}-$nbi{$i}<3);
	    for($j=$i+1;$j<=$nb;$j++){ #strand2
		next if($nbf{$j}-$nbi{$j}<3);
		$n_sheet=0;
		undef %HL;
		undef %DIS;
		for($i1=$nbi{$i};$i1<=$nbf{$i};$i1++){
		    for($j1=$nbi{$j};$j1<=$nbf{$j};$j1++){
			$L=0;
			$dis_a=0;
		      pos60:;
			$pi=$i1+$L;
			$pj=$j1+$L;
			$mk=-1;
			if($pi>=$nbi{$i} && $pi<=$nbf{$i}){
			    if($pj>=$nbi{$j} && $pj<=$nbf{$j}){
				if(abs($pi-$pj)>=3){
				    $dis=sqrt(($xc{$pi}-$xc{$pj})**2+
					      ($yc{$pi}-$yc{$pj})**2+
					      ($zc{$pi}-$zc{$pj})**2);
				    if($dis<=$HB_cutp){
					$mk=1;
				    }
				}
			    }
			}
			if($mk>0){
			    $L++;
			    $dis_a+=$dis;
			    goto pos60;
			}
			$n_sheet++;
			$start{$n_sheet}="$i1 $j1";
			$HL{$n_sheet}=$L;
			$DIS{$n_sheet}=0;
			if($L>0){
			    $DIS{$n_sheet}=$dis_a/$L;
			}
		    }
		}
		@HL_keys=sort{$HL{$b}<=>$HL{$a}} keys %HL;
		$k=$HL_keys[0];
		if($HL{$k}>=$L_cut){
		    for($m=1;$m<=$n_sheet;$m++){ #check same HL but smaller DIS
			if($HL{$m} == $HL{$k}){
			    if($DIS{$m}<=$DIS{$k}){
				$k=$m;
			    }
			}
		    }
		    $M_sheet++;
		    $start{$k}=~/(\d+)\s+(\d+)/;
		    $residue_i{$M_sheet}=$1;
		    $residue_j{$M_sheet}=$2;
		    $L_sheet{$M_sheet}=$HL{$k};
		}
	    }
	}
	##### decide anti-parallel sheet ------------>
	$M_sheetR=0;
	for($i=1;$i<$nb;$i++){ #stand1
	    for($j=$i+1;$j<=$nb;$j++){ #strand2
		$n_sheet=0;
		undef %HL;
		undef %DIS;
		for($i1=$nbi{$i};$i1<=$nbf{$i};$i1++){
		    for($j1=$nbi{$j};$j1<=$nbf{$j};$j1++){
			$L=0;
			$dis_a=0;
		      pos70:;
			$pi=$i1+$L;
			$pj=$j1-$L;
			$mk=-1;
			if($pi>=$nbi{$i} && $pi<=$nbf{$i}){
			    if($pj>=$nbi{$j} && $pj<=$nbf{$j}){
				if(abs($pi-$pj)>=3){
				    $dis=sqrt(($xc{$pi}-$xc{$pj})**2+
					      ($yc{$pi}-$yc{$pj})**2+
					      ($zc{$pi}-$zc{$pj})**2);
				    if($dis<=$HB_cutap){
					$mk=1;
				    }
				}
			    }
			}
			if($mk>0){
			    $L++;
			    $dis_a+=$dis;
			    goto pos70;
			}
			$n_sheet++;
			$start{$n_sheet}="$i1 $j1";
			$HL{$n_sheet}=$L;
			$DIS{$n_sheet}=0;
			if($L>0){
			    $DIS{$n_sheet}=$dis_a/$L;
			}
		    }
		}
		@HL_keys=sort{$HL{$b}<=>$HL{$a}} keys %HL;
		$k=$HL_keys[0];
		if($HL{$k}>=$L_cut){
		    for($m=1;$m<=$n_sheet;$m++){ #check same HL but smaller DIS
			if($HL{$m} == $HL{$k}){
			    if($DIS{$m}<=$DIS{$k}){
				$k=$m;
			    }
			}
		    }
		    $M_sheetR++;
		    $start{$k}=~/(\d+)\s+(\d+)/;
		    $residueR_i{$M_sheetR}=$1;
		    $residueR_j{$M_sheetR}=$2;
		    $L_sheetR{$M_sheetR}=$HL{$k};
		}
	    }
	}
    ########## generate $RES --------------------------->
printf "write PRHB alpha/beta pitches: $na,$nb, $M_sheet, $M_sheetR\n";
    open(hh,">PRHB");
$ssmd=0;
$ssmd=1 if($contrl eq "8" || $contrl eq "81");
    open(ss,">sspredict") if($ssmd>0);
    for($i=1;$i<=$na;$i++){
	printf ss "        rsr.add(secondary_structure.alpha(self.residue_range('$nai{$i}:', '$naf{$i}:')))\n" if($ssmd>0);
    }
    for($i=1;$i<=$nb;$i++){
#	printf " $i       rsr.add(secondary_structure.strand(self.residue_range('$nbi{$i}:', '$nbf{$i}:')))\n" if($ssmd>0);
	printf ss "        rsr.add(secondary_structure.strand(self.residue_range('$nbi{$i}:', '$nbf{$i}:')))\n" if($ssmd>0);
    }
    $NHB=0;
    ###### alpha helix ----------->
    for($i=1;$i<=$na;$i++){
	$i0=$nai{$i}-1;
	$i1=$naf{$i}+1;
	$i0=1 if($i0<1);
	$i1=$Lch if($i1>$Lch);
	for($j=$i0+2;$j<=$i1+1;$j++){
	    $k=$j-4;
	    if($k>0 && $j<=$Lch){
		$NHB++;
		printf hh "$NHB N $j O $k\n";
	    }
	}
    }
    ###### beta strands ---Parellel------->
    for($i=1;$i<=$M_sheet;$i++){
	$L=$L_sheet{$i}-1;    ######V0 changed
	goto ppsheet if($L<3);  ##for parellel
	printf "p-sheet:: $i $L, $residue_i{$i}\n";
	$n=0;
	$dis1=0;
	for($j=1;$j<=$L;$j++){
	    $k=2*$j-1; #1,3,5
	    $I=$residue_i{$i}+$k;
	    $J1=$residue_j{$i}+$k-1;
	    $J2=$residue_j{$i}+$k+1;
	    $n++;
	    $hall{1,$n}="N $I O $J1";
	    $dis1+=sqrt(($xN{$I}-$xO{$J1})**2+
			($yN{$I}-$yO{$J1})**2+
			($zN{$I}-$zO{$J1})**2);
	    goto pos61 if($n>=$L);
	    $n++;
	    $hall{1,$n}="N $J2 O $I";
	    $dis1+=sqrt(($xO{$I}-$xN{$J2})**2+
			($yO{$I}-$yN{$J2})**2+
			($zO{$I}-$zN{$J2})**2);
	    goto pos61 if($n>=$L);
	}
      pos61:;
	$dis1/=$L;
	######### suppose O(i)-N(j+1):
	$n=0;
	$dis2=0;
	for($j=1;$j<=$L;$j++){
	    $k=2*$j-1; #1,3,5
	    $I1=$residue_i{$i}+$k-1;
	    $I2=$residue_i{$i}+$k+1;
	    $J=$residue_j{$i}+$k;
	    $n++;
	    $hall{2,$n}="N $J O $I";
	    $dis2+=sqrt(($xO{$I1}-$xN{$J})**2+
			($yO{$I1}-$yN{$J})**2+
			($zO{$I1}-$zN{$J})**2);
	    goto pos62 if($n>=$L);
	    $n++;
	    $hall{2,$n}="N $I2 O $J";
	    $dis2+=sqrt(($xN{$I2}-$xO{$J})**2+
			($yN{$I2}-$yO{$J})**2+
			($zN{$I2}-$zO{$J})**2);
	    goto pos62 if($n>=$L);
	}
      pos62:;
	$dis2/=$L;
#	printf "beta $residue_i{$i} :: $residue_i{$i} $L\n";
	if($dis1<$dis2){ ######### suppose N(i+1)-O(j):
	    for($q=1;$q<=$L;$q++){
		$NHB++;
		printf hh "$NHB $hall{1,$q}\n";
	    }
	    $n=$residue_i{$i}+1;
	    $o=$residue_j{$i};
	    printf ss "        rsr.add(secondary_structure.sheet(at['N:$n'], at['O:$o'],sheet_h_bonds=$L))\n" if($ssmd>0);
	}else{           ######### suppose O(i)-N(j+1):
	    for($q=1;$q<=$L;$q++){
		$NHB++;
		printf hh "$NHB $hall{2,$q}\n";
	    }
	    $o=$residue_i{$i};
	    $n=$residue_j{$i}+1;
	    printf ss "        rsr.add(secondary_structure.sheet(at['O:$o'], at['N:$n'],sheet_h_bonds=$L))\n" if($ssmd>0);
	}
      ppsheet:
    }
    ###### anti-parallel beta sheets (same residues)------Anti-parellel----->
    for($i=1;$i<=$M_sheetR;$i++){
	$L=$L_sheetR{$i};
	$L-- if($L>1);
	$n=0;
	$dis1=0;
#	printf "Ap-sheet:: $i $L, $residueR_i{$i} :: $residueR_j{$i}\n";
	for($j=1;$j<=$L;$j++){
	    $k=2*$j-1; #1,3,5
	    $I=$residueR_i{$i}+$k;
	    $J=$residueR_j{$i}-$k;
	    $n++;
	    $hall{1,$n}="N $I O $J";
	    $dis1+=sqrt(($xN{$I}-$xO{$J})**2+
			($yN{$I}-$yO{$J})**2+
			($zN{$I}-$zO{$J})**2);
	    goto pos71 if($n>=$L);
	    $n++;
	    $hall{1,$n}="N $J O $I";
	    $dis1+=sqrt(($xO{$I}-$xN{$J})**2+
			($yO{$I}-$yN{$J})**2+
			($zO{$I}-$zN{$J})**2);
	    goto pos71 if($n>=$L);
	}
      pos71:;
	$dis1/=$L;
	######### suppose O(i)-N(j):
	$n=1;
	$I=$residueR_i{$i};
	$J=$residueR_j{$i};
	$hall{2,$n}="N $J O $I";
	$dis2=sqrt(($xO{$I}-$xN{$J})**2+
		   ($yO{$I}-$yN{$J})**2+
		   ($zO{$I}-$zN{$J})**2);
	goto pos72 if($n>=$L);
	for($j=1;$j<=$L;$j++){
	    $k=2*$j; #2,4,6
	    $I=$residueR_i{$i}+$k;
	    $J=$residueR_j{$i}-$k;
	    $n++;
	    $hall{2,$n}="N $I O $J";
	    $dis2+=sqrt(($xN{$I}-$xO{$J})**2+
			($yN{$I}-$yO{$J})**2+
			($zN{$I}-$zO{$J})**2);
	    goto pos72 if($n>=$L);
	    $n++;
	    $hall{2,$n}="N $J O $I";
	    $dis2+=sqrt(($xO{$I}-$xN{$J})**2+
			($yO{$I}-$yN{$J})**2+
			($zO{$I}-$zN{$J})**2);
	    goto pos72 if($n>=$L);
	}
      pos72:;
	$dis2/=$L;
	if($dis1<$dis2){ ######### suppose N(i+1)-O(j-1):
	    for($q=1;$q<=$L;$q++){
		$NHB++;
		printf hh "$NHB $hall{1,$q}\n";
	    }
	    $n=$residueR_i{$i}+1;
	    $o=$residueR_j{$i}-1;
	    printf ss "        rsr.add(secondary_structure.sheet(at['N:$n'], at['O:$o'],sheet_h_bonds=-$L))\n" if($ssmd>0);
	}else{           ######### suppose O(i)-N(j):
	    for($q=1;$q<=$L;$q++){
		$NHB++;
		printf hh "$NHB $hall{2,$q}\n";
	    }

	    $o=$residueR_i{$i};
	    $n=$residueR_j{$i};
	    printf ss "        rsr.add(secondary_structure.sheet(at['O:$o'], at['N:$n'],sheet_h_bonds=-$L))\n" if($ssmd>0);
	}
    }
    close(hh);
    close(ss);
  pos80:;
#   open(sq2,">seq2.dat");  ##concensus seq
#   for($i=1;$i<=$Lch;$i++){
#	printf sq2 "%5d %5s %4d %4d\n",$i,$seq0{$i},$sec{$i},9;
#   }
#   close(sq2);
}

##shift this sub to shift the coordinates to smaller value
sub shift($fg){
#    print "shift $fg\n";
  open(tt,"$fg");
  $i=0;
  $xx=0;
  $yy=0;
  $zz=0;
  undef %x;
  undef %y;
  undef %z;
  undef %a;
  undef %b;
  undef %c;  
  while($line=<tt>){
      chomp $line;
      if($line=~/^ATOM/){ 
	  $i++;
	  $x{$i}=substr($line,30,8);
	  $y{$i}=substr($line,38,8);
	  $z{$i}=substr($line,46,8);	
	  $xx+=$x{$i};
	  $yy+=$y{$i};
	  $zz+=$z{$i};
	  $fu[$i]=substr($line,0,29);
      }    
  }   ##logfile read finish
  close(tt);
  @a=sort{$x{$a}<=>$x{$b}} keys %x;
  @b=sort{$y{$a}<=>$y{$b}} keys %y;
  @c=sort{$z{$a}<=>$z{$b}} keys %z;
  goto skp if($x{$a[0]}> -999 && $y{$b[0]}> -999 && $z{$c[0]}> -999 && $x{$a[$i-1]}< 990 && $y{$b[$i-1]}< 990 && $z{$c[$i-1]}<990);
#  printf "$fg :: $a[0] $b[0] $c[0] :: $a[$i-1] $b[$i-1] $c[$i-1] \n";

  $xx=$xx/$i-50;
  $yy=$yy/$i-50;
  $zz=$zz/$i-50;
  open(pdb,">$fg");
  for($j=1;$j<=$i;$j++){
      printf pdb "%29s %8.3f%8.3f%8.3f\n",$fu[$j],$x{$j}-$xx,$y{$j}-$yy,$z{$j}-$zz;
  }
  printf pdb "%-3s\n","TER";
  close(pdb);
 skp:
}
