!! Dear Dr. Zhang:
!! Recently I have updated the REMO fortran code so it can be run in MAC, all of them are in I/O part.
!! Please feel free to update the file in the server. Thank you!
!! Yunqi
!!
!!this program to build full atom model and assess secondary structure and Hbond
!!generate sspredict and PRHB2, trying to combine to the restraints of modeller
!!3 Apr 2008, add the part to add backbone atoms is input is CA or too many clash (control 11)
!May 26 2008, add the path for BBdat
!May 28 2008, add subroutine CA_build  to make up CA is missing
!June 16, get rid of rotamer
!25 June, adjust the control number, reduce from 11 to 8
!26 June, add side chain opt subroutine
!17 July, modify SD_opt to optimize sidehcain, add hydrophilic as packing function
!31 July add new KB energy term for backbone CA and NC contact
!17 Aug Add all the relaxation methods, HBrotate just one kind (BB only)

!subroutines
!!invoved in backbone (CA movement)
!CA_build   ##build missing CA from fasta
!BBADD(iflag) ##0, keep bb geavy atoms, 1 build be default, 2 search backbone isotamer
!HBrotate(iflag) ##1 move CA to opt HBond, 2 keep CA fixed, change phi/psi plane (core sub for CA movement, Monte Carlo)
!calclash  ##clculate number of clash and broken
!move_clash(fk,nkchbr) ##move Ca to reduce clash and broken

!!involved in sidechain optimization
!CB_ADD     ##to build CB atoms using KB information
!SG_ADD    ##add side chain center, same idea of CB_ADD
!HEAVY_ADD ##ADD other missing heavy atoms (defaut add side chain heavy atoms using stable length and angle
!CABrot(ires,angg) ##rotate side chain of ires around Ca-Cb bond
!CBGrot(ires,angg) ##rotate side chain of ires around Cb-Cg bond
!CGDrot(ires,angg) ##rotate side chain of ires around Cg-Cd bond
!CBZrot(ires,angg) ##rotate side chain of ires around Cb-Rterm bond
!SDclashscan ##calculate side chain atom clash (<2.0A) 
!SD_opt1    ##adjust the bond length and angle to standard value from CHarmm22
!SD_opt     ##optimize side chain orientation by search rotamer, reduce atom clash and distance to SG (17 July 2008)


!!involved in hydrogen atoms only
!HADD      ##ADD hydrogen atoms
!AD_1H     ##optimize position of 1H case in O-H, S-H

!!functions and energy calculatation
!IMERGE(iflag) ##1, imerge and shift; 2 only imerge
!angle     ##calaulate phi/psi/chi1/chi2
!Hbonds(iflag) ##1 all Hbond, 2 only backbone Hbond
!ass2nd   ##identify secondary structure based on Ramchadran plot (trained by Stride)
!restr_stru ##generate secondary struicture and Hbond restraints
!sort #sort in increasing order
!sortindex ##sort and output the increasing order array index
!rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,an) ##rotate (x,y,z) with origin (ax,ay,az) around axis (xi,yi,zi) an
!dihe_rotate(ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz,dhang,bcang) ##rotate (x,y,z) with origin (ax,ay,az) around axis (xi,yi,zi) an
!gaussj(A,B) ##Gaussian-Jordan method solve coefficient matrix A, whith results B
!CARMSD calculate Ca RMSD in xx and bbkxx
!rest_tasser ##calculate TASSER restraints (no sidechain contact)
!u3b(w,x,y,n,mode,rms,u,t,ier) ##rotata x to y, matrix u and t

program main
implicit none
INTEGER,PARAMETER::La=360  !1.0A mesh, 640A large !La is the matrix length and edge of matrix
integer,parameter::mang=200000  !!maximum number of angles
integer,parameter::Merot=26000 !!number of rotamer
integer,PARAMETER::NMAX=50000   !!max atom number
integer,PARAMETER::Lmax=3000   !!max residue number
integer,parameter::NSITT=10   !!maximum clash on the same sites
integer,parameter::km14=25,km13=10,km24=10,kmpx=4,kmpy=10,kmnr=10
integer,parameter::Nchild=5  !!for caca rot
integer,parameter::NLMAT=250  !!LMProt  attmpts, must consider atoms

real,parameter::rmesh=0.5 !!reverse of mesh size  20 Nov changed
REAL,PARAMETER::pi=3.1415926,hpi=0.0174533,vpi=4.188791,pi05=0.087266463,pi10=0.174532925   !pi and pi/180.0,and 4pi/3
real,parameter::dismin=1e-6,dismin2=1e-3  !!the minimum cutoff 
real,parameter::rHAcutmin=1.65,rHAcut=2.6,rDAcut=3.9,rDAcutmin=1.36,DHAcut=90.0,HAAbcut=90.0,dHAAx=20.0  !!HBOND
real,parameter::rcachcut=12.96,rcabr=16.81   !!CACA CLACH broken cutoff
real,parameter::dtrl=0.97,dtrh=1.03,dlmp=0.1  !!for LMprot
integer,parameter::Lang=45 !the start for each residue is in [45*(resd(iatom)-1),75*(resd(iatom)-1))
integer,parameter::NRamch=1369  !!37^2 binwidth of Ramchran plot, 10o bin (same as rotamer)
integer,parameter::Ncytotal=180
!integer,parameter::Merot=224000 !!number of rotamer
real,DIMENSION(0:NMAX)::xx,yy,zz,xx0,yy0,zz0,bkxx,bkyy,bkzz,bbkxx,bbkyy,bbkzz,sxx,syy,szz !1 final coordinates
real,DIMENSION(NMAX,3)::ATMPAR  !record charge density,vdwE,vdwR
real,DIMENSION(NMAX,8)::BOND    !record con1E,con1R ... con4E,con4R
real,dimension(NMAX,4)::ANG     !record ktheta theta0 kUB S0 angle and UB energy
real,dimension(Lmax,4)::phipsichi  !!Rotamer angle,phi,psi,chi1..chi4
real,dimension(80,4)::H_BOND  !!1, for rha, index:(r-1.4)/0.02; 2,3 <DHA & < HAAb, (ang-20.0)/2.0 degree
real,dimension(Lmax,4)::htbon  !!Hbond invovled parameters, 1 R_HA   2 <DHA 3<HAAb 4 hbond_E
integer,dimension(Lmax,2)::hbn !!donor and acceptor

integer,dimension(50)::HH1,indx !!this record the atoms index for 1H within 5A, indx the sorted index
real,dimension(50)::rHH1 !!this for sorting index

real,dimension(Lmax)::caxx,cayy,cazz  !!used for CA loop build

character*1,dimension(NMAX)::ATY   !!record the atom type, H, N O,S, C
CHARACTER*4,DIMENSION(NMAX)::ATOMTY !RECORD INPUT ATOMNAME
character*3,dimension(NMAX)::resd   !record the residue sequence name
character*3,dimension(NMAX)::resd2   !record the residue sequence name
CHARACTER*4,DIMENSION(NMAX,2)::ATOM !record atomNAME, connnect type
character*4,dimension(20)::afn      !full name of 20 amino acids
character*30,dimension(NMAX)::fu    !for I/O out put

integer,dimension(20)::hra,ra      !record number of heavy atoms and atoms in each residue
real,dimension(20)::sdhro,rsdr !rsdr1,rsdr2  !!residue specific hydrophobic/hydrophilic hydropathy index (two *, higher is better), and side chain radius
integer,DIMENSION(La,La,La)::SITE   !descrete mesh
integer,DIMENSION(NMAX,NSITT)::SITT !record clashes, 4 at most, otherwise(warning, 0.1natom)!!SITT has same index of IP(iatom)
integer,dimension(NMAX,3)::NZR !!!record the non-zero point in SITE, for zeroing

INTEGER,DIMENSION(NMAX)::ATMHB    !record Hbond, 1(1 accptor,-1,donor) label
!!3, 0 for no form, >0.5 to record on ip(onyl useful on H) of the acceptor
integer,DIMENSION(NMAX,6)::bonda    !atom connect and 5 number of bonds(doube bond= -1),6 heavy atom bonded number
integer,DIMENSION(Mang,3)::anga     !atom1 atom2 atom3
integer,dimension(NMAX)::ress,ress2       !record residue series number
integer,dimension(Lmax,2)::ca      !first record Ca atom index, 2nd record residue type in 20
integer,dimension(NMAX,0:15)::bnd4  !!record current atom bonded four neighbour bond, 3 residues at most
integer,dimension(NMAX)::ASSrd  !!!record which atoms are added by this program

integer,dimension(Lmax,2)::seq    !2ndst predict, 1(0)coil,2(1)Helix,4(-4)strand;2 /10 prob. #
integer,dimension(Lmax,4)::seq2 !1 type, 2 pitch index, 3 4 start residue, end residue
real,dimension(NRamch,2)::RAMchra  !!record the probability to be helix or sheet from RAMchran plot

!real,dimension(Merot,6)::rotamer !!rotamer library 1,phi 2 psi 3 prob,4~7 chi1~4,8~11 bias
!integer,dimension(20,2)::nrotamer  !1 record the number in each amino acid, 2 record the start number in rotamer lib 2+1,1
!integer,dimension(Lmax)::rotind  !!record the rotamer index

real,dimension(Lmax)::sgx,sgy,sgz  !!side chain mass center

!!the following for backbone build
integer,dimension(0:km14,0:km13,0:km24,0:kmpx,0:kmpy,1:20,1:kmnr)::bbdata,bbdatb,bbdatc !!helix,sheet and coil
integer,dimension(Lmax)::hbvar  !!0 need move, 2 keep frozen
integer,dimension(Lmax)::bbindex !!record the index used in last time
!real,dimension(242400,6)::bbcoord
real,dimension(70000,6)::bbcoord
integer,dimension(Lmax,3)::reshbn

real,dimension(Lmax,3)::bvv,bvv2   !!for relaxtion
real,dimension(Lmax)::CArmsdd,CArmsdd0  !!record the rmsd value for each residue

real,dimension(1076400,4)::bbcontact  !!CA NC contact 1 6.8A, 2 5.8A, 3 (i<j,Ni-Cj) 4 Nj-Ci (3.9A)

real,dimension(Lmax)::cbca,bkcbca  !!for CA contact score combCA.dat
integer,dimension(NMAX,2)::combsca  !!record CA contacts (>5 residues, <6.0A)
real,dimension(NMAX)::pcombca  !!record CA contacts,probability (>5 residues, <6.0A)
integer,dimension(Lmax)::numclash  !!record the clashed/brokedn atoms
integer,dimension(NMAX,2)::rsd  !record CA distance [2,6] residues
integer,dimension(NMAX,2)::rsdL    !record CA distance no less than 10 residues
real,dimension(NMAX,2)::drsd     !record CA distance [2,6] residues
real,dimension(NMAX)::drsdL    !record CA distance no less than 10 residues per 10 residues
integer,dimension(Nmax)::tkfp,tkfpres  !!for tranfer

!integer,dimension(20,2)::nrotamer  !1 record the number in each amino acid, 2 record the start number in rotamer lib 2+1,1
!real,dimension(Merot,11)::rotamer !!rotamer library 1,phi 2 psi 3 prob,4~7 chi1~4,8~11 bias
integer,dimension(Lmax)::sdclash,sdclash0
real,dimension(Lmax)::sgd,sgd0  !!difference to SG

real,dimension(6)::ddCa   !!!LMPROT parameters
real,dimension(4)::ddN,ddC
integer,dimension(3,3)::relax  !!record the time for each relaxation methods

double precision rr00(3,NMAX),rr11(3,NMAX)  !!this for U3B
double precision w(NMAX)
double precision u(3,3),t(3),rms

    CHARACTER*100 SS,du,duu
    CHARACTER*54 kfp(Nmax)
    character*4 abc,prtype
    character bb
    character*4 icontrol
    character*100 f1,f2,f3,f4  !!the template file name(only use Ca)
    character*2 tp1,tp2  !!Hbond type
    character*2 ssclass   !!aa alpha protein, bb beta protein, ab a/b protein

real shiftx,shifty,shiftz
real dx,dy,dz
real rk,Rg

real vr0(3),vr1(3,3)
real ax,ay,az,xi,yi,zi,x,y,z,r
real angg,fbd,bd,r1
real ehbond,draa  !!Hbond energy
real r12,r23,r13,bx,by,bz,cx,cy,cz
real r2,r3,phi,tab,tac,tbc,t1,ctbcx,ctbcy,ctbcz,ctbax,ctbay,ctbaz,t2
real faredu  !!prefactor for decresing of move CA
real rclash,rbroken
real RMSD,dRMSD,RMSD0,RMSDcut
real e0,ee0,ee,de,eg0,feg  !!for Metropolis rule
real engk,egapHBm,egapHB,etasser
real fhb,fRMSD,ffrozen,ftasser  !!decide the weight of prediction HB list, helix higher, beta lower
real energy

real rcacut
real cashortk,calongk    !!Ca short and long distance restraints, L dependence, setted in relax
real cacontactk,sdcontactk    !!energy prefactor for CA contact
real cadist,cadistL,cacontact
real sdgg,sdgg0   !!the SG distance deviation, an energy term to select side chain
real ebbcon,eclash,seng0  !bb contact and clash break energy
integer ipp(3),ipp0(3)
integer ires,iatom,ip1,in,iw
integer ix,iy,iz,ix0,iy0,iz0,lxmin,lymin,lzmin,lxmax,lymax,lzmax,ip,kat
integer index_r,index_rk,ia1,ia2,ia3,ia4
integer ia,ib,ic,iflag,ir1,ir2
integer natom0  !!only heavy atoms
integer i,j,k,ii,kx,kk,kx0,j1
integer icc,ip2,ip3,ip4,ins,ipc2,ipc3
integer NNZR,Ncaclash,Ncaclash0,Nbroken  !!Ncaclash only consider the clash among Ca atoms
integer Nhbonds
integer LintH       !!cut off of vdw(ele) and Hbond
integer Nfbc,Ledge     !the side length of bondy simulation lattice and the edge,smallest box for one residue
integer NMAXx,NMAXy,NMAXz    !the largest box used in simulation, free boundary condition used:impetrable
integer nres,natom,nang,MHbond !number of atoms, angles, hbond bin width
integer ires0,ires1   !!!local residue assign and energy calculation use
integer iseed
integer n1,na,nb,nc,ncsg,nasg,nbsg,mmhb0,naa,nbb
integer i1,i2,i3,i4,i5,i6,i7,i8
integer Nreshb0,nhbhit
integer icy,labelcycle,Nsuc
integer NcombCA,Ndist,NdistL,longca,longcam
integer NHB0,NHB1,NHB2,NHB3  !!!record three strudture, which has the most backbone Hbond, which will be the final model
integer ier !!for u3B
integer Nmerot,Maxrot !!maxline of rotamer

integer nsdclash,nsdclash0
integer nhbcy  !!number of iterations to optimize Hbond
logical tarest,templatelog,PRHBlog  !!true for tasser restraints availuable, else not use
logical tasserlog,bbconlog !!when true, move any Ca
logical Mlog !!log for whether Lmprot sucess
logical CAbdlog  !!CA_build log, 
      data afn/'GLY','ALA','SER','CYS','VAL','THR','ILE','PRO','MET','ASP','ASN','LEU','LYS','GLU','GLN','ARG','HIS','PHE','TYR','TRP'/
      data hra/4,5,6,6,7,7,8,7,8,8,8,8,9,9,9,11,10,11,12,14/   !!heavy atoms
      data ra/7,10,11,11,16,14,19,14,17,12,14,19,22,15,17,24,17,20,21,24/  !!whole atoms
      data sdhro/-0.4,1.8,-0.8,2.5,4.2,-0.7,4.5,-1.6,1.9,-3.5,-3.5,3.8,-3.9,-3.5,-3.5,-4.5,-3.2,2.8,-1.3,-0.9/
!      data rsdr1/0.0,1.505,1.766,1.904,1.902,1.871,2.235,1.785,2.623,2.177,2.179,2.425,3.059,2.709,2.711,3.514,2.456,2.640,2.805,2.832/   !!angle <105o side chain center to CA distance
!      data rsdr2/0.0,1.493,1.738,1.866,1.893,1.844,2.220,1.820,2.710,2.126,2.085,2.432,3.064,2.801,2.758,3.538,2.441,2.611,2.797,2.926/
      data rsdr/0.0,1.499,1.752,1.885,1.897,1.857,2.228,1.803,2.667,2.151,2.132,2.428,3.061,2.755,2.735,3.526,2.448,2.625,2.801,2.879/
      data ddCa/14.5161,14.5161,5.81753,5.77275,2.0449,0.0/   
      !!ddN CA-N, N-C, N-Ca,C(-1)-b-N
!      data ddN/5.81753,5.51091,2.0449,1.809/
      data ddN/5.81753,5.51091,1.809,0.0/
      !!ddC N-C, C-CA',c-b-N(+1),Ca-b-C
!      data ddC/5.51091,5.77275,1.809,2.2201/
      data ddC/5.51091,5.77275,2.2201,0.0/
!!!for LMPROT movement
      
 f1=''
call getarg(1,icontrol)
call getarg(2,f1)

if(len(trim(f1))<2)then
print*,"USING Guide::"
print*,"./FAMR 1  xx.pdb (ADD H only, check xx.pdb.h)"
print*,"./FAMR 2  xx.pdb BBdat (ADD backbone, CA fixed, one output xx.pdb.h, HB opt)"
print*,"./FAMR 23 xx.pdb BBdat ( keep other part fixed,ADD backbone and H xx.pdb.h,fast)"
print*,"./FAMR 3  xx.pdb BBdat [template] (same as 2, add template, CA flexible,out xx.pdb.fix and xx.pdb.h)"
print*,"./FAMR 31 xx.pdb BBdat [template] (based on Hdd, build missing CA, and then same as 3)"
print*,"./FAMR 4  xx.pdb BBdat [template] (same as 3, max template restraints)"


print*,"./FAMR 5  xx.pdb (Hbond list (on screen) and chi1 file phi/psi/chi1/chi2 list)"
print*,"./FAMR 6  xx.pdb (ADD H and count H-bond,mkrestraints:  PRHB2 HBlist)"
print*,"                 (input: seq.dat, PRHB, xx.pdb; output: PRHB2)"
print*,"./FAMR 66  xx.pdb (same as 6, but use Hbondlist from current structure only)"
print*,"./FAMR 7   xx.pdb (chi1 and chi2 only, without set as 0)"
print*,"./FAMR 8   xx.pdb (output seq0.dat based on phi/psi)"
goto 777
endif

!iseed=-7
!rmsdcut=1.0 !1.2  !!the maximum of RMSD change compare with initial
labelcycle=0
ledge=30           !!default edge length in simulation lattice
relax=0
      LintH=max(1,Nint(3.2*rmesh))  !is 4.0A/rmesh of the distance between H-donor
      Nfbc=La-ledge           !!the length atoms can reach
!!read pdb structure *************************FULL ATOMIC***************************************
    open(1,file=trim(f1))   !!!only for test
        in=0
        ir1=1
 501 format(A100)
30  read(1,501,end=31)ss     !!F1 structure file
      if(ss(1:3).eq.'END'.or.ss(1:3).eq.'TER') goto 31
      bb=ss(17:17)
      if(ss(1:4).eq.'ATOM'.and.(bb.eq.' '.or.bb.eq.'A'))then
         in=in+1
         if(icontrol.eq."2".or.icontrol.eq."3".or.icontrol.eq."4".or.icontrol.eq."23".or.icontrol.eq."31")then
            read(ss,101)du,atomty(in),duu,resd(in),duu,ir2,duu,xx0(in),yy0(in),zz0(in)  
            ress(in)=ir2
         else
            read(ss,101)du,atomty(in),duu,resd(in),duu,ir2,duu,xx0(in),yy0(in),zz0(in)     
            if(icontrol.eq."1")then
  502 format(A30)
               read(ss,502)kfp(in)
               tkfpres(in)=ir2
            endif
            if(ir2>ic.and.in>1)ir1=ir1+1
            ress(in)=ir1     
            ic=ir2
         endif
      endif
      goto 30
31   continue   
      close(1)
      if(in<10)then
         print*,"read pdb file err, total atoms",in
         stop
      endif
      natom0=in
!!******************************READ chmm.dd *****************************************************************
    open(10,file="Hdd")
    read(10,*)natom,nang,MHBOND

    kk=0
    ins=0
    do i=1,natom
!!                        atomname contype    residue     chargeden    vdwE       vdwR        HBtype HB connect
       read(10,102)i2,atom(i,1),atom(i,2),resd2(i),atmpar(i,1),atmpar(i,2),atmpar(i,3),atmhb(i) 
       aty(i)=atom(i,1)(2:2)
       if(atom(i,1)==' N  ')ins=ins+1
       ress2(i)=ins       !residue series
       if(atom(i,1)==' CA ')then
          ca(ress2(i),1)=i
          do j=1,20
             if(resd2(i).eq.afn(j))then
                ca(ress2(i),2)=j  !!residue index, 1~20
                goto 120
             endif
          enddo
120       continue
       endif
    enddo
Nres=ress2(natom)

!print*,"Nres",ins,ca(ins-1,1),ca(ins-1,2)
if(icontrol.eq."7")goto 772  !!only calculate angle

    do i=1,natom
!!#            #es_No.  con1  con2  con3   con4  con1E  con1r  con2E   con2r   con3E  con3r  con4E  con4r nbonds        
       read(10,103)i2,bonda(i,1),bonda(i,2),bonda(i,3),bonda(i,4),bond(i,1),bond(i,2),bond(i,3),bond(i,4),bond(i,5),bond(i,6),bond(i,7),bond(i,8),bonda(i,5)
    enddo
    do i=1,nang
!!#  "#NO.s   atom  atom  atom  Ktheta theta    kub    s0              
       read(10,104)i2,anga(i,1),anga(i,2),anga(i,3),ang(i,1),ang(i,2),ang(i,3),ang(i,4)
    enddo

   do i=1,MHBOND   !dha          !<DHA      <HAAb           Chi
      read(10,*)rk,H_BOND(i,1),rk,H_BOND(i,2),H_BOND(i,3),rk,H_BOND(i,4)
   enddo

   if(icontrol.ne."1")then
!!read ramachran data
      do i=1,Nramch   !index phi psi  p_helix  p_sheet
         read(10,*)rk,rk,rk,RAMchra(i,1),RAMchra(i,2)
      enddo
   endif
    close(10)   !!CHARMM parameter file read finish


101 format(A12,A4,A1,A3,A2,I4,A4,3F8.3)   !pdb
102 format(I4,1X,2(A4,1X),A3,2X,F6.3,1X,2(F7.4,1X),I4)  !atm
103 format(I4,1X,4(I6,1X),4(F8.3,1X,F7.4,1X),I2)  !!bnd
104 format(4(I5,1X),3(F7.3,1X),F7.3)  !ang
Nhbonds=0

772 continue

xx=999.0
yy=999.0
zz=999.0
tkfp=0
do i=1,natom  !!should be charmm atm file
   do j=1,natom0
      if(atomty(j).eq.atom(i,1).and.resd(j).eq.resd2(i).and.ress(j).eq.ress2(i))then
         xx(i)=xx0(j)
         yy(i)=yy0(j)
         zz(i)=zz0(j)
!write(*,"I5,A5,A4,I4,3f8.3"),j,atomty(i),resd(i),ress(i),xx(i),yy(i),zz(i)
         tkfp(i)=j
         goto 11
      endif
   enddo
11 continue
enddo

do i=1,natom
      kx0=0
   do k=1,bonda(i,5)
      ii=bonda(i,k)
      if(aty(ii).eq.'H')cycle
      kx0=kx0+1
   enddo
   bonda(i,6)=kx0  !number of heavy atoms bonded
   ress(i)=ress2(i)
   atomty(i)=atom(i,1)
   resd(i)=resd2(i)
enddo
!do i=1,Nres
! print*,"res",i,ca(i,1),resd(ca(i,1)),ca(i,2)
!enddo
!pause


Assrd=0
NNZR=0
iseed=natom 

call IMERGE(0)

if(icontrol.eq."1")then
   call HADD
!!!20 Nov add for EV statistics
!   call EVcon
!!
   f2=trim(f1)//".h"
   open(10,file=f2)
   do i=1,natom
      k=tkfp(i)
      if(xx(i)>990.0)cycle
      if(k>1)ir2=tkfpres(k)
      if(aty(i).eq."H")then
         write(10,129),0,atomty(i),resd(i),ir2,xx(i),yy(i),zz(i),ASSrd(i)         
      elseif(k>0)then
503 format(A30,3f8.3)
         write(10,503),kfp(k),xx(i),yy(i),zz(i)
      endif
   enddo
   write(10,119)
   close(10)
   goto 777 
endif
!print*,"NRES",Nres
if(icontrol.eq."7")then
call angle
goto 777
endif
if(icontrol.eq."8")then
call angle
call ass2nd
goto 777
endif

if(icontrol.eq.'2'.or.icontrol.eq.'23'.or.icontrol.eq.'3'.or.icontrol.eq.'4'.or.icontrol.eq.'31')then
!print*,"select run>>>>",icontrol
   if(icontrol.eq.'31')call CA_build !!check whether need to build CA
!print*,"CAbuild done"
!!decide protein class ************************************

   if(icontrol.ne.'2'.and.icontrol.ne."23")then
      k=0
      open(85,file="seq.dat",status="old",err=85)    !2nd structure identification
      naa=0
      nbb=0
      do i=1,Nres   !seq.dat  !!1 coil, 2 helix, 4 sheet
         read(85,*,err=85)k,abc,seq(i,1),seq(i,2) 
      enddo
85    close(85)
      do i=1,Nres   !seq.dat  !!1 coil, 2 helix, 4 sheet
         if(seq(i,1).eq.2)then
            naa=naa+1
         elseif(seq(i,1).eq.4)then
            nbb=nbb+1
         endif
      enddo
      if(naa>0.1*Nres.and.nbb<0.1*Nres)then
         ssclass="aa"
         fhb=1.0 !0.73
      elseif(nbb>0.1*Nres.and.naa<0.1*Nres)then
         ssclass="bb"
         fhb=0.67
      else
         ssclass="ab"
         fhb=1.0 !0.74
      endif
      if(k<Nres)then
         print*,"WARNING!! seq.dat is different to the structure, not use seq instead"
         !       stop
      endif
   endif  !!ne 2

!!2, 3, 4
!!!!get the type, hard,medm and easy***************************************************************
      prtype="easy"
      open(80,file="init.dat",status="old",err=80)  !read target type
      read(80,*,err=80)j,prtype  !!easy medm hard
80    close(80)

      call getarg(3,f3)  !!path of BBdat
!print*,"begin to read BBdat>>>",f3
      open(10,file=trim(f3),err=77)  !!BBdat
      read(10,*)n1,nc,na,nb
      do ii=1,n1
         read(10,810),i7,i1,i2,i3,i4,i5,i6,i8,(bbcoord(ii,j),j=1,6)
   !      read(10,810),i7,i1,i2,i3,i4,i5,i6,i8,(bbcoord(ii,j),j=1,12)
         if(i7.eq.1)then
            bbdatc(i1,i2,i3,i4,i5,i6,i8)=ii
         elseif(i7.eq.2)then
            bbdata(i1,i2,i3,i4,i5,i6,i8)=ii
         else
            bbdatb(i1,i2,i3,i4,i5,i6,i8)=ii
         endif
      enddo
      close(10)
810   format(8(I2,1x),6(F7.3,1x))
77    continue
      if(na<1)then
         print*,"BBdat is not available, quit"
         goto 777
      endif
      templatelog=.false.
      tarest=.false.
!print*,"BBdat read finish",na,nb
      if(icontrol.ne."2".and.icontrol.ne."23")then  !!3 and 4, second template
         call getarg(4,f4)
         ir2=0
         open(1,file=trim(f4),err=333)   !!!only for test
  504 format(A100)
33       read(1,504,end=333)ss     !!F1 structure file
         if(ss(1:3).eq.'END'.or.ss(1:3).eq.'TER') goto 333
         bb=ss(17:17)
         if(ss(1:4).eq.'ATOM'.and.(bb.eq.' '.or.bb.eq.'A').and.ss(13:16).eq." CA ")then
            !          read(ss,101)du,atomty(in),duu,resd(in),duu,ress(in),duu,xx0(in),yy0(in),zz0(in)
            read(ss,101)du,abc,duu,abc,duu,ir2,duu,x,y,z
            if(ir2>0)then
               bbkxx(ir2)=x
               bbkyy(ir2)=y
               bbkzz(ir2)=z
            endif
         endif
         goto 33
333      continue 
         close(1)    
         templatelog=.true.
         if(ir2<1)then
            print*,"WARNING!!! No template used"
            templatelog=.false.
         endif
!!!!paramters for TASSER
         if(prtype.eq."hard")then
            cashortk=2.7          !for dist.dat
            calongk=0.5           !for distL.dat
            cacontactk=0.4        !for combCA.dat
            !       sdcontactk=0.4        !for comb.dat
            rcacut=0.1  !0.1 hard 0.2 medm 0.3 easy
         elseif(prtype.eq."medm")then
            cashortk=4.05          !for dist.dat
            calongk=1.0           !for distL.dat
            cacontactk=1.08 !1.08       !for combCA.dat
            !       sdcontactk=0.81       !for comb.dat
            rcacut=0.2  !0.1 hard 0.2 medm 0.3 easy
         else
            cashortk=3.6          !for dist.dat
            calongk=0.45          !for distL.dat
            cacontactk=2.7        !for combCA.dat
            !       sdcontactk=0.765 
            rcacut=0.3  !0.1 hard 0.2 medm 0.3 easy
         endif
!!read TASSER restraints *****************************
         open(81,file="dist.dat",status="old",err=81)  !short CA(>1 <=6) distance restarints,
         read(81,*)Ndist
         do i=1,Ndist
            read(81,*,err=81)rsd(i,1),rsd(i,2),j,drsd(i,1),drsd(i,2) !residue 1=>2,r 1=>2, diviation
         enddo
81       close(81)
         open(82,file="distL.dat",status="old",err=82)  !long CA(=10*n) distance restraints
         read(82,*)NdistL
         do i=1,NdistL
            read(82,*,err=82)rsdL(i,1),rsdL(i,2),drsdL(i) !residue 1=>2,r 1=>2
         enddo
82       close(82)
         open(84,file="combCA.dat",status="old",err=84) !CA contact (>=5 residue, <6A)
         read(84,*)NcombCA   !combCA.dat
         i=1
         j=0
         do while(i<=NcombCA)
            read(84,*,err=84)ia,ib,rk
            if(rk>rcacut)then
               j=j+1
               combsca(j,1)=ia
               combsca(j,2)=ib
               pcombca(j)=rk
            endif
            i=i+1
         enddo
84       close(84)
!!!!TASSER restraints finish *****************************
         NcombCA=j
         longca=int(Nres/10)+1
         Longcam=mod(Nres,10)
         tasserlog=.true. !.false.

         if(Ndist+NdistL+NcombCA>Nres)then
            tarest=.true.
         else
            print*,"warning!!!, no TASSER restraints used"
         endif
      endif  !!! ne 2 and 23
      Ncaclash=0
      Nbroken=0
      Nhbhit=0
      Nhbonds=0
      engk=sqrt(1.0/Nres)
      egapHB=0.05/engk
      egapHBm=0.01/engk
!print*,"build bb defualt"
     call BBADD(0)
     call SD_opt !!include heavy add
	506 format(A10,2f8.2,4I4,I6,2f12.2)
     if(icontrol.eq."23")then
        f2=trim(f1)//".h"
        open(10,file=f2)
        k=0
        do i=1,natom
           if(xx(i)>990.0.or.aty(i).eq."H")cycle
           k=k+1
           write(10,129),k,atomty(i),resd(i),ress(i),xx(i),yy(i),zz(i),ASSrd(i)
        enddo
        write(10,119)
        close(10)
        goto 777
     else
       call HADD
       call IMERGE(1)     
     endif  !!!23 is done here
     call Hbonds(2)
     call angle
     call ass2nd  !!assign 2nd structure

!         open(87,file="CONN",status="old",err=807)
!         do i=1,1076400
      !   do i=1,119600
       !    read(87,*),ix,iy,iz,i1,i2,(bbcontact(i,j),j=1,4)
!            read(87,*,err=807),(bbcontact(i,j),j=1,4)
!         enddo
!807      continue
!         close(87)
!         bbconlog=.true.
!         if(abs(bbcontact(1,1))+abs(bbcontact(1,2))+abs(bbcontact(1,3))+abs(bbcontact(1,4))<0.0001)then
!            print*,"warning, CONN not exist"
            bbconlog=.false.
!         endif
     PRHBlog=.false.
     call restr_stru !!output sspredict (concensus) and HB list
print*,"HBlist",Nreshb0,PRHBlog

     if(.not.PRHBlog)then  !!just maximize the current BBHB
        fhb=0
        hbvar=0
        print*,"Warning,no predicted Hbond list availuable",Nreshb0
        Nreshb0=0
     endif
     if(templatelog)then
        do i=1,Nres
           j=ca(i,1)
           rr00(1,i)=xx(j)
           rr00(2,i)=yy(j)
           rr00(3,i)=zz(j)
           rr11(1,i)=bbkxx(i)
           rr11(2,i)=bbkyy(i)
           rr11(3,i)=bbkzz(i)
        enddo
        w=1.0      
        call  u3b(w,rr11,rr00,Nres,1,rms,u,t,ier)  !!rotate a to b first to second
        do i=1,Nres
           bbkxx(i)=rr11(1,i)*u(1,1)+rr11(2,i)*u(1,2)+rr11(3,i)*u(1,3)+t(1)
           bbkyy(i)=rr11(1,i)*u(2,1)+rr11(2,i)*u(2,2)+rr11(3,i)*u(2,3)+t(2)
           bbkzz(i)=rr11(1,i)*u(3,1)+rr11(2,i)*u(3,2)+rr11(3,i)*u(3,3)+t(3)
        enddo    !!CA is enough
        RMSD0=1.1*sqrt(rms/Nres)
     else
        do i=1,Nres
           j=ca(i,1)
           bbkxx(i)=xx(j)
           bbkyy(i)=yy(j)
           bbkzz(i)=zz(j)
        enddo
     endif
     if(RMSD0<3.0)RMSD0=3.0
     RMSDcut=RMSD0-0.2 !!!!!

!!this is too sensitive compare with clash
     if(templatelog)then
        if(icontrol.eq."4")then
           fRMSD=200000.0
           ffrozen=1.0
        else
           fRMSD=70.0 !00.0
           ffrozen=0.1 !0.02 !1.0 
        endif
     else
        fRMSD=0
        ffrozen=0.02
     endif
     !!only need training if templatelog is true
     feg=1.0 !0.05 !1.0 !0.5  !!energy term in Hbond
     ftasser=100.0
     faredu=0.7 !0.71 !0.71
     ffrozen=1.0

     rclash=3.65 !3.75 !6+0.1*iicy/Mcy
     rbroken=4.05 !3.95 !4.1-0.2*iicy/Mcy
     
     NHB0=NHbonds
     do j=1,natom
        bkxx(j)=xx(j)
        bkyy(j)=yy(j)
        bkzz(j)=zz(j)
     enddo

     call HBrotate(2)
     
     call IMERGE(2)
     call Hbonds(2)

     if(NHB0>NHbonds)then
        do j=1,natom
           xx(j)=bkxx(j)
           yy(j)=bkyy(j)
           zz(j)=bkzz(j)
        enddo
        call IMERGE(2)
        call Hbonds(2)
     else
        NHB0=NHbonds
        do j=1,natom
           bkxx(j)=xx(j)
           bkyy(j)=yy(j)
           bkzz(j)=zz(j) 
        enddo
     endif
     mmhb0=Nhbhit   !!hit number

!!this part only need to consider Hbond
    call energy_CAL
    ee0=energy
    E0=ee0
    Ncaclash0=Ncaclash+2*Nbroken+Nres/2 
    eg0=e0
    do j=1,natom  !!used for final model, the increase in HB
       sxx(j)=xx(j)
       syy(j)=yy(j)
       szz(j)=zz(j)  
    enddo
    seng0=Nhbonds+Nhbhit-e0

    if(templatelog)then
       do i=1,Nres
          CArmsdd0(i)=CArmsdd(i)
          if(CArmsdd0(i)<0.01)then
             hbvar(i)=2
          else
             hbvar(i)=0
          endif
       enddo
    endif
505 format(A6,f8.4,6I4,2f12.2,A5)
    write(*,505),"initd",RMSD,labelcycle,Nreshb0,Nhbonds,Nhbhit,Ncaclash,Nbroken,e0,etasser,ssclass
!open(20,file="dynamics")
!write(20,*),"labelcycle Nhbonds Nhbhit Ncb ebbcon etasser RMSD e0"
!write(20,"I4,3I4,3f10.3,f12.2"),labelcycle,Nhbonds,Nhbhit,Ncaclash+Nbroken,ebbcon,etasser,RMSDcut,e0
!open(21,file="reprelax")
!write(21,*),Nres,e0,labelcycle,5000
!do i=1,Nres
!  j=ca(i,1)
!  write(21,"3f8.3"),xx(j),yy(j),zz(j)
!enddo

63  continue
     Nhbcy=max(5,labelcycle/10)
     Nhbcy=min(Nhbcy,20)
     if(icontrol.eq."2")Nhbcy=180
!!this part optimize Hbond *****************************************************************
!print*,"HB rest",Nreshb0,labelcycle
    if(Nreshb0>0)then
       if(labelcycle>0)then
        call HBrotate(2)
        call IMERGE(2)
        call Hbonds(2)
        if(Nhbonds+fhb*Nhbhit-feg*e0>NHB0+fhb*mmhb0-feg*eg0)THEN !.and.Ncaclash<=Ncaclash0)then
           mmhb0=Nhbhit
           NHB0=NHbonds
           eg0=e0
           do j=1,natom
              bkxx(j)=xx(j)
              bkyy(j)=yy(j)
              bkzz(j)=zz(j)  
           enddo
           if(Nhbonds+Nhbhit-e0>seng0)then
              seng0=Nhbonds+Nhbhit-e0
              do j=1,natom
                 sxx(j)=xx(j)
                 syy(j)=yy(j)
                 szz(j)=zz(j)  
              enddo
           endif
        else
           do j=1,natom
              xx(j)=bkxx(j)
              yy(j)=bkyy(j)
              zz(j)=bkzz(j)  
           enddo
        endif      
     endif

      do icy=1,Nhbcy        
        call BBADD(2)
        call IMERGE(2)
        call Hbonds(2)
        Nhbhit=0
        if(icontrol.ne."4")then
           do i=1,Nreshb0
              i1=reshbn(i,1)
              i2=reshbn(i,2)
              do j=1,Nhbonds        
                 if(hbn(j,1).eq.i1.and.hbn(j,2).eq.i2)then
                    Nhbhit=Nhbhit+1
                    if(ir1>1)hbvar(ir1-1)=2
                    hbvar(ir2)=2
                    goto 93
                 endif
              enddo
              if(ir1>1)hbvar(ir1-1)=0
              hbvar(ir2)=0
93            continue
           enddo
        endif
        if(Nhbonds>=NHB0)then
!write(*,"A8,5I4,2f12.3"),"betterHB",NHB0,mmhb0,Nhbonds,Nhbhit,labelcycle,e0,eg0
           mmhb0=Nhbhit
           NHB0=NHbonds
           eg0=e0
           do j=1,natom
              bkxx(j)=xx(j)
              bkyy(j)=yy(j)
              bkzz(j)=zz(j)  
           enddo
           if(Nhbonds+Nhbhit-e0>seng0)then
              write(*,506),"betterHB",RMSD,RMSDcut,NHB0,mmhb0,Nhbonds,Nhbhit,labelcycle,e0,eg0
              seng0=Nhbonds+Nhbhit-e0
              do j=1,natom
                 sxx(j)=xx(j)
                 syy(j)=yy(j)
                 szz(j)=zz(j)  
              enddo
           endif
        else
           do j=1,natom
              xx(j)=bkxx(j)
              yy(j)=bkyy(j)
              zz(j)=bkzz(j)  
           enddo
        endif      
     enddo
  else  !!!no HB restraints
     do icy=1,Nhbcy         
        call BBADD(2)
        call IMERGE(2)
        call Hbonds(2)
        if(Nhbonds>=NHB0)then
           NHB0=NHbonds
           eg0=e0
           do j=1,natom
              bkxx(j)=xx(j)
              bkyy(j)=yy(j)
              bkzz(j)=zz(j)  
           enddo
           do j=1,Nhbonds        
              hbvar(ress(hbn(j,1)))=2
              hbvar(ress(hbn(j,2)))=2
           enddo
           if(Nhbonds+Nhbhit-e0>seng0)then
              write(*,506),"betterHB0",RMSD,RMSDcut,NHB0,mmhb0,Nhbonds,Nhbhit,labelcycle,e0,eg0
              seng0=Nhbonds+Nhbhit-e0
              do j=1,natom
                 sxx(j)=xx(j)
                 syy(j)=yy(j)
                 szz(j)=zz(j)  
              enddo
           endif
        else
           do j=1,natom
              xx(j)=bkxx(j)
              yy(j)=bkyy(j)
              zz(j)=bkzz(j)  
           enddo
        endif
     enddo
  endif
  e0=eg0
     if(labelcycle<1)then
        call SD_opt !!include heavy add
         if(icontrol.eq."2")then
           f2=trim(f1)//".h"
        else
           f2=trim(f1)//".fix"
        endif
        open(10,file=f2)
        k=0
        do i=1,natom
           if(xx(i)>990.0.or.aty(i).eq."H")cycle
           k=k+1
           write(10,129),k,atomty(i),resd(i),ress(i),xx(i),yy(i),zz(i),ASSrd(i)
           !           write(10,129),i,atomty(i),resd(i),ress(i),bkxx(i),bkyy(i),bkzz(i),ASSrd(i)
        enddo
        close(10)
        if(icontrol.eq."2")goto 777 !!to here, 2 done, only left 3 and 4
     endif

!!this part move CA *************************************************
!      if(labelcycle<Ncytotal)then
!   write(20,"I4,3I4,3f10.3,f12.2"),labelcycle,Nhbonds,Nhbhit,Ncaclash+Nbroken,ebbcon,etasser,RMSDcut,e0
!   if(labelcycle<200.or.(mod(labelcycle,20).eq.0.and.labelcycle>=50))then
!      write(21,*),Nres,e0,labelcycle,5000
!      do i=1,Nres
!         j=ca(i,1)
!         write(21,"3f8.3"),xx(j),yy(j),zz(j)
!      enddo
!   endif
!endif
64       continue
         labelcycle=labelcycle+1
         if(labelcycle>100.and.RMSDcut<0.01)goto 65
         faredu=max(0.5,faredu-0.002) !7
         rclash=min(3.7,rclash+0.002) !max(3.61,rclash-0.01)
         rbroken=max(3.9,rbroken-0.002) !min(4.1,rbroken+0.01)
         call HBrotate(1)
 !        write(*,"A6,3f8.4,5I4,2f12.2"),"cyclet",RMSD,RMSDcut,RMSD0,labelcycle,Nhbonds,Nreshb0,Ncaclash,Nbroken,ee,etasser
         if(labelcycle<Ncytotal)goto 63
      close(20)
      close(21)
65    continue
      do j=1,natom
         xx(j)=sxx(j)
         yy(j)=syy(j)
         zz(j)=szz(j)
      enddo
      call SD_opt  !!optimize side chain
      f2=trim(f1)//".h"
      open(10,file=f2)
      k=0
      do i=1,natom
         if(xx(i)>990.0.or.aty(i).eq."H")cycle
         k=k+1
!         write(10,129),i,atomty(i),resd(i),ress(i),bkxx(i),bkyy(i),bkzz(i),ASSrd(i)
         write(10,129),k,atomty(i),resd(i),ress(i),xx(i),yy(i),zz(i),ASSrd(i)
      enddo
      write(10,119)
      close(10)
507 format(3(A5,3I8))
write(*,507),"Lmpr",relax(1,1),relax(1,2),relax(1,3),"CaCa",relax(2,1),relax(2,2),relax(2,3),"Glob",relax(3,1),relax(3,2),relax(3,3)
      goto 777 
   endif  !!build fulll atomic model  !!2, 22, 3 and 4 done

119 format("TER")
129 format("ATOM",I7,1X,A4,1X,A3,2X,I4,4X,3F8.3,I3)   !pdb
!   call SD_opt
   call HEAVY_ADD
   call HADD
   call IMERGE(1)
   call Hbonds(1)

if(icontrol.eq."5")then
!   F1=trim(F1)//'.hbnd'
write(*,*)"#No.      donor     acceptor  || Ca-CAdis  R_HA   <DHA   <HAAb  Ehbnd"
do i=1,Nhbonds
   ir1=ress(hbn(i,1))
   ir2=ress(hbn(i,2))
   ia=ca(ir1,1)
   ib=ca(ir2,1)
   draa=sqrt((xx(ia)-xx(ib))**2+(yy(ia)-yy(ib))**2+(zz(ia)-zz(ib))**2)
   if(atomty(hbn(i,1)).eq." N  ")then
      tp1="B"
   else
      tp1="S"
   endif
   if(atomty(hbn(i,2)).eq." O  ")then
      tp2="B"
   else
      tp2="S"
   endif
 write(*,117),i,ir1,resd(ia),atomty(hbn(i,1)),ir2,resd(ib),atomty(hbn(i,2)),tp1,tp2,draa,htbon(i,1),htbon(i,2),htbon(i,3),htbon(i,4)
enddo
508 format(A22,I6,f11.4)
write(*,508),"number of Hbonds(EHB)",Nhbonds,EHbond 
goto 777
endif  !!5 done here

if(icontrol.eq."6".or.icontrol.eq."66")then
   open(58,file="seq.dat",status="old",err=58)    !2nd structure identification
   do i=1,Nres   !seq.dat  !!1 coil, 2 helix, 4 sheet
      read(58,*,err=58)k,abc,seq(i,1),seq(i,2) 
   enddo
58 close(58)

   call angle
   call ass2nd  !!assign 2nd structure
   call restr_stru !!output sspredict (concensus) and HB list
   goto 777
endif

117 format(I4,I4,A4,A5,I4,A4,A5,1x,A1,A1,1x,F6.3,F7.3,2F8.3,F10.3)
777 continue
contains   !!6 and 66 done

!!this subroutine to statistics the atomic contact in 3.0A
subroutine EVcon
  implicit none
  integer,dimension(nmax,0:15)::bnd4
  integer lintca,npc1,npc2,ntp1,ntp2,np1,np1min,np1max,np2min,np2max
  integer itm,ip0,irs,irs2,i0,ijk,ikk,ikk4,icn,icn2,icn3
  REAL r2,arvdw,rt,x0,y0,z0,EV,fb,rk,ea,ach

  npc1=0
  npc2=0
  np1=0  !count on each atom
  ntp1=0  !non polar
  ntp2=0  !polar
  np1min=20
  np2min=20
  np1max=0
  np2max=0
  Lintca=3  !!4A
  call IMERGE(1)

!!!this part to generate the 1-2, 1-3 excluded table and 1-4 partially interact table for each atom
bnd4=0
do ijk=1,natom
    ikk=1
    ikk4=1
      do icn=1,bonda(ijk,5)  !!this to excluded neighbour bonded atoms, = has been substrated
         ip1=bonda(ijk,icn)   !excluded the bonded atoms
         if(ip1<1.or.ip1>natom.or.ip1==ijk)cycle
         iw=0
         do i=1,ikk
            if(bnd4(ijk,i)==ip1)then
               iw=1
               goto 401
            endif
         enddo
401      continue
         if(iw==0)then
            bnd4(ijk,ikk)=ip1
            ikk=ikk+1
         endif       
         do icn2=1,bonda(ip1,5)              !exclude 2-bonded atoms
            ip2=bonda(ip1,icn2)
            if(ip2<1.or.ip2>natom.or.ip2==ijk)cycle
            iw=0
            do i=1,ikk
               if(bnd4(ijk,i)==ip2)then
                  iw=1
                  goto 402
               endif
            enddo
402         continue
            if(iw==0)then
               bnd4(ijk,ikk)=ip2
               ikk=ikk+1
            endif         
            do icn3=1,bonda(ip2,5)           !excluded the next 3-bonded atoms, 1-4
               ip3=bonda(ip2,icn3)
               if(ip3<1.or.ip3>natom.or.ip3==ijk)cycle
               iw=0
                do i=1,ikk
                  if(bnd4(ijk,i)==ip3)then
                     iw=1
                     goto 404
                  endif
               enddo
404            continue
            enddo
         enddo
      enddo 
      bnd4(ijk,0)=ikk-1
   enddo


  do itm=1,Natom
     if(aty(itm).eq."H")cycle
     np1=0
	 irs=ress(itm)
     ach=abs(atmpar(itm,1)) !!partial charge
       x0=xx(itm)
       y0=yy(itm)
       z0=zz(itm)
       ix0=Nint(xx(itm)*rmesh)   
       iy0=Nint(yy(itm)*rmesh)   
       iz0=Nint(zz(itm)*rmesh)   
       Lxmin=max(1,ix0-Lintca)  !!if it's 4, no atom clash yet
       Lymin=max(1,iy0-Lintca)
       Lzmin=max(1,iz0-Lintca)
       Lxmax=min(La,ix0+Lintca)
       Lymax=min(La,iy0+Lintca)
       Lzmax=min(La,iz0+Lintca)
!!for the clash, only j>i (in natom) case are considered, the only the large side has been labeled
       do ix=Lxmin,Lxmax
          do iy=Lymin,Lymax
             do iz=Lzmin,Lzmax    
              !  if(ix==ix0.and.iy==iy0.and.iz==iz0)goto 123 
                ip=SITE(ix,iy,iz)
                if(ip<1)goto 123
                ip0=ip
                irs2=ress(ip)
                if(irs2.eq.irs)goto 123  !!not consider in the same residue
                do j=0,NSITT
                   if(j>0)then
                      if(SITT(ip0,j)>=1)then
                         ip=SITT(ip0,j)
                      else
                         goto 123  !!this is no clash
                      endif
                   endif
                   if(aty(ip).eq.'H'.or.ip.eq.itm)goto 124
                   do k=1,bnd4(itm,0)
                      if(bnd4(itm,k)==ip)goto 124
                   enddo
                   r2=(xx(itm)-xx(ip))**2+(yy(itm)-yy(ip))**2+(zz(itm)-zz(ip))**2
                   if(r2<=9.0)then
                      np1=np1+1
                   endif
124                continue  !exclude of bonded atoms
                enddo   !!NSITT
123             continue    
             enddo
          enddo
       enddo
       if(ach<0.3)then
          if(np1<np1min)np1min=np1  !!count for the solvation
          if(np1>np1max)np1max=np1
          npc1=npc1+np1
          ntp1=ntp1+1
       else
          if(np1<np2min)np2min=np1 !!count for the solvation
          if(np1>np2max)np2max=np1
          npc2=npc2+np1
          ntp2=ntp2+1
       endif
    enddo  !!all heavy atoms in this residue
509 format(A6,2f8.3,6I5)
    write(*,509),"avg3pn",1.0*npc1/ntp1,1.0*npc2/ntp2,ntp1,ntp2,np1min,np1max,np2min,np2max

  end subroutine EVCON
!!!function to calculate the energy
subroutine energy_CAL
implicit none
real eLca,ehb
   call IMERGE(2)
   call Hbonds(2)  !!count the BB Hbonds
!print*,"Hbonds",Nhbonds,Nhbhit,Nreshb0
   call calclash
   ebbcon=0.0
   if(bbconlog)call bbcont
   if(templatelog)then
      call CARMSD
   else
      RMSD=0.0
   endif
   if(tarest)then
      call rest_tasser
      etasser=ftasser*(cacontactk*CAcontact+cashortk*CAdist+calongk*CAdistL)
      eLca=engk*eclash !(10*Ncaclash+20.0*Nbroken)
      ehb=Nhbonds+fhb*Nhbhit !NHB0+fhb*mmhb0
      energy=engk*etasser+eLca+fRMSD*RMSD-ehb+1.5*ebbcon
 !     energy=engk*(cacontactk*CAcontact+cashortk*CAdist+calongk*CAdistL+Ncaclash+Nbroken+500.0*RMSD-5*(Nhbonds+fhb*Nhbhit))
   else
      eLca=engk*eclash !(10*Ncaclash+20.0*Nbroken)
      ehb=Nhbonds+fhb*Nhbhit !NHB0+fhb*mmhb0
      energy=eLca+fRMSD*RMSD-ehb+1.5*ebbcon
   endif
!write(*,"A4,7I4,8f10.2"),"ENG",labelcycle,Ncaclash,Nbroken,NHB0,NHbonds,Nhbhit,mmhb0,etasser,eLca,ehb,ebbcon,fRMSD*RMSD,fRMSD*RMSDcut,energy,e0
 !  call energy_CAL
 !  energy=energy+engk*EHbond
!energy=10.0*Ncaclash+20.0*Nbroken+5.0*RMSD-5*Nhbonds-20*Nhbhit
return
end subroutine energy_CAL
!!this subroutine to calculate the BBcontact energy
subroutine bbcont
implicit none
integer i,j,ir1,ir2,is1,is2,ipca1,ipca2

ebbcon=0
do i=1,Nres-2
   ir1=ca(i,2)
   if(seq(i,1).eq.4)then !!strand
      is1=3
   elseif(seq(i,1).eq.2)then !!helix
      is1=2
   else
      is1=1
   endif
   ipca1=ca(i,1)
   do j=i+2,Nres
      ir2=ca(j,2)
      if(seq(j,1).eq.4)then !!strand
         is2=3
      elseif(seq(i,1).eq.2)then !!helix
         is2=2
      else
         is2=1
      endif
      k=j-i-2
      if(k>298)k=298
      ip=k*3600+(ir1-1)*180+(ir2-1)*9+(is1-1)*3+is2
 !     ip=k*400+((ir1-1)*20)+ir2
      if(ip>1076400)then
    !  if(ip>119600)then
         print*,"warning ID for backbone contact err",ip,k,ir1,ir2,is1,is2
         cycle
      endif
      ipca2=ca(j,1)
      r1=sqrt((xx(ipca1)-xx(ipca2))**2+(yy(ipca1)-yy(ipca2))**2+(zz(ipca1)-zz(ipca2))**2)
      r2=sqrt((xx(ipca1-1)-xx(ipca2+1))**2+(yy(ipca1-1)-yy(ipca2+1))**2+(zz(ipca1-1)-zz(ipca2+1))**2)
      r3=sqrt((xx(ipca1+1)-xx(ipca2-1))**2+(yy(ipca1+1)-yy(ipca2-1))**2+(zz(ipca1+1)-zz(ipca2-1))**2)
      if(r1<6.8)then
         if(r1<5.8)then
            ebbcon=ebbcon+bbcontact(ip,2)
         else
            ebbcon=ebbcon+bbcontact(ip,1)
         endif
      endif
      if(r2<3.9)ebbcon=ebbcon+bbcontact(ip,3)
      if(r3<3.9)ebbcon=ebbcon+bbcontact(ip,4)
   end do
enddo
!print*,"bbcontact",ebbcon
end subroutine bbcont
!!this subroutine to build up CA is missing
!!idea mkdir i->k priciple, random in this main direction
subroutine CA_build
implicit none
integer miss(1000,2)
real r,r1,fb,xi,yi,zi,ax0,ay0,az0
real diss,adss,dss0,ads0
integer Lpcut,Lcon,nmiss,ip0,ip1,i1,i2,kg,n_try
integer nk,ni
integer i,j,jj,ii1

CAbdlog=.false.
!count the break regions and remove isolated short regions (less than three residues)
Lpcut=2  !!the segment island to be cut

!!scan the structure, if the CA distance os too large, rebuild this part
do i=1,Nres
   ip0=ca(i,1)
   if(xx(ip0)<999.0)then
     if(i>1.and.i<Nres)then
        if(xx(ca(i-1,1))<999.0)then
           r=sqrt((xx(ip0)-xx(ca(i-1,1)))**2+(yy(ip0)-yy(ca(i-1,1)))**2+(zz(ip0)-zz(ca(i-1,1)))**2)
            if(r>4.2)then
              xx(ca(i-1,1))=999.0
              xx(ip0)=999.0
           endif
        endif
        if(xx(ca(i+1,1))<999.0)then
           r=sqrt((xx(ip0)-xx(ca(i+1,1)))**2+(yy(ip0)-yy(ca(i+1,1)))**2+(zz(ip0)-zz(ca(i+1,1)))**2)
           if(r>4.2)then
              xx(ip0)=999.0
              xx(ca(i+1,1))=999.0
           endif
        endif
     elseif(i.eq.1)then
        if(xx(ca(2,1))<999.0)then
           r1=sqrt((xx(ip0)-xx(ca(2,1)))**2+(yy(ip0)-yy(ca(2,1)))**2+(zz(ip0)-zz(ca(2,1)))**2)
           if(r1>4.2)then
              xx(ip0)=999.0
           endif  
        endif
     elseif(i.eq.Nres)then
        if(xx(ca(Nres-1,1))<999.0)then
           r1=sqrt((xx(ip0)-xx(ca(Nres-1,1)))**2+(yy(ip0)-yy(ca(Nres-1,1)))**2+(zz(ip0)-zz(ca(Nres-1,1)))**2)
           if(r1>4.2)then
              xx(ip0)=999.0
           endif  
        endif
     endif
  endif
enddo

!!scan and assign rebuild regions
nmiss=0  !!pitches need to rebuild
do i=1,Nres
   if(xx(ca(i,1))>990.0)then
      nmiss=nmiss+1
      miss(nmiss,1)=i  !!initial
      k=i+1
      do while(k<Nres)
         if(xx(ca(k,1))>990.0)then
            k=k+1
         else
            goto 340
         endif
      enddo
340   continue
      miss(nmiss,2)=k-1  !!end residue
   endif
enddo
!!remove small align segment

i=1
do while(i<nmiss)
   if(miss(i+1,1)-miss(i,2)<=Lpcut)then
      miss(i,2)=miss(i+1,2)
      do j=i+1,nmiss-1
         miss(j,1)=miss(j+1,1)
         miss(j,2)=miss(j+1,2)
      enddo
      nmiss=nmiss-1
   else
      i=i+1
   endif
enddo
!!!check whether the loop region is rebuildable
!print*,"reconstructed pitches:",nmiss
do i=1,nmiss
!   print*,"pitches>>",i,miss(i,1),miss(i,2)
   if(miss(i,1)>1.and.miss(i,2)<Nres)then
      ip0=ca(miss(i,1)-1,1)
      ip1=ca(miss(i,2)+1,1)
      if(xx(ip1)<990.0.and.xx(ip0)>990.0)then
         print*,"boundary err",i,miss(i,1),miss(i,2)
         goto 240
      endif
      r=sqrt((xx(ip1)-xx(ip0))**2+(yy(ip1)-yy(ip0))**2+(zz(ip1)-zz(ip0))**2)
      nk=(r-3.6*(miss(i,2)-miss(i,1)+2))/3.6  !!the number of residues need shift
      if(nk<=0)goto 240  !!no need for adjustment
      if(nk==1)then
         if(miss(i,1)>ni.and.seq(miss(i,1),1)<2)then
            miss(i,1)=miss(i,1)-1
         elseif(miss(i,2)<Nres)then
            miss(i,2)=miss(i,2)+1
         endif
      else
         ni=nk/2
         if(miss(i,1)>ni.and.miss(i,2)<=Nres-ni)then
            miss(i,1)=miss(i,1)-ni
            miss(i,2)=miss(i,2)+ni
         elseif(miss(i,1)>ni)then
            miss(i,1)=1
         elseif(miss(i,2)>Nres-ni)then
            miss(i,2)=Nres
         endif
      endif
   endif
240  continue
enddo
!!!the boundary settle down

do i=1,nmiss
   i1=miss(i,1)
   i2=miss(i,2)
!print*,"adding",i,i1,i2
   n_try=0
   if(i1.eq.1)then  !!add to N-term
      caxx(i2+1)=xx(ca(i2+1,1))
      cayy(i2+1)=yy(ca(i2+1,1))
      cazz(i2+1)=zz(ca(i2+1,1))
341   continue
      n_try=n_try+1
      if(n_try>10000)then
         CAbdlog=.true.
         print*,"not build region",i,i1,i2
         stop "failed build region EXIST!!!!"
      endif
      do j=i2,1,-1
         kg=0
361      continue
         xi=2.0*rand(iseed)-1.0
         yi=2.0*rand(iseed)-1.0 !*yp
         zi=2.0*rand(iseed)-1.0 !*zp
         r=sqrt(xi*xi+yi*yi+zi*zi)
         if(r>0.001)then
            fb=3.8/r         
         else
            goto 361
         endif
         caxx(j)=caxx(j+1)+xi*fb
         cayy(j)=cayy(j+1)+yi*fb
         cazz(j)=cazz(j+1)+zi*fb
         kg=kg+1
         if(kg>1000)then
            goto 341
         endif
         if(j>=i2-1)then
            if(j+2<=Nres.and.xx(ca(j+2,1))<999.0)then
               ip0=ca(j+2,1)
               r=sqrt((xx(ip0)-caxx(j))**2+(yy(ip0)-cayy(j))**2+(zz(ip0)-cazz(j))**2)
               if(r<4.2.or.r>7.8)goto 361
            endif
         else
            r=sqrt((caxx(j+2)-caxx(j))**2+(cayy(j+2)-cayy(j))**2+(cazz(j+2)-cazz(j))**2)
            if(r<4.2.or.r>7.8)goto 361
         endif
!write(*,"A8,3I5,6f8.3"),"kg,j Nterm",n_try,j,kg,caxx(j),cayy(j),cazz(j),caxx(j+1),cayy(j+1),cazz(j+1)
        do jj=i2,j+2,-1
            r=(caxx(jj)-caxx(j))**2+(cayy(jj)-cayy(j))**2+(cazz(j)-cazz(j))**2
            if(r<12.96)then  !!12.96
               goto 361
            endif
         enddo
         do jj=i2+1,Nres  !!avoid heavy clash
            ip0=ca(jj,1)
            if(xx(ip0)<990.0.and.abs(jj-j)>1)then
               r=(xx(ip0)-caxx(j))**2+(yy(ip0)-cayy(j))**2+(zz(ip0)-cazz(j))**2
               if(r<12.96)then  !!12.96
                  goto 361
               endif
            endif
         enddo
      end do
   elseif(i2.eq.Nres)then
      caxx(i1-1)=xx(ca(i1-1,1))
      cayy(i1-1)=yy(ca(i1-1,1))
      cazz(i1-1)=zz(ca(i1-1,1))
342   continue
      n_try=n_try+1
      if(n_try>10000)then
         CAbdlog=.true.
         print*,"not build region",i,i1,i2
         stop "failed build region EXIST2!!!!"
      endif
      do j=i1,Nres
         kg=0
362      continue
         xi=2.0*rand(iseed)-1.0
         yi=2.0*rand(iseed)-1.0 !*yp
         zi=2.0*rand(iseed)-1.0 !*zp
         r=sqrt(xi*xi+yi*yi+zi*zi)
         if(r>0.001)then
            fb=3.8/r         
         else
            goto 362
         endif
         caxx(j)=caxx(j-1)+xi*fb
         cayy(j)=cayy(j-1)+yi*fb
         cazz(j)=cazz(j-1)+zi*fb
         kg=kg+1
         if(kg>1000)then
            goto 342
         endif
        if(j<=i1+1)then
            if(j-2>=1.and.xx(ca(j-2,1))<999.0)then
               ip0=ca(j-2,1)
               r=sqrt((xx(ip0)-caxx(j))**2+(yy(ip0)-cayy(j))**2+(zz(ip0)-cazz(j))**2)
               if(r<4.2.or.r>7.8)goto 362
            endif
         else
            r=sqrt((caxx(j-2)-caxx(j))**2+(cayy(j-2)-cayy(j))**2+(cazz(j-2)-cazz(j))**2)
            if(r<4.2.or.r>7.8)goto 362
         endif
         do jj=i1,j-2
            r=(caxx(jj)-caxx(j))**2+(cayy(jj)-cayy(j))**2+(cazz(j)-cazz(j))**2
            if(r<12.96)then  !!12.96
               goto 362
            endif
         enddo
         do jj=1,i1-1  !!avoid heavy clash
            ip0=ca(jj,1)
            if(xx(ip0)<990.0.and.abs(jj-j)>1)then
               r=(xx(ip0)-caxx(j))**2+(yy(ip0)-cayy(j))**2+(zz(ip0)-cazz(j))**2
               if(r<12.96)then  !!12.96
                  goto 362
               endif
            endif
         enddo
      end do
   else   !!!add the middle terms i1->i2
      dss0=3.5
      caxx(i1-1)=xx(ca(i1-1,1))
      cayy(i1-1)=yy(ca(i1-1,1))
      cazz(i1-1)=zz(ca(i1-1,1))
      ax0=xx(ca(i2+1,1))
      ay0=yy(ca(i2+1,1))
      az0=zz(ca(i2+1,1)) 
      diss=sqrt((ax0-caxx(i1-1))**2+(ay0-cayy(i1-1))**2+(az0-cazz(i1-1))**2)
      adss=diss/(i2-i1+2)
!write(*,"A8,2I4,6f8.3,2f8.3"),"diss>>",i1-1,i2+1,ax0,ay0,az0,caxx(i1-1),cayy(i1-1),cazz(i1-1),diss,adss
!!!large seperation, linearly put the coordinates
      n_try=0
      ii1=i1
      ads0=0.4/(i2-i1+1)
!print*,"initial",i,adss,dss0
243   continue
      if(adss>dss0)then
         xi=(ax0-caxx(ii1-1))/(i2-ii1+2)
         yi=(ay0-cayy(ii1-1))/(i2-ii1+2)
         zi=(az0-cazz(ii1-1))/(i2-ii1+2)
         do j=ii1,i2
            caxx(j)=caxx(j-1)+xi
            cayy(j)=cayy(j-1)+yi
            cazz(j)=cazz(j-1)+zi
         enddo
         goto 370 !!!end of this pitch
      endif
!!random walk
343   continue
      n_try=n_try+1
      if(n_try>10000)then
         CAbdlog=.true.
         print*,"bad build region",i,i1,i2,adss,dss0
         xi=(ax0-caxx(ii1-1))/(i2-ii1+2)
         yi=(ay0-cayy(ii1-1))/(i2-ii1+2)
         zi=(az0-cazz(ii1-1))/(i2-ii1+2)
         do j=ii1,i2
            caxx(j)=caxx(j-1)+xi
            cayy(j)=cayy(j-1)+yi
            cazz(j)=cazz(j-1)+zi
         enddo
         goto 370 !!!end of this pitch
!         stop "failed build region EXIST3!!!"
      endif
      do j=i1,i2
         kg=0
363      continue
         xi=2.0*rand(iseed)-1.0
         yi=2.0*rand(iseed)-1.0 !*yp
         zi=2.0*rand(iseed)-1.0 !*zp
         r=sqrt(xi*xi+yi*yi+zi*zi)         
         if(r>0.001)then
            fb=3.8/r         
         else
            goto 363
         endif
         caxx(j)=caxx(j-1)+xi*fb
         cayy(j)=cayy(j-1)+yi*fb
         cazz(j)=cazz(j-1)+zi*fb      
         kg=kg+1
         if(kg>1000)then
            goto 343
         endif
        if(j<=i1+1)then
            if(j-2>=1.and.xx(ca(j-2,1))<999.0)then
               ip0=ca(j-2,1)
               r=sqrt((xx(ip0)-caxx(j))**2+(yy(ip0)-cayy(j))**2+(zz(ip0)-cazz(j))**2)
               if(r<4.2.or.r>7.8)goto 363
            endif
         else
            r=sqrt((caxx(j-2)-caxx(j))**2+(cayy(j-2)-cayy(j))**2+(cazz(j-2)-cazz(j))**2)
            if(r<4.2.or.r>7.8)goto 363
         endif
!!!CONNECTIONG
         r=sqrt((ax0-caxx(j))**2+(ay0-cayy(j))**2+(az0-cazz(j))**2)         
         adss=r/(i2-j+1)
!print*,n_try,kg,r,adss,j
         if(adss>3.67+ads0*(j-i1))goto 363 !!.or.adss<3.4)goto 363       
         do jj=i1,j-2
            r=(caxx(jj)-caxx(j))**2+(cayy(jj)-cayy(j))**2+(cazz(j)-cazz(j))**2
            if(r<12.96)then  !!12.96
               goto 363
            endif
         enddo
         do jj=1,Nres  !!avoid heavy clash
            ip0=ca(jj,1)
            if(xx(ip0)<999.0.and.abs(jj-j)>1)then
               r=(xx(ip0)-caxx(j))**2+(yy(ip0)-cayy(j))**2+(zz(ip0)-cazz(j))**2
               if(r<12.96)then  !!12.96
                  goto 363
               endif
            endif
         enddo
         if(adss>dss0.and.kg>990.and.n_try>9000)then  !!need extent for left residues
           ii1=j
           print*,"inter",n_try,i,ii1,adss,dss0
           goto 243
        endif
      enddo
   endif  !! all three cases done
370 continue
   do j=i1,i2
      xx(ca(j,1))=caxx(j)
      yy(ca(j,1))=cayy(j)
      zz(ca(j,1))=cazz(j)
   enddo
print*,"CA build from",i1," to ",i2,i
enddo !!!nmiss pitches
end subroutine CA_build
!!this subroutine to scan the atom clash in sidechain, and look whether CG-C and CG-N distance is in range
!!need imerge
subroutine SDclashscan
implicit none
real ep
integer i,ipca,ipc,ipcg,ip0,ix,iy,iz,kca,cgx,cgy,cgz
  nsdclash=0
  call IMERGE(2)
  sdclash=0
  sdgg=0
  do i=1,Nres
     if(ca(i,2)<2)cycle !!no side chain
     kca=3
     ipca=ca(i,1)
     ipc=ipca+hra(ca(i,2))-2  !!all the side chain heavy atoms
     cgx=0
     cgy=0
     cgz=0
!print*,"scan",i,ipca,ipc,resd(ipca)
     do j=ipca+kca,ipc
        ix=Nint(xx(j)*rmesh)
        iy=Nint(yy(j)*rmesh)
        iz=Nint(zz(j)*rmesh)
        do k=1,6
           select case(k)
           case(1)
              ia=ix-1
              ib=iy
              ic=iz
           case(2)
              ia=ix
              ib=iy-1
              ic=iz
           case(3)
              ia=ix
              ib=iy
              ic=iz-1
           case(4)
              ia=ix+1
              ib=iy
              ic=iz
           case(5)
              ia=ix
              ib=iy+1
              ic=iz
           case(6)
              ia=ix
              ib=iy
              ic=iz+1
           case default
           end select
           if(SITE(ia,ib,ic)>0.and.SITE(ia,ib,ic)<=natom)then
              ip0=site(ia,ib,ic)
              if(aty(ip0).ne."H".and.ress(ip0).ne.i)then
                 r=sqrt((xx(ip0)-xx(j))**2+(yy(ip0)-yy(j))**2+(zz(ip0)-zz(j))**2)
                 if(r<2.2)then
                    ep=sdhro(ca(ress(ip0),2))*sdhro(ca(i,2))/(rsdr(ca(i,2))+rsdr(ca(ress(ip0),2)))
                    sdclash(i)=sdclash(i)+1-ep
                    nsdclash=nsdclash+1-ep
                 endif
              endif
           endif
        enddo !!k=1,6
        cgx=cgx+xx(j)
        cgy=cgy+yy(j)
        cgz=cgz+zz(j)          
     enddo  !!each side chain
!!calculate the SG distance diviation
  !   if(i>1.and.i<Nres)then
  !      cgx=cgx/(hra(ca(i,2))-4)
   !     cgy=cgy/(hra(ca(i,2))-4)
   !     cgz=cgz/(hra(ca(i,2))-4)
   !     r=sqrt((cgx-sgx(i))**2+(cgy-sgy(i))**2+(cgz-sgz(i))**2)
   !     sgd(i)=r
   !     sdgg=sdgg+r
   !  endif

   !  cycle
!!calculate CG-C and CG-N distance
     if(ca(i,2)<3)cycle !!no side chain
     ipcg=ipca+4
     r=sqrt((xx(ipcg)-xx(ipca+1))**2+(yy(ipcg)-yy(ipca+1))**2+(zz(ipcg)-zz(ipca+1))**2)
     if(r<2.76.or.(r>3.28.and.r<3.58).or.r>3.97)then
        sdclash(i)=sdclash(i)+1
        nsdclash=nsdclash+1
     endif
     if(ca(i,2).eq.8)cycle
     r=sqrt((xx(ipcg)-xx(ipca-1))**2+(yy(ipcg)-yy(ipca-1))**2+(zz(ipcg)-zz(ipca-1))**2)
     if(r<2.78.or.(r>3.28.and.r<3.62).or.r>3.9)then
        sdclash(i)=sdclash(i)+1
        nsdclash=nsdclash+1
     endif
!write(*,"I4,A5,I7,2f9.2"),i,resd(ca(i,1)),sdclash(i),sgd(i),ep
  enddo
!pause
!  do i=1,Nres
!     sdclash(i)=0
!  enddo
!  nsdclash=0

end subroutine SDclashscan
!!this subroutine to calculate the distance CG-C and CG-N and as a energy term 

!!this subroutine to optimization sidechain, for final model build
!!select new side chain, use side chain atom clash as energy function for judge
subroutine SD_opt
implicit none
real rdangg,angg,phi,psi,chi1,ta0,ta,tb,tc !!real angle change and range
integer i,j,ii,nhra,ires,ipca,NSCT,iresin,kbg,ipcg,nsd


do i=1,natom
   if(atomty(i).ne." C  ".and.atomty(i).ne." N  ".and.atomty(i).ne." CA ".and.atomty(i).ne." O  ")then
      xx(i)=999.0
!print*,"readd",i,atomty(i),ress(i),resd(i)
   endif
enddo
call CB_ADD
!print*,"CBADD finish"
!call SG_ADD
call HEAVY_ADD
call SD_opt1

goto 277
if(labelcycle>1)then
   call IMERGE(2)  !!???do simulation not call this part
else
   call IMERGE(1)
endif

   do i=1,natom
     xx0(i)=xx(i)
     yy0(i)=yy(i)
     zz0(i)=zz(i)
  enddo
  call SDclashscan
  do i=1,Nres
     sdclash0(i)=sdclash(i)
     sgd0(i)=sgd(i)
!print*,"i",i,resd(ca(i,1)),sdclash0(i),sgd0(i)
  enddo
  nsdclash0=nsdclash
  sdgg0=sdgg

do ii=1,100  !!!cycle to reduce clash
   do i=1,Nres
      iresin=ca(i,2)
!     nhra=hra(ca(i,2))-4  !!number of heavy atoms
      nhra=1  !!number of heavy atoms this is the best for near native structure
      if(sdclash0(i)<nhra.and.sgd(i)<0.5)cycle
      if(iresin<3.or.iresin.eq.8)cycle

      kbg=0
      ipca=ca(i,1)
      ipcg=ipca+4
      nsd=hra(iresin)-4  !!side chain atom number
19    continue
      NSCT=0
      if(iresin<=6)then
         NSCT=0
      elseif(iresin.eq.7.or.iresin.eq.12.or.iresin>16)then
         NSCT=int(rand(iseed)*2)
      else !9,13,14,15,16
         NSCT=int(rand(iseed)*3)  !!0 3
      endif
      if(NSCT.eq.0)then
         angg=(2.0*rand(iseed)-1.0)*pi   !*(101-ii)/100
         call CABrot(i,angg)
    !  elseif(NSCT.eq.1)then
    !     angg=(2.0*rand(iseed)-1.0)*pi !(rand(iseed)-0.5)*pi   !*(101-ii)/100
    !     call CBZrot(i,angg)
      elseif(NSCT.eq.1)then   
         angg=(2.0*rand(iseed)-1.0)*pi   !*(101-ii)/100
         call CBGROT(i,angg) 
      else
         angg=(2.0*rand(iseed)-1.0)*pi   !*(101-ii)/100
         call CGDROT(i,angg) 
      endif
      kbg=kbg+1
      
      if(NSCT.eq.0)then
         r=sqrt((xx(ipcg)-xx(ipca+1))**2+(yy(ipcg)-yy(ipca+1))**2+(zz(ipcg)-zz(ipca+1))**2)
         if(kbg<50.and.(r<2.76.or.(r>3.28.and.r<3.58).or.r>3.97))goto 19
         if(ca(i,2).eq.8)cycle
         r=sqrt((xx(ipcg)-xx(ipca-1))**2+(yy(ipcg)-yy(ipca-1))**2+(zz(ipcg)-zz(ipca-1))**2)
         if(kbg<50.and.(r<2.78.or.(r>3.28.and.r<3.62).or.r>3.9))goto 19
      endif
      if(.false..and.i>1.and.i<Nres)then
         ta=0
         tb=0
         tc=0
         do j=ipca+3,ipca+2+nsd
            ta=ta+xx(j)
            tb=tb+yy(j)
            tc=tc+zz(j)
         enddo
         ta=ta/nsd
         tb=tb/nsd
         tc=tc/nsd
         r=sqrt((ta-sgx(i))**2+(tb-sgy(i))**2+(tc-sgz(i))**2)
         if(kbg<20.and.r>sgd0(i))goto 19
      endif
   enddo  !!all movement finish

!!!energy term
  call SDclashscan
!print*,">>>>>>> scan2 finish",i
  if(nsdclash<nsdclash0)then
     do j=1,natom
        xx0(j)=xx(j)
        yy0(j)=yy(j)
        zz0(j)=zz(j)
     enddo
     do i=1,Nres
        sdclash0(i)=sdclash(i)
     enddo
     nsdclash0=nsdclash
  else
     do i=1,Nres
        ipca=ca(i,1)
        nhra=hra(ca(i,2))-2  !!number of heavy atoms
        if(sdclash(i)>=sdclash0(i))then
           do j=ipca+4,ipca+nhra
              xx(j)=xx0(j)
              yy(j)=yy0(j)
              zz(j)=zz0(j)
           enddo
        else
           nsdclash0=nsdclash0+sdclash(i)-sdclash0(i)
           sdclash0(i)=sdclash(i)
    !       sdgg0=sdgg0+sgd(i)-sgd0(i)
   !        sgd0(i)=sgd(i)
           do j=ipca+4,ipca+nhra
              xx0(j)=xx(j)
              yy0(j)=yy(j)
              zz0(j)=zz(j)
           enddo
        endif
     enddo
  endif
!if(mod(ii,50).eq.0)write(*,"I4,2I9,2f8.3"),ii,nsdclash0,nsdclash,sdgg0,sdgg
!if(mod(ii,50).eq.0)write(*,*),ii,nsdclash0,nsdclash,sdgg0,sdgg
enddo
do i=1,natom
   if(aty(i).ne."H")then
      xx(i)=xx0(i)
      yy(i)=yy0(i)
      zz(i)=zz0(i)
   else
      xx(i)=999.0
   endif
enddo
!do i=1,5
call SD_opt1
!enddo

277 continue
!call HADD

end subroutine SD_opt

!!intra-resiude, optimiza in the same residue, avoid the atom clash in same residue
!!EV, CG, bond length/angle
subroutine SD_opt1
implicit none
real,parameter::ce11=2.96,ce12=3.83,ce21=3.02,ce22=3.81,rcut=2.4
real bx,by,bz,bbx,bby,bbz,cx,cy,cz,r,r2,rcutt,rc1,rc2,rc3,rc4,fk,bd2,r0
integer i,j,ic,ipca,ipc,ik0,ipcg,icase,kgg

  do i=1,Nres
     if(ca(i,2)<2)cycle !!no side chain
     ipc=ca(i,1)+hra(ca(i,2))-2  !!all the side chain heavy atoms
     ipca=ca(i,1)     
     do ii=-1,2
        ic=ipca+ii  !!Ca
        do j=ipca+4,ipc
           if(ca(i,2)>=17.and.j-ipca>4)cycle
           bx=xx(j)-xx(ic)
           by=yy(j)-yy(ic)
           bz=zz(j)-zz(ic)
           r=sqrt(bx*bx+by*by+bz*bz)
           if(ca(i,2).ne.8)then
              rcutt=rcut
           else
              rcutt=rcut-0.16
           endif
           if(r<rcut)then
              if(r<1.0)r=1.0
              cx=xx(ic)+bx*rcutt/r-xx(j)
              cy=yy(ic)+by*rcutt/r-yy(j)
              cz=zz(ic)+bz*rcutt/r-zz(j)
              r=sqrt(cx*cx+cy*cy+cz*cz)
              fk=1.0
              if(r>0.5)fk=0.5/r
              do k=j,ipc
                 xx(k)=xx(k)+cx*fk
                 yy(k)=yy(k)+cy*fk
                 zz(k)=zz(k)+cz*fk
              enddo
           endif
        enddo
     enddo
     if(ca(i,2).eq.9.or.(ca(i,2).ge.13.and.ca(i,2).le.16))then  !!MET,LYS,GLU,GLN,ARG linear
        do j=ipca+3,ipc-1
           if((ca(i,2).eq.14.or.ca(i,2).eq.15).and.j-ipca.eq.6)cycle
           if(ca(i,2).eq.16.and.j-ipca.eq.8)cycle
           bx=xx(j+1)-xx(j-1)
           by=yy(j+1)-yy(j-1)
           bz=zz(j+1)-zz(j-1)
           r=sqrt(bx*bx+by*by+bz*bz)
           if(ca(i,2).eq.8)then
              rcutt=rcut-0.16
           elseif(ca(i,2)>=17)then
              rcutt=1.3
           else
              rcutt=rcut
           endif
           if(r<rcutt)then
              if(r<1.0)r=1.0
              !print*,atomty(j-1),resd(j),r,atomty(j+1),ca(i,2)
              cx=xx(j-1)+bx*rcutt/r-xx(j+1)
              cy=yy(j-1)+by*rcutt/r-yy(j+1)
              cz=zz(j-1)+bz*rcutt/r-zz(j+1)
              r=sqrt(cx*cx+cy*cy+cz*cz)
              fk=1.0
              if(r>0.5)fk=0.5/r
              do k=j+1,ipc
                 xx(k)=xx(k)+cx*fk
                 yy(k)=yy(k)+cy*fk
                 zz(k)=zz(k)+cz*fk
              enddo
           endif
        enddo
     endif
     do j=ipc,ipca+4,-1
        if(ca(i,2)>=17.and.j-ipca>4)cycle
        ic=bonda(j,1)
        bx=xx(j)-xx(ic)
        by=yy(j)-yy(ic)
        bz=zz(j)-zz(ic)
        r=sqrt(bx*bx+by*by+bz*bz)
        if(r>1.2*bond(j,2))then
           cx=xx(ic)+bond(j,2)*bx/r-xx(j)
           cy=yy(ic)+bond(j,2)*by/r-yy(j)
           cz=zz(ic)+bond(j,2)*bz/r-zz(j)
           r=sqrt(cx*cx+cy*cy+cz*cz)
           fk=1.0
           if(r>0.5)fk=0.5/r
           do k=j,ipc
              xx(k)=xx(k)+cx*fk
              yy(k)=yy(k)+cy*fk
              zz(k)=zz(k)+cz*fk
           enddo
        endif
     enddo
!!adjust CG position
     if(ca(i,2).eq.8)cycle
     ipcg=ipca+4
     bx=xx(ipcg)-xx(ipca+1) !CG-C
     by=yy(ipcg)-yy(ipca+1)
     bz=zz(ipcg)-zz(ipca+1)
     bbx=xx(ipcg)-xx(ipca-1) !CG-N
     bby=yy(ipcg)-yy(ipca-1)
     bbz=zz(ipcg)-zz(ipca-1)
     r=sqrt(bx*bx+by*by+bz*bz)
     r2=sqrt(bbx*bbx+bby*bby+bbz*bbz)
     if(((r>0.1.and.r<2.76).or.(r>3.28.and.r<3.58).or.r>3.97).or.((r2>0.1.and.r2<2.78).or.(r2>3.28.and.r2<3.62).or.r2>3.9))then
        cx=0
        cy=0
        cz=0
        rc1=abs(r-ce11)+abs(r2-ce21)
        rc2=abs(r-ce11)+abs(r2-ce22)
        rc3=abs(r-ce12)+abs(r2-ce21)
        rc4=abs(r-ce12)+abs(r2-ce22)
        if(rc1<=rc2.and.rc1<=rc3.and.rc1<=rc4)then
           icase=1
        elseif(rc2<=rc1.and.rc2<=rc3.and.rc2<=rc4)then
           icase=2
        elseif(rc3<=rc1.and.rc3<=rc2.and.rc3<=rc4)then
           icase=3
        else !if(rc1<rc2.and.rc1<rc3.and.rc1<rc4)then
           icase=4
        endif        
        if(icase<=2)then
           if(r<2.76.or.r>3.28)then
              cx=cx+bx*(ce11-r)/r
              cy=cy+by*(ce11-r)/r
              cz=cz+bz*(ce11-r)/r
           endif
        else
           if(r<3.58.or.r>3.97)then
              cx=cx+bx*(ce12-r)/r
              cy=cy+by*(ce12-r)/r
              cz=cz+bz*(ce12-r)/r
           endif
        endif
        if(icase.eq.1.or.icase.eq.3)then
           if(r2<2.78.or.r2>3.28)then
              cx=cx+bbx*(ce21-r2)/r2
              cy=cy+bby*(ce21-r2)/r2
              cz=cz+bbz*(ce21-r2)/r2
           endif
        else
           if(r2<3.62.or.r2>3.9)then
              cx=cx+bbx*(ce22-r2)/r2
              cy=cy+bby*(ce22-r2)/r2
              cz=cz+bbz*(ce22-r2)/r2
           endif
        endif
        r=sqrt(cx*cx+cy*cy+cz*cz)
        if(r>0.01)then
           fk=1.0
           if(r>0.5)fk=0.5/r
           do j=ipc,ipca+4,-1
              xx(j)=xx(j)+cx*fk
              yy(j)=yy(j)+cy*fk
              zz(j)=zz(j)+cz*fk
           enddo
        endif        
     endif  !!need adjust
!cycle
!!adjust all bond angle and bond length in the sidechain
701  continue
     do j=ipca+4,ipc
        if(ca(i,2)>=17.and.j-ipca>4)cycle
        kgg=0 !!no need for movement
        do k=1,bonda(j,5)
           if(aty(bonda(j,k)).eq."H")cycle 
           icc=bonda(j,k)
           bd=bond(j,2*k)
           goto 51
        enddo
51      continue
        do k=1,bonda(icc,5) !!the connect heavy atom  !!ip2-icc-j (1,2,3)
           if(bonda(icc,k)>=j.or.aty(bonda(icc,k)).eq."H" )cycle
           ip2=bonda(icc,k)  !!ip2-icc-i check and bond length and bond angle
           bd2=bond(icc,2*k)
           goto 52
        enddo
52      continue
        do ii=Lang*(i-1)+1,i*Lang
           if(anga(ii,1)<1)cycle
           if(anga(ii,2)==icc.and.(anga(ii,1)==ip2.and.anga(ii,3)==j).or.(anga(ii,1)==j.and.anga(ii,3)==ip2))then
              angg=ang(ii,2)*hpi
              goto 20
           endif
        enddo
20      continue 
!print*,i,atomty(j),angg,ii,bd2
        bx=xx(j)-xx(icc) !CG-CB etc.
        by=yy(j)-yy(icc)
        bz=zz(j)-zz(icc)
        bbx=xx(j)-xx(ip2) !CG-CA
        bby=yy(j)-yy(ip2)
        bbz=zz(j)-zz(ip2)
        cx=0
        cy=0
        cz=0
        r=sqrt(bx*bx+by*by+bz*bz)
        r2=sqrt(bbx*bbx+bby*bby+bbz*bbz)    
        r0=sqrt(r*r+bd2*bd2-2.0*r*bd2*cos(angg))
        if(abs(r-bd)>0.1*bd)then
           cx=cx+(bd-r)/r*bx
           cy=cy+(bd-r)/r*by
           cz=cz+(bd-r)/r*bz
        endif
        if(abs(r2-r0)>0.1*r0)then
           cx=cx+(r0-r2)/r2*bbx
           cy=cy+(r0-r2)/r2*bby
           cz=cz+(r0-r2)/r2*bbz
        endif
        r=sqrt(cx*cx+cy*cy+cz*cz)
        if(r>0.01)then
           kgg=kgg+1
           do k=j,ipc
              xx(k)=xx(k)+cx
              yy(k)=yy(k)+cy
              zz(k)=zz(k)+cz
           enddo
!print*,"kgg",j,kgg,atomty(j),r
       !    if(r<0.05)goto 21
           if(kgg<20)goto 20
        endif
21         continue
     enddo
  end do
end subroutine SD_opt1
!!this is for 1/5 secondary structure
!!<NCa(i+1)CA 16(0.5) degree
!!N-C 2.43(0.05,same residue) i; Ca(i)-N(i+1): 2.483 (else), 2.359 (PRO), Ca-C 1.49, ca(i+1)-C: 2.45(0.3)
!!<CCaN(i+1)=    , it's negative to <CCaCa(i+1)
!!he: i=(r13-5)/0.1, j=(r14 -4.5)/0.1, k=(r15-4.5)/0.1, m=(dihe(ca,ca(i+2),ca(i+1),C)+pi)/5 degrees
!!cos(<CCaCa(i+1))=0.9425-0.05*(rcaca(i+1)-3.8)
!!cos(<CCaN(i+1))=0.875-0.04*(rcaca(i+1)-3.8), rCaN(i+1) 2.483(2.359)+0.1*(rcaca(i+1)-3.8)
!!score for C: 2.38-rNC/rNC-2.48 + 2.15-rCCa(i+1)/rCCa(i+1)-2.75 + g2, g2=max(value*0.01,0.3) coil and g2=max(value*0.05,0.3) sheet, g2=max(value*0.001,0.3) for helix  (in case large than boundary)

!!i-1,i,i+1,i+2 4CA to dicide the C-N between i-i+1
!!7 dimension r14,r13,r24,ipx(i+1 project on i->i+2),ipy(i+1 project on i->i-1), ipz(i+1 projext on 
!!i->i+2 X i->i1), and 1(coil) 2 helix and 3 sheet
!!the coordinates the the projection of C and N on the new coordinate system
!iflag=0  may keep the existed backbone heavy atoms
!!iflag=1 insert all 
!!iflag=2 only change those need adjust region
!!iflag==3 only check CB
subroutine BBADD(iflag)
implicit none
real a(3,3),b(3),bb(3)
real vx23,vy23,vz23,vx14,vy14,vz14,r14,r13,r24,r23,px,py,pz,fk
real vxa,vya,vza,vxb,vyb,vzb,vxc,vyc,vzc,vxx,vyy,vzz
real raa,rbb,rcc,rdd,r,fr
integer i1,i2,i3,i4,i5,i6,i7,i8,i11,i22,i33,i44,i55,j1,j2,j3,j4,j5
integer i14,i13,i24,ipx,ipy,ngk,nggk,kkg,iss
integer i,ii,j,kk,kg,kd,ipn,ipc,ipb,irn,iresin,ir
integer ip1,ip2,ip3,ip4,iflag
ngk=0  !!use neighbour
nggk=0
kg=0
kkg=0
kk=0

if(iflag.eq.3)goto 713
!!for ter treatment
ip1=ca(2,1)
xx(0)=2*xx(2)-xx(ip1)
yy(0)=2*yy(2)-yy(ip1)
zz(0)=2*zz(2)-zz(ip1)
ip1=ca(Nres-2,1)
ip2=ca(Nres-1,1)
ip3=ca(Nres,1)
xx(natom+2)=2*xx(ip3)-xx(ip2)
yy(natom+2)=2*yy(ip3)-yy(ip2)
zz(natom+2)=2*zz(ip3)-zz(ip2)
xx(natom+3)=2*xx(ip3)-xx(ip1)
yy(natom+3)=2*yy(ip3)-yy(ip1)
zz(natom+3)=2*zz(ip3)-zz(ip1)

do i=1,Nres
   if(iflag.eq.2.and.hbvar(i)>1)cycle  !!no need for adjustment
   if(i<2)then
      ip1=0
      ip3=ca(i+1,1)
      ip4=ca(i+2,1)
   elseif(i==Nres-1)then
      ip1=ca(i-1,1)
      ip3=ca(i+1,1)
      ip4=natom+2
   elseif(i==Nres)then
      ip1=ca(i-1,1)
      ip3=natom+2
      ip4=natom+3
   else
      ip1=ca(i-1,1)
      ip3=ca(i+1,1)
      ip4=ca(i+2,1) 
   endif
   ip2=ca(i,1)
   if(iflag.eq.0.and.xx(ip2+1)<990.0.and.xx(ip3-1)<999.0)cycle

   iss=seq(i,1)   !!!1 coil, 2 helix, 4 sheet
   iresin=ca(i,2)
   irn=min(iresin,2)

   vx23=xx(ip3)-xx(ip2)
   vy23=yy(ip3)-yy(ip2)
   vz23=zz(ip3)-zz(ip2)
   vx14=xx(ip4)-xx(ip1)
   vy14=yy(ip4)-yy(ip1) 
   vz14=zz(ip4)-zz(ip1)
   r14=sqrt((vx14**2+vy14**2+vz14**2))
   r13=sqrt((xx(ip3)-xx(ip1))**2+(yy(ip3)-yy(ip1))**2+(zz(ip3)-zz(ip1))**2)
   r24=sqrt((xx(ip4)-xx(ip2))**2+(yy(ip4)-yy(ip2))**2+(zz(ip4)-zz(ip2))**2)
   r23=sqrt(vx23**2+vy23**2+vz23**2)
   if(r23<0.0001.or.r14<0.0001)then
      kk=0
      goto 702
   endif  
   i14=max(0,int((r14-4.45)/0.25))
   i13=max(0,int((r13-4.85)/0.25))
   i24=max(0,int((r24-4.85)/0.25))

   vxa=vx14/r14+vx23/r23
   vya=vy14/r14+vy23/r23
   vza=vz14/r14+vz23/r23
   vxb=vx14/r14-vx23/r23
   vyb=vy14/r14-vy23/r23
   vzb=vz14/r14-vz23/r23

   raa=sqrt(vxa**2+vya**2+vza**2)
   rbb=sqrt(vxb**2+vyb**2+vzb**2)  
   if(raa<0.0001.or.rbb<0.0001)then
      kk=0
      goto 702
   endif
   vxa=vxa/raa
   vya=vya/raa
   vza=vza/raa
   vxb=vxb/rbb
   vyb=vyb/rbb
   vzb=vzb/rbb

   vxc=vya*vzb-vza*vyb
   vyc=vza*vxb-vxa*vzb
   vzc=vxa*vyb-vya*vxb

   px=vx23*vxa+vy23*vya+vz23*vza
   py=vx23*vxb+vy23*vyb+vz23*vzb 
   ipx=max(0,int((px-3.15)/0.15))
   ipy=max(0,int((py+2.1)/0.2))

   if(i14>km14)i14=km14
   if(i13>km13)i13=km13
   if(i24>km24)i24=km24
   if(ipx>kmpx)ipx=kmpx
   if(ipy>kmpy)ipy=kmpy

!write(*,"a3,8I3,i7"),">>>",i,i14,i13,i24,ipx,ipy,iresin,ca(i,2),ca(i,1)
   kk=0
   if(iss.eq.1)then
      if(bbdatc(i14,i13,i24,ipx,ipy,iresin,1)>0)then
         kk=bbdatc(i14,i13,i24,ipx,ipy,iresin,1)
         goto 701
      endif
      if(bbdatb(i14,i13,i24,ipx,ipy,iresin,1)>0)then
         kk=bbdatb(i14,i13,i24,ipx,ipy,iresin,1)
         goto 701
      endif
      if(bbdata(i14,i13,i24,ipx,ipy,iresin,1)>0)then
         kk=bbdata(i14,i13,i24,ipx,ipy,iresin,1)
         goto 701
      endif
   elseif(iss.eq.2)then !!helix
      if(bbdata(i14,i13,i24,ipx,ipy,iresin,1)>0)then
         kk=bbdata(i14,i13,i24,ipx,ipy,iresin,1)
         goto 701
      endif
      if(bbdatc(i14,i13,i24,ipx,ipy,iresin,1)>0)then
         kk=bbdatc(i14,i13,i24,ipx,ipy,iresin,1)
         goto 701
      endif
   else  !!sheet
      if(bbdatb(i14,i13,i24,ipx,ipy,iresin,1)>0)then
         kk=bbdatb(i14,i13,i24,ipx,ipy,iresin,1)
         goto 701
      endif
      if(bbdatc(i14,i13,i24,ipx,ipy,iresin,1)>0)then
         kk=bbdatc(i14,i13,i24,ipx,ipy,iresin,1)
         goto 701
      endif
   endif
      
   ngk=ngk+1
   if(iresin.ne.8)then  !proline
      if(iss.eq.1)then
         do ir=20,irn,-1
            if(bbdatc(i14,i13,i24,ipx,ipy,ir,1)>0)then
               kk=bbdatc(i14,i13,i24,ipx,ipy,ir,1)
               goto 701
            endif
         enddo
      elseif(iss.eq.2)then
         do ir=20,irn,-1
            if(bbdata(i14,i13,i24,ipx,ipy,ir,1)>0)then
               kk=bbdata(i14,i13,i24,ipx,ipy,ir,1)
               goto 701
            endif
         enddo
      else
         do ir=20,irn,-1
            if(bbdatb(i14,i13,i24,ipx,ipy,ir,1)>0)then
               kk=bbdatb(i14,i13,i24,ipx,ipy,ir,1)
               goto 701
            endif
         enddo
      endif
   endif
      
   nggk=nggk+1
   kd=1
700 continue
   i1=max(0,i14-kd)
   i2=max(0,i13-kd)
   i3=max(0,i24-kd)
   i4=max(0,ipx-kd)
   i5=max(0,ipy-kd)
   i11=min(km14,i14+kd)
   i22=min(km13,i13+kd)
   i33=min(km24,i24+kd)
   i44=min(kmpx,ipx+kd)
   i55=min(kmpy,ipy+kd)
   do j1=i1,i11
      do j2=i2,i22
         do j3=i3,i33
            do j4=i4,i44
               do j5=i5,i55
                  do ir=irn,20
                     if((iresin.eq.8.or.ir.eq.8).and.ir.ne.iresin)cycle   !!!proline is special
                     if(iss.eq.1)then  !!coil
                        if(bbdatc(j1,j2,j3,j4,j5,ir,1)>0)then
                           kk=bbdatc(j1,j2,j3,j4,j5,ir,1)
                           i14=j1
                           i13=j2
                           i24=j3
                           ipx=j4
                           ipy=j5 
                           goto 701
                        endif
                     elseif(iss.eq.2)then !!helix
                        if(bbdata(j1,j2,j3,j4,j5,ir,1)>0)then
                           kk=bbdata(j1,j2,j3,j4,j5,ir,1)
                           i14=j1
                           i13=j2
                           i24=j3
                           ipx=j4
                           ipy=j5 
                           goto 701
                        endif
                     else  !!sheet
                        if(bbdatb(j1,j2,j3,j4,j5,ir,1)>0)then
                           kk=bbdatb(j1,j2,j3,j4,j5,ir,1)
                           i14=j1
                           i13=j2
                           i24=j3
                           ipx=j4
                           ipy=j5 
                           goto 701
                        endif
                     endif
                  enddo
               enddo
            enddo
         enddo
      enddo
   enddo
   kd=kd+1
   if(kd<2)goto 700
701 continue
!   write(*,"A3,8I12,I9"),"xx",i,i14,i13,i24,ipx,ipy,iresin,nggk,kk
   if(iflag.eq.2)then
      if(kk>0)then
       !  do i8=1,kmnr
         i8=mod(int(rand(iseed)*998631),kmnr)+1
         if(iss.eq.1)then
            if(bbdatc(i14,i13,i24,ipx,ipy,iresin,1)>0)then
               kk=bbdatc(i14,i13,i24,ipx,ipy,iresin,i8)
               if(kk.ne.bbindex(i))goto 702
            endif
         elseif(iss.eq.2)then
            if(bbdata(i14,i13,i24,ipx,ipy,iresin,1)>0)then
               kk=bbdata(i14,i13,i24,ipx,ipy,iresin,i8)
               if(kk.ne.bbindex(i))goto 702
            endif
         else
            if(bbdatb(i14,i13,i24,ipx,ipy,iresin,1)>0)then
               kk=bbdatb(i14,i13,i24,ipx,ipy,iresin,i8)
               if(kk.ne.bbindex(i))goto 702
            endif
         endif
         !    enddo
      endif
      ipn=i14
      ipc=i13
      ipb=i24
      i1=Nint(3*(2*rand(iseed)-1))
      i2=Nint(2*(2*rand(iseed)-1))
      i3=Nint(2*(2*rand(iseed)-1))
      i4=Nint(2*(2*rand(iseed)-1))
      i5=Nint(2*(2*rand(iseed)-1))
      i14=i14+i1
      if(i14<0.or.i14>km14)i14=i14-2*i1
      i13=i13+i2
      if(i13<0.or.i13>km13)i13=i13-2*i2
      i24=i24+i3
      if(i24<0.or.i24>km24)i24=i24-2*i3

      if(seq(i,1).eq.4)then
         i14=max(i14,ipn)
         i13=max(i13,ipc)
         i24=max(i24,ipb)
      endif
      ipx=ipx+i4
      if(ipx<0.or.ipx>kmpx)ipx=ipx-2*i4
      ipy=ipy+i5
      if(ipy<0.or.ipy>kmpy)ipy=ipy-2*i5
!  write(*,"A2,7I3,I16,2I9"),"bb",i,i14,i13,i24,ipx,ipy,iresin,irn,kk,bbindex(i)
         do ir=irn,20
            if((iresin.eq.8.or.ir.eq.8).and.ir.ne.iresin)cycle   !!!proline is special
            if(iss.eq.1)then
               if(bbdatc(i14,i13,i24,ipx,ipy,ir,1)>0)then
                  kk=bbdatc(i14,i13,i24,ipx,ipy,ir,1)
                  if(kk.ne.bbindex(i))goto 702
               endif
            elseif(iss.eq.2)then
               if(bbdata(i14,i13,i24,ipx,ipy,ir,1)>0)then
                  kk=bbdata(i14,i13,i24,ipx,ipy,ir,1)
                  if(kk.ne.bbindex(i))goto 702
               endif
            else
               if(bbdatb(i14,i13,i24,ipx,ipy,ir,1)>0)then
                  kk=bbdatb(i14,i13,i24,ipx,ipy,ir,1)
                  if(kk.ne.bbindex(i))goto 702
               endif
            endif
         enddo
 !   write(*,"A2,8I3,2I9"),"cc",i,i14,i13,i24,ipx,ipy,iresin,nggk,kk,bbindex(i)

      i1=int((2*rand(iseed)-1)*1970)
      kk=kk+i1

      if(iss.eq.1)then
         if(kk<1.or.kk>nc)kk=kk-2*i1
      elseif(iss.eq.2)then
         if(kk<nc+1.or.kk>nc+na)kk=kk-2*i1
      else
         if(kk<na+nc+1.or.kk>n1)kk=kk-2*i1
      endif
      goto 702
   endif  !!2 optimization

!  write(*,"A2,8I3,2I9"),"aa",i,i14,i13,i24,ipx,ipy,iresin,nggk,kk,bbindex(i)

702 continue

   if(kk<1)then
      kg=kg+1
      if(iss.eq.1)then
         kk=int(rand(iseed)*nc)+1
      elseif(iss.eq.2)then
         kk=int(rand(iseed)*na)+nc+1
      else
         kk=int(rand(iseed)*nb)+na+nc+1
      endif
   endif

bbindex(i)=kk  !the index
   do j=0,1
      b(1)=bbcoord(kk,1+3*j)
      b(2)=bbcoord(kk,2+3*j)
      b(3)=bbcoord(kk,3+3*j)
      a(1,1)=vxa
      a(1,2)=vya
      a(1,3)=vza
      a(2,1)=vxb
      a(2,2)=vyb
      a(2,3)=vzb
      a(3,1)=vxc
      a(3,2)=vyc
      a(3,3)=vzc
    !  do i1=1,3
    !     b(i1)=bb(1)*a(i1,1)+bb(2)*a(i1,2)+bb(3)*a(i1,3)
    !  enddo     
      if(abs(b(1))+abs(b(2))+abs(b(3))>0.001)then
         call gaussj(a,b)
      else
         cycle
      endif     
      
      r=sqrt(b(1)**2+b(2)**2+b(3)**2)
      if(j.eq.1)then   !!C
         if(r>1.7.or.(r<1.3.and.r>0.01))then
            fk=1.49/r
         elseif(r<0.01)then
            fk=0.33
            b(1)=xx(ip3)-xx(ip2)
            b(2)=yy(ip3)-yy(ip2)
            b(3)=zz(ip3)-zz(ip2)
         else
            fk=1.0
         endif
         xx(ip2+1)=b(1)*fk+xx(ip2)
         yy(ip2+1)=b(2)*fk+yy(ip2)
         zz(ip2+1)=b(3)*fk+zz(ip2)
      elseif(j.eq.0)then    !!N
         if(r>2.7.or.(r<2.1.and.r>0.01))then
            fk=1.45/r
         elseif(r<0.01)then
            fk=0.67
            b(1)=xx(ip3)-xx(ip2)
            b(2)=yy(ip3)-yy(ip2)
            b(3)=zz(ip3)-zz(ip2)
         else
            fk=1.0
         endif
         xx(ip3-1)=b(1)*fk+xx(ip2)
         yy(ip3-1)=b(2)*fk+yy(ip2)
         zz(ip3-1)=b(3)*fk+zz(ip2)   
      endif
   enddo

goto 771
!if(iflag.ne.1)goto 771
!!adjust C
   if(i>1)then
      px=xx(ip2+1)-xx(ip2-1)  !!adjust C
      py=yy(ip2+1)-yy(ip2-1)
      pz=zz(ip2+1)-zz(ip2-1)
      r=sqrt(px**2+py**2+pz**2)
      if(r>2.7.or.(r<2.2.and.r>0.01))then
         fk=(r-2.43)/r
!         print*,"adjust C",i,resd(ip2),atomty(ip2+1),r,fk
         xx(ip2+1)=xx(ip2+1)-fk*px
         yy(ip2+1)=yy(ip2+1)-fk*py
         zz(ip2+1)=zz(ip2+1)-fk*pz
         px=xx(ip2+1)-xx(ip2)  !!adjust C
         py=yy(ip2+1)-yy(ip2)
         pz=zz(ip2+1)-zz(ip2)
         r=sqrt(px**2+py**2+pz**2)
         if(r>1.7.or.(r<1.2.and.r>0.01))then
            fk=(r-1.49)/r
            xx(ip2+1)=xx(ip2+1)-fk*px
            yy(ip2+1)=yy(ip2+1)-fk*py
            zz(ip2+1)=zz(ip2+1)-fk*pz
         endif
      endif
   endif

   px=xx(ip3-1)-xx(ip3)  !!adjust N
   py=yy(ip3-1)-yy(ip3)
   pz=zz(ip3-1)-zz(ip3)        
   r=sqrt(px**2+py**2+pz**2)
   if(r>1.58.or.(r<1.3.and.r>0.01))then  !!1.55 1.2
      r=sqrt((xx(ip2+1)-xx(ip2))**2+(xx(ip2+1)-xx(ip2))**2+(xx(ip2+1)-xx(ip2))**2)
      if(r>0.01)then
         fk=1.43/r
      else
         fk=10.0
      endif
      xx(ip3-1)=xx(ip3)-(xx(ip2+1)-xx(ip2))*fk
      yy(ip3-1)=yy(ip3)-(yy(ip2+1)-yy(ip2))*fk
      zz(ip3-1)=zz(ip3)-(zz(ip2+1)-zz(ip2))*fk
      px=xx(ip3-1)-xx(ip2+1)
      py=yy(ip3-1)-yy(ip2+1)
      pz=zz(ip3-1)-zz(ip2+1)        
      r=sqrt(px**2+py**2+pz**2)
      if(i<Nres.and.(r>1.6.or.(r<1.1.and.r>0.01)))then
         fk=(r-1.345)/r
!         print*,"adjust N",i,resd(ip3),atomty(ip3-1),r,fk
         xx(ip3-1)=xx(ip3-1)-fk*px
         yy(ip3-1)=yy(ip3-1)-fk*py
         zz(ip3-1)=zz(ip3-1)-fk*pz
      endif
   endif
771 continue
end do

!open(31,file="testaa00")
!do i=1,natom
!   if(xx(i)<999.0)then
!      k=i
!      write(31,129),k,atomty(i),resd(i),ress(i),xx(i),yy(i),zz(i),ASSrd(i)
!  endif
!enddo
!close(31)

call ADD_HO
713 continue
end subroutine BBADD

!!this subroutine add the H and O in backbone
subroutine ADD_HO
implicit none
real px,py,pz,vxx,vyy,vzz,r
integer i,ip1,ip2,ip3,iresin,ihra
!!insert O
do i=1,Nres
   ip2=ca(i,1)
   if(i<Nres)then
      ip3=ca(i+1,1)
   else
      ip3=natom+2
   endif
   iresin=ca(i,2)

   if(i<Nres)then
      vxx=0.5*(xx(ip3-1)+xx(ip2))
      vyy=0.5*(yy(ip3-1)+yy(ip2))
      vzz=0.5*(zz(ip3-1)+zz(ip2))
      px=xx(ip2+1)
      py=yy(ip2+1)
      pz=zz(ip2+1)
      r=sqrt((px-vxx)**2+(py-vyy)**2+(pz-vzz)**2)
      if(r>0.01)then  !!O
         xx(ip2+2)=px+1.23*(px-vxx)/r
         yy(ip2+2)=py+1.23*(py-vyy)/r
         zz(ip2+2)=pz+1.23*(pz-vzz)/r
      endif
   else
      ihra=hra(iresin)
      ip2=ca(Nres-1,1)
      ip1=ca(Nres,1)
      px=xx(ip1)-xx(ip2+2)
      py=yy(ip1)-yy(ip2+2)
      pz=zz(ip1)-zz(ip2+2)
      r=sqrt(px*px+py*py+pz*pz)
      if(r>0.01)then
         xx(ip1+2)=xx(ip1+1)+px*1.23/r
         yy(ip1+2)=yy(ip1+1)+py*1.23/r
         zz(ip1+2)=zz(ip1+1)+pz*1.23/r
         px=xx(ip1+1)-0.55*xx(ip1)-0.45*xx(ip1+2)
         py=yy(ip1+1)-0.55*yy(ip1)-0.45*yy(ip1+2)
         pz=zz(ip1+1)-0.55*zz(ip1)-0.45*zz(ip1+2)
      endif
      r=sqrt(px*px+py*py+pz*pz)
      if(r>0.01)then !!OXT
         xx(ip1+ihra-1)=xx(ip1+1)+px*1.23/r
         yy(ip1+ihra-1)=yy(ip1+1)+py*1.23/r
         zz(ip1+ihra-1)=zz(ip1+1)+pz*1.23/r
      endif
   endif
!!!Add H atoms
   if(iresin.eq.8.or.i.eq.1)cycle
   ip1=ca(i,1)+hra(iresin)-1  !!H id
   if(i.eq.Nres)ip1=ip1+1
   ip2=ca(i,1)-1 !N
   ip3=ca(i-1,1)+1 !C for former residue
   px=xx(ip2)-0.5*(xx(ca(i,1))+xx(ip3))
   py=yy(ip2)-0.5*(yy(ca(i,1))+yy(ip3))
   pz=zz(ip2)-0.5*(zz(ca(i,1))+zz(ip3))
   r=sqrt(px*px+py*py+pz*pz)
   if(r<0.001)r=0.001
   xx(ip1)=xx(ip2)+px/r
   yy(ip1)=yy(ip2)+py/r
   zz(ip1)=zz(ip2)+pz/r
   !print*,"selection",i,kk,ipx,ipy
 enddo  !!!1,Nres

!!insert N-Term
 ip2=ca(2,1)
 xx(1)=xx(ip2-1)-xx(ip2)+xx(2)
 yy(1)=yy(ip2-1)-yy(ip2)+yy(2)
 zz(1)=zz(ip2-1)-zz(ip2)+zz(2)
!!C-Term
! xx(ca(Nres,1)+2)=999.0
end subroutine ADD_HO

!!!this subroutine to build CB based on the neighbouring CA positions
!!ca(i,2) the type of amino acids
subroutine CB_ADD
implicit none
real,dimension(20)::xcb1=(/0.0,0.249,0.192,0.189,0.271,0.218,0.276,0.154,0.235,0.14,0.047,0.244,0.215,0.239,0.216,0.222,0.152,0.21,0.207,0.228/)
real,dimension(20)::ycb1=(/0.0,-1.118,-1.089,-1.073,-1.095,-1.089,-1.1,-1.091,-1.098,-1.049,-1.034,-1.107,-1.105,-1.11,-1.102,-1.102,-1.07,-1.085,-1.087,-1.095/)
real,dimension(20)::zcb1=(/0.0,0.976,1.016,1.025,1.019,1.029,1.015,0.971,1.002,1.05,1.064,0.996,0.994,0.99,0.999,0.996,1.031,1.012,1.01,1.006/)
real,dimension(20)::xcb2=(/0.0,0.113,0.13,0.098,0.09,0.145,0.085,0.096,0.102,0.093,0.073,0.107,0.108,0.109,0.101,0.1,0.082,0.097,0.098,0.108/)
real,dimension(20)::ycb2=(/0.0,-0.736,-0.765,-0.726,-0.659,-0.74,-0.654,-0.827,-0.702,-0.749,-0.738,-0.692,-0.718,-0.717,-0.71,-0.719,-0.729,-0.706,-0.709,-0.707/)
real,dimension(20)::zcb2=(/0.0,1.294,1.276,1.308,1.373,1.31,1.376,1.258,1.327,1.287,1.292,1.334,1.311,1.309,1.316,1.31,1.305,1.323,1.322,1.322/)
real r1,r2,r3,cosa,vx1,vx2,vy1,vy2,vz1,vz2,xnx,xny,xnz,ynx,yny,ynz,znx,zny,znz
integer i,k,ip,ip1,ip2
!print*,"build CB atoms"
do i=2,Nres-1
   ip=ca(i,1)
   if(ca(i,2)<2.or.xx(ip+3)<990.0)cycle
   ip1=ca(i-1,1)
   ip2=ca(i+1,1)
   ax=xx(ip)
   ay=yy(ip)
   az=zz(ip)
   vx1=ax-xx(ip1)
   vy1=ay-yy(ip1)
   vz1=az-zz(ip1)
   vx2=-ax+xx(ip2)
   vy2=-ay+yy(ip2)
   vz2=-az+zz(ip2) 
   r1=sqrt(vx1*vx1+vy1*vy1+vz1*vz1)
   r2=sqrt(vx2*vx2+vy2*vy2+vz2*vz2)
   r3=(xx(ip2)-xx(ip1))**2+(yy(ip2)-yy(ip1))**2+(zz(ip2)-zz(ip1))**2
   if(r1*r2<0.001)goto 201 !cycle   !!this is irreugular structure
   cosa=(r1*r1+r2*r2-r3)/2.0/r1/r2  !!cosing value

   vx1=vx1/r1
   vy1=vy1/r1
   vz1=vz1/r1
   vx2=vx2/r2
   vy2=vy2/r2
   vz2=vz2/r2
   xnx=vx1+vx2
   xny=vy1+vy2
   xnz=vz1+vz2
   ynx=vx1-vx2
   yny=vy1-vy2
   ynz=vz1-vz2
   r1=sqrt(xnx*xnx+xny*xny+xnz*xnz)
   r2=sqrt(ynx*ynx+yny*yny+ynz*ynz)
   if(r1*r2<0.001)goto 201 !cycle   !!this is irreugular structure
   xnx=xnx/r1
   xny=xny/r1
   xnz=xnz/r1
   ynx=ynx/r2
   yny=yny/r2
   ynz=ynz/r2
   znx=yny*xnz-ynz*xny
   zny=ynz*xnx-ynx*xnz
   znz=ynx*xny-yny*xnx
   r1=sqrt(znx*znx+zny*zny+znz*znz)
   if(r1<0.001)goto 201 !cycle   !!this is irreugular structure
   znx=znx/r1
   zny=zny/r1
   znz=znz/r1
   k=ca(i,2)
   if(cosa>-0.258819)then !! <105o, chi +/- 120o
      xx(ip+3)=ax+xcb1(k)*xnx+ycb1(k)*znx+zcb1(k)*ynx
      yy(ip+3)=ay+xcb1(k)*xny+ycb1(k)*zny+zcb1(k)*yny
      zz(ip+3)=az+xcb1(k)*xnz+ycb1(k)*znz+zcb1(k)*ynz
   else  !!extent form
      xx(ip+3)=ax+xcb2(k)*xnx+ycb2(k)*znx+zcb2(k)*ynx
      yy(ip+3)=ay+xcb2(k)*xny+ycb2(k)*zny+zcb2(k)*yny
      zz(ip+3)=az+xcb2(k)*xnz+ycb2(k)*znz+zcb2(k)*ynz
   endif
201 continue
enddo
end subroutine CB_ADD

!!add the side chain mass center
!!ca(i,2) the type of amino acids
!!i) ADD_SG
!!ii) adjust SG direction, search space positive to Ca->Cb
!!adjust SG
subroutine SG_ADD
implicit none
real,dimension(20)::xcb1=(/0.0,0.249,0.169,-0.073,0.274,0.09,0.1,-0.743,-0.049,-0.221,-0.357,-0.057,0.027,-0.013,-0.086,0.113,-0.221,0.111,0.128,0.476/)
real,dimension(20)::ycb1=(/0.0,-1.118,-1.369,-1.201,-1.162,-1.296,-1.363,-1.563,-1.246,-1.249,-1.096,-1.161,-1.616,-1.554,-1.439,-1.932,-1.138,-0.984,-1.035,-1.156/)
real,dimension(20)::zcb1=(/0.0,0.976,1.103,1.476,1.48,1.346,1.769,0.438,2.308,1.769,1.849,2.128,2.597,2.219,2.296,2.933,2.165,2.447,2.604,2.541/)
real,dimension(20)::xcb2=(/0.0,0.113,0.227,0.084,0.093,0.07,-0.105,-0.98,0.094,0.334,0.097,0.003,-0.019,0.101,0.041,-0.02,-0.133,-0.363,-0.375,-0.058/)
real,dimension(20)::ycb2=(/0.0,-0.736,-0.966,-0.738,-0.583,-0.854,-0.601,-1.183,-0.723,-0.664,-0.699,-0.393,-0.745,-0.793,-0.707,-0.998,-0.598,-0.632,-0.601,-0.427/)
real,dimension(20)::zcb2=(/0.0,1.294,1.427,1.712,1.799,1.633,2.135,0.976,2.61,1.992,1.962,2.4,2.972,2.684,2.666,3.394,2.363,2.507,2.706,2.894/)
real,dimension(Lmax)::SDscore,CAangle   !!side chain packing score for SG
real r1,r2,r3,cosa,vx1,vx2,vy1,vy2,vz1,vz2,xnx,xny,xnz,ynx,yny,ynz,znx,zny,znz
real rsdr0,ep0,ep,eep0,rf
integer i,k,ires2,ip,ip1,ip2,ip3,ipd,ix,iy,iz,ix2,iy2,iz2,iax,iay,iaz,ibx,iby,ibz,ixn,iyn,izn
integer ichange,margin,id,iresin
!print*,"build SG"
do i=2,Nres-1
   if(ca(i,2)<3)cycle
   ip=ca(i,1)
   ip1=ca(i-1,1)
   ip2=ca(i+1,1)
   ax=xx(ip)
   ay=yy(ip)
   az=zz(ip)
   vx1=ax-xx(ip1)
   vy1=ay-yy(ip1)
   vz1=az-zz(ip1)
   vx2=-ax+xx(ip2)
   vy2=-ay+yy(ip2)
   vz2=-az+zz(ip2) 
   r1=sqrt(vx1*vx1+vy1*vy1+vz1*vz1)
   r2=sqrt(vx2*vx2+vy2*vy2+vz2*vz2)
   r3=(xx(ip2)-xx(ip1))**2+(yy(ip2)-yy(ip1))**2+(zz(ip2)-zz(ip1))**2
   if(r1*r2<0.001)cycle   !!this is irreugular structure
   cosa=(r1*r1+r2*r2-r3)/2.0/r1/r2  !!cosing value
   CAangle(i)=cosa !>-0.258819, using 1, else use 2; for SER and CYS, <-0.5, rotate O closc to C
   vx1=vx1/r1
   vy1=vy1/r1
   vz1=vz1/r1
   vx2=vx2/r2
   vy2=vy2/r2
   vz2=vz2/r2
   xnx=vx1+vx2
   xny=vy1+vy2
   xnz=vz1+vz2
   ynx=vx1-vx2
   yny=vy1-vy2
   ynz=vz1-vz2
   r1=sqrt(xnx*xnx+xny*xny+xnz*xnz)
   r2=sqrt(ynx*ynx+yny*yny+ynz*ynz)
   xnx=xnx/r1
   xny=xny/r1
   xnz=xnz/r1
   ynx=ynx/r2
   yny=yny/r2
   ynz=ynz/r2
   znx=yny*xnz-ynz*xny
   zny=ynz*xnx-ynx*xnz
   znz=ynx*xny-yny*xnx
   r1=sqrt(znx*znx+zny*zny+znz*znz)
   znx=znx/r1
   zny=zny/r1
   znz=znz/r1
   k=ca(i,2)
   if(cosa>-0.258819)then !! <105o, chi +/- 120o
!if(ca(i,2).eq.3)print*,"1 SER",i
      sgx(i)=ax+xcb1(k)*xnx+ycb1(k)*znx+zcb1(k)*ynx
      sgy(i)=ay+xcb1(k)*xny+ycb1(k)*zny+zcb1(k)*yny
      sgz(i)=az+xcb1(k)*xnz+ycb1(k)*znz+zcb1(k)*ynz
   else  !!extent form
      sgx(i)=ax+xcb2(k)*xnx+ycb2(k)*znx+zcb2(k)*ynx
      sgy(i)=ay+xcb2(k)*xny+ycb2(k)*zny+zcb2(k)*yny
      sgz(i)=az+xcb2(k)*xnz+ycb2(k)*znz+zcb2(k)*ynz
   endif
!   if(ca(i,2)>=10.and.ca(i,2)<=12)then !!ASP, ASN,LEU.
!      xx(ip+4)=sgx(i)
!      yy(ip+4)=sgy(i)
!      zz(ip+4)=sgz(i)     
!   endif
enddo
goto 76
!!adjust SG
call IMERGE(2)  !inly detect the Sg
do i=2,Nres-1
   if(ca(i,2)<3)cycle
   ip=ca(i,1)
   ax=xx(ip)
   ay=yy(ip)
   az=zz(ip)
   ip2=ip+4 !nod sg  
   rsdr0=rsdr(ca(i,2))
   
   dx=xx(ip2)-ax
   dy=yy(ip2)-ay
   dz=zz(ip2)-az    
   ix0=1
   iy0=1
   iz0=1 !!initial bias direction
   if(dx<0)ix0=-1
   if(dy<0)iy0=-1
   if(dz<0)iz0=-1
   ichange=0  !!1 for change
   iax=Nint(ax*rmesh)
   iay=Nint(ay*rmesh)
   iaz=Nint(az*rmesh)
   margin=1
   if(rsdr0>2)margin=2
   ep0=0
   do ix=-margin,margin
      do iy=-margin,margin
         do iz=-margin,margin
            ip3=site(iax+ix,iay+iy,iaz+iz)
            if(ip3<1)cycle  !!use original direction
            if(ip3.eq.ip2)then
               if(SITT(ip2,1)<1)cycle  !!no clash, else need move
            endif  !!if ip.ne.i, also need move
            do j=0,NSITT
               if(j.eq.0)then
                  id=ip3
               else
                  if(SITT(ip3,j)>0)id=SITT(ip3,j)
               endif                  
               if(ress(id).ne.i)then
                  ires2=ress(id)
                  if(ca(ires2,2)<3)cycle
                  ipd=ca(ires2,1)+4
                  ep0=ep0+sdhro(ca(i,2))*sdhro(ca(ires2,2))  !!hydro interaction
                  r=sqrt((xx(ip2)-xx(ipd))**2+(yy(ip2)-yy(ipd))**2+(zz(ip2)-zz(ipd))**2)
                  if(r<rsdr(ca(ires2,2))+rsdr0)then
                     ep0=ep0-10.0   !!atom clash  !!!need to train the weight
                  endif
               endif
            enddo
         enddo
      enddo
   enddo  !!for current position
   eep0=ep0
   !!the Ca the center to search neighboring positions
   do ix2=-margin,margin
      do iy2=-margin,margin
         do iz2=-margin,margin
            if(ix2==0.and.iy2==0.and.iz2==0.or.ix2==ix0.and.iy2==iy0.and.iz2==iz0)cycle
            
            if((ix2*dx+iy2*dy+iz2*dz)/sqrt(1.0*ix2**2+iy2**2+iz2**2)<2.6)cycle !2.82)cycle !!!correct direction, Ca->CB (CG) <20o
            ip3=site(iax+ix2,iay+iy2,iaz+iz2)
            if(ip3>0.and.ress(ip3).ne.i)cycle !!has clash point
            ibx=iax+ix2
            iby=iay+iy2
            ibz=iaz+iz2
            ep=0
            do ix=-margin,margin
               do iy=-margin,margin
                  do iz=-margin,margin
                     ip3=site(ibx+ix,iby+iy,ibz+iz)
                     if(ip3<1)cycle  !!use original direction
                     if(ip3.eq.ip2)then
                        if(SITT(ip2,1)<1)cycle  !!no clash, else need move
                     endif  !!if ip.ne.i, also need move
                     do j=0,NSITT
                        if(j.eq.0)then
                           id=ip3
                        else
                           if(SITT(ip3,j)>0)id=SITT(ip3,j)
                        endif
                        if(ress(id).ne.i)then
                           ires2=ress(id)
                           if(ca(ires2,2)<3)cycle
                           ipd=ca(ires2,1)+4
                           ep=ep+0.5*sdhro(ca(i,2))*sdhro(ca(ires2,2))  !!hydro interaction
                           r=sqrt((xx(ip2)-xx(ipd))**2+(yy(ip2)-yy(ipd))**2+(zz(ip2)-zz(ipd))**2)
                           if(r<rsdr(ca(ires2,2))+rsdr0)then
                              ep=ep-5.0   !!atom clash  !!!need to train the weight
                           endif
                        endif
                     enddo
                  enddo
               enddo
            enddo  !!for current position
            ep=ep-sqrt(1.0*(ix2-ix0)**2+(iy2-iy0)**2+(iz2-iz0)**2)  !!the wight??, three part, hydro/clash/distance
            if(ep>ep0)then
               ep0=ep
               ixn=ix
               iyn=iy
               izn=iz
               ichange=1
            endif
         enddo
      enddo
   enddo
   if(ichange>0)then  !!need to change Cg direction
!!!new bias ixn,iyn,izn
      bd=abs(ixn)+abs(iyn)+abs(izn)
      rf=rsdr0/bd   
       
      xx(ip2)=ax+ixn*rf
      yy(ip2)=ay+iyn*rf
      zz(ip2)=az+izn*rf
!write(*,"A10,I4,A5,6f8.3,2f9.2"),"ADJUST SG",i,resd(ip2),xx(ip2),yy(ip2),zz(ip2),sgx(i),sgy(i),sgz(i),eep0,ep0
      sgx(i)=xx(ip2)
      sgy(i)=yy(ip2)
      sgz(i)=zz(ip2)
   endif
enddo
!!build sidechain accord to SG center
do i=2,Nres-1
!!!sidechain center include CB
   ip=ca(i,1)
   ip2=ip+3
   iresin=ca(i,2)
   bd=bond(ip2+1,2)  !!cg-Cg
   xi=xx(ip2)-xx(ip)
   yi=yy(ip2)-yy(ip)
   zi=zz(ip2)-zz(ip)
   ax=xx(ip2)
   ay=yy(ip2)
   az=zz(ip2)
   if(iresin.eq.3.or.iresin.eq.4)then  !!SER and CYS, only need to adjust bond length and angle
      x=sgx(i)-ax
      y=sgy(i)-ay
      z=sgz(i)-az
      r1=sqrt(x*x+y*y+z*z)
      if(r1<0.1)r1=0.1
      fbd=bd/r1
      if(x*xi+y*yi+z*zi<=0)then
         x=-x
         y=-y
         z=-z
      endif
      xx(ip2+1)=0.5*(xx(ip2)+fbd*x+xx(ip2+1))
      yy(ip2+1)=0.5*(yy(ip2)+fbd*y+yy(ip2+1))
      zz(ip2+1)=0.5*(zz(ip2)+fbd*z+zz(ip2+1))
      if(CAangle(i)<-0.34)then !>120o
         r1=(xx(ip2+1)-xx(ip-1))**2+(yy(ip2+1)-yy(ip-1))**2+(zz(ip2+1)-zz(ip-1))**2  !!CG-N
         r2=(xx(ip2+1)-xx(ip+1))**2+(yy(ip2+1)-yy(ip+1))**2+(zz(ip2+1)-zz(ip+1))**2  !!CG-O
         if(r1<r2+1.7)then !1need adjust            
            angg=0.786
            x=xx(ip2+1)
            y=yy(ip2+1)
            z=zz(ip2+1)
            call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !125o
            xx(ip2+1)=x
            yy(ip2+1)=y
            zz(ip2+1)=z
         endif
      elseif(CAangle(i)>0)then !1<90o
            angg=-0.5*pi
            x=xx(ip2+1)
            y=yy(ip2+1)
            z=zz(ip2+1)
            call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !125o
            xx(ip2+1)=x
            yy(ip2+1)=y
            zz(ip2+1)=z     
      endif
   elseif(iresin.eq.5.or.iresin.eq.6.or.iresin.eq.7)then !VAL and THR 
 !  elseif(iresin>=5.and.iresin.ne.8)then !VAL and THR 
      if(CAangle(i)<-0.2588)then
         angg=pi/3.1 
      else
         angg=pi/10.0
      endif
      x=sgx(i)
      y=sgy(i)
      z=sgz(i)
      call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !125/2
      r1=sqrt((x-ax)*(x-ax)+(y-ay)*(y-ay)+(z-az)*(z-az))
      if(r1<0.1)r1=0.1
      fbd=bd/r1
      xx(ip2+1)=0.5*(ax+fbd*(x-ax)+xx(ip2+1))
      yy(ip2+1)=0.5*(ay+fbd*(y-ay)+yy(ip2+1))
      zz(ip2+1)=0.5*(az+fbd*(z-az)+zz(ip2+1))
      x=sgx(i)
      y=sgy(i)
      z=sgz(i)
      bd=bond(ip2+2,2)
      angg=-pi/3.0 !/6.0
      call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !125o/2
      !      write(*,"A5,I4,A5,9f8.3,f6.3"),"rot2",ip2+1,atomty(ip2+2),x,y,z,ax,ay,az,xi,yi,zi,-angg
      r1=sqrt((x-ax)*(x-ax)+(y-ay)*(y-ay)+(z-az)*(z-az))
      if(r1<0.1)r1=0.1
      fbd=bd/r1
      xx(ip2+2)=0.5*(ax+fbd*(x-ax)+xx(ip2+2))
      yy(ip2+2)=0.5*(ay+fbd*(y-ay)+yy(ip2+2))
      zz(ip2+2)=0.5*(az+fbd*(z-az)+zz(ip2+2))
      x=xx(ip2+2)-xx(ip2+1)
      y=yy(ip2+2)-yy(ip2+1)
      z=zz(ip2+2)-zz(ip2+1)
      r1=sqrt(x*x+y*y+z*z)
      if(r1<2.4)then
         if(r1<0.1)r1=0.1
         fbd=0.5*(2.4-r1)/r1
         xx(ip2+1)=xx(ip2+1)-x*fbd
         yy(ip2+1)=yy(ip2+1)-y*fbd
         zz(ip2+1)=zz(ip2+1)-z*fbd
         xx(ip2+2)=xx(ip2+2)+x*fbd
         yy(ip2+2)=yy(ip2+2)+y*fbd
         zz(ip2+2)=zz(ip2+2)+z*fbd
         !          print*,i,resd(ip2),r1,"Adjust",atomty(ip2+1)     
      endif
      if(iresin.eq.7)then
         xx(ip2+3)=xx(ip2+2)+xi
         yy(ip2+3)=yy(ip2+2)+yi
         zz(ip2+3)=zz(ip2+2)+zi
      endif
   elseif(iresin.eq.8)then !PRO
!print*,"ADDPRO",ress(ip),resd(ip),atomty(ip2)
      vx1=(xx(ca(i-1,1))+xx(ca(i+1,1)))/2.0-xx(ip)
      vy1=(yy(ca(i-1,1))+yy(ca(i+1,1)))/2.0-yy(ip)
      vz1=(zz(ca(i-1,1))+zz(ca(i+1,1)))/2.0-zz(ip)
      ax=0.6*xx(ip)+0.4*xx(ip-1) !N
      ay=0.6*yy(ip)+0.4*yy(ip-1)
      az=0.6*zz(ip)+0.4*zz(ip-1)
      x=sgx(i)-ax+0.22*vx1
      y=sgy(i)-ay+0.22*vy1
      z=sgz(i)-az+0.22*vy2   
      r1=sqrt(x*x+y*y+z*z)
      if(r1<0.1)r1=0.1
      fbd=2.3/r1 !2.278/r1
      xx(ip2+1)=ax+x*fbd
      yy(ip2+1)=ay+y*fbd
      zz(ip2+1)=az+z*fbd
      ax=3.0*sgx(i)-(xx(ip2)+xx(ip2+1))+0.15*vx1
      ay=3.0*sgy(i)-(yy(ip2)+yy(ip2+1))+0.15*vy1
      az=3.0*sgz(i)-(zz(ip2)+zz(ip2+1))+0.15*vz1 
      x=ax-xx(ip-1)
      y=ay-yy(ip-1)
      z=az-zz(ip-1)     
      r1=sqrt(x*x+y*y+z*z)
      if(r1<0.1)r1=0.1
      fbd=1.485/r1
      xx(ip2+2)=xx(ip-1)+x*fbd
      yy(ip2+2)=yy(ip-1)+y*fbd
      zz(ip2+2)=zz(ip-1)+z*fbd
   elseif(iresin.eq.10.or.iresin.eq.11)then !!ASN, ASP
      if(CAangle(i)<-0.2588)then
         angg=pi/3.1
!print*,">105",I,acos(CAangle(i))/hpi
      else
         angg=pi/4.7 !4.5
!print*,"<105",I,acos(CAangle(i))/hpi
      endif
      x=sgx(i)
      y=sgy(i)
      z=sgz(i)
      call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !125/2
      r1=sqrt((x-xx(ip))**2+(y-yy(ip))**2+(z-zz(ip))**2)
      if(r1<0.1)r1=0.1
      fbd=2.53/r1
      xx(ip2+1)=0.5*(xx(ip)+fbd*(x-xx(ip))+xx(ip2+1))
      yy(ip2+1)=0.5*(yy(ip)+fbd*(y-yy(ip))+yy(ip2+1))
      zz(ip2+1)=0.5*(zz(ip)+fbd*(z-zz(ip))+zz(ip2+1))
      x=xx(ip2+1)-ax
      y=yy(ip2+1)-ay
      z=zz(ip2+1)-az
      r1=sqrt(x*x+y*y+z*z)
      if(r1<0.1)r1=0.1
      fbd=bd/r1
      xx(ip2+1)=ax+x*fbd
      yy(ip2+1)=ay+y*fbd
      zz(ip2+1)=az+z*fbd  
      fbd=bond(ip2+2,2)/bond(ip2,2)
      xx(ip2+2)=xx(ip2+1)+fbd*xi
      yy(ip2+2)=yy(ip2+1)+fbd*yi
      zz(ip2+2)=zz(ip2+1)+fbd*zi
      x=0.5*(xx(ip2+2)+ax)
      y=0.5*(yy(ip2+2)+ay)
      z=0.5*(zz(ip2+2)+az)      
      r=sqrt((xx(ip2+1)-x)**2+(yy(ip2+1)-y)**2+(zz(ip2+1)-z)**2)
      if(r<0.1)r=0.1
      fbd=bond(i+3,2)/r
      xx(ip2+3)=xx(ip2+1)+(xx(ip2+1)-x)*fbd
      yy(ip2+3)=yy(ip2+1)+(yy(ip2+1)-y)*fbd
      zz(ip2+3)=zz(ip2+1)+(zz(ip2+1)-z)*fbd
   else   !!other long side chain
      if(CAangle(i)<-0.2588)then
         angg=-pi/30.0 
      else
         angg=pi/5.5
      endif
      x=sgx(i)
      y=sgy(i)
      z=sgz(i)
      call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !125/2
      r1=sqrt((x-xx(ip))**2+(y-yy(ip))**2+(z-zz(ip))**2)
      if(r1<0.1)r1=0.1
      fbd=2.53/r1
      xx(ip2+1)=0.5*(xx(ip)+fbd*(x-xx(ip))+xx(ip2+1))
      yy(ip2+1)=0.5*(yy(ip)+fbd*(y-yy(ip))+yy(ip2+1))
      zz(ip2+1)=0.5*(zz(ip)+fbd*(z-zz(ip))+zz(ip2+1))
      x=xx(ip2+1)-ax
      y=yy(ip2+1)-ay
      z=zz(ip2+1)-az
      r1=sqrt(x*x+y*y+z*z)
      if(r1<0.1)r1=0.1
      fbd=bd/r1
      xx(ip2+1)=ax+x*fbd
      yy(ip2+1)=ay+y*fbd
      zz(ip2+1)=az+z*fbd  
   endif
enddo
76 continue
end subroutine SG_ADD

!!this subroutine to add those misssing heavy atoms
!!except CB, CG and CD
subroutine HEAVY_ADD
implicit none
!scan heavy atoms at first
do i=1,natom
   if(aty(i).eq.'H'.or.xx(i)<990)cycle
        ASSrd(i)=1
!if(atomty(i).eq." N  ")print*,"one N missing",i,ress(i),resd(i),xx(i)
   do k=1,bonda(i,5)
      if(xx(bonda(i,k))>990)cycle 
      icc=bonda(i,k)
      bd=bond(i,2*k)
      goto 51
   enddo
51 continue
      ax=xx(icc)  !!origin point
      ay=yy(icc)
      az=zz(icc)
!write(*,"A10,I5,A4,A4,I4,3f8.3"),"readd heavy",i,atomty(i),resd(i),ress(i),ax,ay,az
      ires=ress(i)
      kx0=0
      do j=1,bonda(icc,5) !!the connect heavy atom  !!ipx-ip2-icc-H (1,2,3)
         ii=bonda(icc,j)
         if(xx(ii)>990)cycle
         kx0=kx0+1
         ipp0(kx0)=bonda(icc,j)
      enddo    
         ip2=ipp0(1)
!!!get the angle ip2-icc-i
      do ii=Lang*(ires-1)+1,ires*Lang
         if(anga(ii,1)<1)cycle
         if(anga(ii,2)==icc.and.(anga(ii,1)==ip2.and.anga(ii,3)==i).or.(anga(ii,1)==i.and.anga(ii,3)==ip2))then
            angg=ang(ii,2)*hpi
            goto 20
         endif
      enddo
20    continue 
      kx=0
      do j=1,bonda(ip2,5)
         if(bonda(ip2,j)==icc.or.ATY(bonda(ip2,j))=='H'.or.xx(bonda(ip2,j))>990)cycle
         kx=kx+1
         ipp(kx)=bonda(ip2,j)
      enddo
      ip3=ipp(1)
!treat heavy atoms missing !ip3-ip2-icc-i (heavy atom)
!!C-term ALA-Cb; ILE Cd; LEU Cd1,Cd2;MET Ce; Val CG1; THR CG2  sp3
!!O-term O for all, ASN OD1; ASP OD1, OD2; GLN OE1; GLU OE1,OE2; TYR OH;sp2 || SER OG; THR OG1, OXT (sp3) 
!!S-term CYS SG sp3
!!N-term N-term LYS NZ sp3, ARG NH1,NH2; ASN ND2; GLN NE2 sp2
      abc=atomty(i)  !for sp2 second atom      
  if(bonda(i,6).eq.1)then 
     if(abc.eq.' OD2'.or.abc.eq.' OE2'.or.abc.eq.' ND2'.or.abc.eq.' NE2'.or.abc.eq.' NH2'.or.abc.eq.' OH '.or.abc.eq.' OXT')then
        if(abc.eq.' OH ')then
           ip2=icc-1
           ip3=icc-2
        else !if(abc.eq." OXT".or.abc.eq." NH2")then
           ip2=icc-1
           ip3=icc+1
        endif
        x=0.5*(xx(ip2)+xx(ip3))
        y=0.5*(yy(ip2)+yy(ip3))
        z=0.5*(zz(ip2)+zz(ip3))
        r1=sqrt((x-ax)*(x-ax)+(y-ay)*(y-ay)+(z-az)*(z-az))
        if(r1>dismin2)then
           fbd=bd/r1
           xx(i)=ax+fbd*(ax-x)
           yy(i)=ay+fbd*(ay-y)
           zz(i)=az+fbd*(az-z)
        else
           ASSrd(i)=2           
      !     print*,"warn!ADD Cterm1",i,atomty(i),ip3,ip2,icc
           ip3=ipp(1)
           xx(i)=ax+xx(ip2)-xx(ip3)
           yy(i)=ay+yy(ip2)-yy(ip3)
           zz(i)=az+zz(ip2)-zz(ip3)
        endif
     elseif(abc.eq.' CD2'.and.resd(i).eq.'LEU')then
        xi=ax-xx(icc-1)
        yi=ay-yy(icc-1)
        zi=az-zz(icc-1)
        x=xx(i-1) !CD1
        y=yy(i-1)
        z=zz(i-1)
        call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,2.18166)  !125o
        xx(i)=x
        yy(i)=y
        zz(i)=z  
     elseif(abc.eq.' CG2')then !VAL, THR, ILE
        xi=ax-xx(icc-3)
        yi=ay-yy(icc-3)
        zi=az-zz(icc-3)
        x=xx(i-1) !CD1
        y=yy(i-1)
        z=zz(i-1)
        call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,2.18166)  !125o
        xx(i)=x
        yy(i)=y
        zz(i)=z  
!        print*,"CG2",atomty(i),xx(i),yy(i),zz(i) 
     elseif((abc.eq.' CD1'.and.resd(i).eq."ILE").or.(abc.eq.' CE '.and.resd(i).eq."MET"))then !ILE
        xi=xx(icc-1)-0.5*(xx(i-1)+xx(icc-4))
        yi=yy(icc-1)-0.5*(yy(i-1)+yy(icc-4))
        zi=zz(icc-1)-0.5*(zz(i-1)+zz(icc-4))
        x=xi-ax
        y=yi-ay
        z=zi-az
        r1=sqrt(x*x+y*y+z*z)
        if(r1<0.1)r1=0.1
        fbd=bd/r1
        xx(i)=ax+fbd*x
        yy(i)=ay+fbd*y
        zz(i)=az+fbd*z      
     else  !!sp3 and sp2 first atom  ?????????
        if(abc.eq.' CG1'.or.abc.eq.' SG '.or.abc.eq.' OG1'.or.abc.eq.' OG ')ip3=ip2+1
!     print*,"HADD>=",ress(i),atomty(ip3),atomty(ip2),atomty(i),resd(i),abc
        x=xx(ip2)-xx(ip3)
        y=yy(ip2)-yy(ip3)
        z=zz(ip2)-zz(ip3)
        r1=sqrt(x*x+y*y+z*z)
        if(r1>dismin2)then
           fbd=bd/r1
           xx(i)=ax+fbd*x
           yy(i)=ay+fbd*y
           zz(i)=az+fbd*z
        else
           ASSrd(i)=2
        !   print*,"warn!ADD Cterm2",i,atomty(i),ip3,ip2,icc
           xx(i)=2.0*ax-xx(ip2)
           yy(i)=2.0*ay-yy(ip2)
           zz(i)=2.0*az-zz(ip2)
        endif   
     endif
  elseif(bonda(i,6)>=2)then  !!treat on more atoms missing
     if(abc.eq.' CG ')ip3=ip2+1
     x=xx(ip2)-xx(ip3)
     y=yy(ip2)-yy(ip3)
     z=zz(ip2)-zz(ip3)
     r1=sqrt(x*x+y*y+z*z)
     if(r1>dismin2)then
        fbd=bd/r1
        xx(i)=ax+fbd*x
        yy(i)=ay+fbd*y
        zz(i)=az+fbd*z
     else
        ASSrd(i)=2
     !   print*,"warn!ADD Cterm3",i,atomty(i),ip3,ip2,icc
        xx(i)=ax+x
        yy(i)=ay+y
        zz(i)=az+z
     endif  !!default set
if(.not.(ca(ress(i),2).eq.8.or.ca(ress(i),2)>=17))cycle

!print*,atomty(i),resd(i),ress(i),"<<<<ring??"
if(abc.eq." CG ")then
   if(resd(i).eq."PRO")then   !!this region treat the ring PRO CG-CD
      x=xx(i)-xx(ip2-1)
      y=yy(i)-yy(ip2-1)
      z=zz(i)-zz(ip2-1)
      r1=sqrt(x*x+y*y+z*z)
      if(r1<dismin2)r1=dismin2
      fbd=2.22/r1
      xx(i)=xx(ip2-1)+fbd*x  !CG
      yy(i)=yy(ip2-1)+fbd*y
      zz(i)=zz(ip2-1)+fbd*z
      x=0.5*(xx(ip2-1)+xx(i)-xx(ip2)-xx(i-1))
      y=0.5*(yy(ip2-1)+yy(i)-yy(ip2)-yy(i-1))
      z=0.5*(zz(ip2-1)+zz(i)-zz(ip2)-zz(i-1))
      xx(i+1)=0.5*(xx(ip2)+xx(i-1))+1.618*x
      yy(i+1)=0.5*(yy(ip2)+yy(i-1))+1.618*y
      zz(i+1)=0.5*(zz(ip2)+zz(i-1))+1.618*z
      ASSrd(i+1)=1
   else
      bx=xx(ip2+1)-xx(ip2-1)
      by=yy(ip2+1)-yy(ip2-1)
      bz=zz(ip2+1)-zz(ip2-1)
      cx=xx(i)-xx(i-1)
      cy=yy(i)-yy(i-1)
      cz=zz(i)-zz(i-1)
      ctbcx=by*cz-bz*cy      !cross product  bxc
      ctbcy=bz*cx-bx*cz
      ctbcz=bx*cy-by*cx
      r1=sqrt(ctbcx*ctbcx+ctbcy*ctbcy+ctbcz*ctbcz)
      if(r1<dismin2)r1=dismin2
   if(resd(i).eq."PHE".or.resd(i).eq."TYR")then !.or.resd(i).eq."TYR"))then
      xx(i+5)=xx(i)+1.788*cx
      yy(i+5)=yy(i)+1.788*cy
      zz(i+5)=zz(i)+1.788*cz
      xi=xx(i+5)-xx(i)
      yi=yy(i+5)-yy(i)
      zi=zz(i+5)-zz(i)
      fbd=1.191/r1
      xx(i+1)=xx(i)+0.25*xi+ctbcx*fbd
      yy(i+1)=yy(i)+0.25*yi+ctbcy*fbd
      zz(i+1)=zz(i)+0.25*zi+ctbcz*fbd
      xx(i+2)=xx(i)+0.25*xi-ctbcx*fbd
      yy(i+2)=yy(i)+0.25*yi-ctbcy*fbd
      zz(i+2)=zz(i)+0.25*zi-ctbcz*fbd
      xx(i+3)=xx(i)+0.75*xi+ctbcx*fbd
      yy(i+3)=yy(i)+0.75*yi+ctbcy*fbd
      zz(i+3)=zz(i)+0.75*zi+ctbcz*fbd
      xx(i+4)=xx(i)+0.75*xi-ctbcx*fbd
      yy(i+4)=yy(i)+0.75*yi-ctbcy*fbd
      zz(i+4)=zz(i)+0.75*zi-ctbcz*fbd
      if(resd(i).eq."TYR")then
         xx(i+6)=xx(i+5)+0.937*cx  !!OH
         yy(i+6)=yy(i+5)+0.937*cy
         zz(i+6)=zz(i+5)+0.937*cz
      endif
      ASSrd(i+1)=1
      ASSrd(i+2)=1
      ASSrd(i+3)=1
      ASSrd(i+4)=1
   elseif(resd(i).eq."TRP")then
      fbd=1.19/r1
      xx(i+1)=xx(i)+0.63*cx+fbd*ctbcx !CD1
      yy(i+1)=yy(i)+0.63*cy+fbd*ctbcy
      zz(i+1)=zz(i)+0.63*cz+fbd*ctbcz
      xx(i+2)=xx(i)+0.448*cx-fbd*ctbcx !!CD2
      yy(i+2)=yy(i)+0.448*cy-fbd*ctbcy
      zz(i+2)=zz(i)+0.448*cz-fbd*ctbcz
      fbd=0.688/r1
      xx(i+3)=xx(i+1)+0.819*cx-fbd*ctbcx  !!NE1
      yy(i+3)=yy(i+1)+0.819*cy-fbd*ctbcy
      zz(i+3)=zz(i+1)+0.819*cz-fbd*ctbcz
      xx(i+4)=xx(i+2)+0.774*cx+fbd*ctbcx !!CE2
      yy(i+4)=yy(i+2)+0.774*cy+fbd*ctbcy
      zz(i+4)=zz(i+2)+0.774*cz+fbd*ctbcz
      fbd=1.19/r1
      xx(i+5)=xx(i+2)-fbd*ctbcx-0.448*cx !CE3
      yy(i+5)=yy(i+2)-fbd*ctbcy-0.448*cy
      zz(i+5)=zz(i+2)-fbd*ctbcz-0.448*cz 
      xx(i+6)=xx(i+4)-fbd*ctbcx+0.448*cx !CZ2
      yy(i+6)=yy(i+4)-fbd*ctbcy+0.448*cy
      zz(i+6)=zz(i+4)-fbd*ctbcz+0.448*cz      
      xx(i+7)=xx(i+5)-fbd*ctbcx+0.448*cx !CZ3
      yy(i+7)=yy(i+5)-fbd*ctbcy+0.448*cy
      zz(i+7)=zz(i+5)-fbd*ctbcz+0.448*cz      
      xx(i+8)=xx(i+7)+xx(i+4)-xx(i+2)    !CH2
      yy(i+8)=yy(i+7)+yy(i+4)-yy(i+2)
      zz(i+8)=zz(i+7)+zz(i+4)-zz(i+2)  
      do j=1,8
         ASSrd(i+j)=1
      enddo
   elseif(resd(i).eq."HIS")then
      fbd=1.19/r1
      xx(i+1)=xx(i)+0.6*cx+fbd*ctbcx  !!ND1
      yy(i+1)=yy(i)+0.6*cy+fbd*ctbcy
      zz(i+1)=zz(i)+0.6*cz+fbd*ctbcz
      xx(i+2)=xx(i)+0.63*cx-fbd*ctbcx !!CD2
      yy(i+2)=yy(i)+0.63*cy-fbd*ctbcy
      zz(i+2)=zz(i)+0.63*cz-fbd*ctbcz
      fbd=0.688/r1
      xx(i+3)=xx(i)+1.35*cx+fbd*ctbcx  !!CE1
      yy(i+3)=yy(i)+1.35*cy+fbd*ctbcy
      zz(i+3)=zz(i)+1.35*cz+fbd*ctbcz
      xx(i+4)=xx(i)+1.41*cx-fbd*ctbcx !!NE2
      yy(i+4)=yy(i)+1.41*cy-fbd*ctbcy
      zz(i+4)=zz(i)+1.41*cz-fbd*ctbcz
      do j=1,4
         ASSrd(i+j)=1
      enddo
   endif
endif
elseif(abc.eq." CD ".or.abc.eq." CD1")then  
   if(resd(i).eq."PRO")then   !!this region treat the ring PRO CG-CD
      ip3=ca(ress(i),1)  !!Ca
      x=0.5*(xx(ip3-1)+xx(i-1)-xx(ip3)-xx(i-2))
      y=0.5*(yy(ip3-1)+yy(i-1)-yy(ip3)-yy(i-2))
      z=0.5*(zz(ip3-1)+zz(i-1)-zz(ip3)-zz(i-2))
      xx(i+1)=0.5*(xx(ip3)+xx(i-2))+1.618*x
      yy(i+1)=0.5*(yy(ip3)+yy(i-2))+1.618*y
      zz(i+1)=0.5*(zz(ip3)+zz(i-2))+1.618*z
      ASSrd(i)=1
   else
      bx=xx(ip3+1)-xx(ip3-1)
      by=yy(ip3+1)-yy(ip3-1)
      bz=zz(ip3+1)-zz(ip3-1)
      cx=xx(i-1)-xx(i-2)
      cy=yy(i-1)-yy(i-2)
      cz=zz(i-1)-zz(i-2)
      ctbcx=by*cz-bz*cy      !cross product  bxc
      ctbcy=bz*cx-bx*cz
      ctbcz=bx*cy-by*cx
      r1=sqrt(ctbcx*ctbcx+ctbcy*ctbcy+ctbcz*ctbcz)
      if(r1<dismin2)r1=dismin2
      if(resd(i).eq."PHE".or.resd(i).eq."TYR")then !!
         xx(i+4)=xx(i-1)+1.788*cx
         yy(i+4)=yy(i-1)+1.788*cy
         zz(i+4)=zz(i-1)+1.788*cz
         xi=xx(i+4)-xx(i-1)
         yi=yy(i+4)-yy(i-1)
         zi=zz(i+4)-zz(i-1)
         fbd=1.191/r1
         xx(i)=xx(i-1)+0.25*xi+ctbcx*fbd
         yy(i)=yy(i-1)+0.25*yi+ctbcy*fbd
         zz(i)=zz(i-1)+0.25*zi+ctbcz*fbd
         xx(i+1)=xx(i-1)+0.25*xi-ctbcx*fbd
         yy(i+1)=yy(i-1)+0.25*yi-ctbcy*fbd
         zz(i+1)=zz(i-1)+0.25*zi-ctbcz*fbd
         xx(i+2)=xx(i-1)+0.75*xi+ctbcx*fbd
         yy(i+2)=yy(i-1)+0.75*yi+ctbcy*fbd
         zz(i+2)=zz(i-1)+0.75*zi+ctbcz*fbd
         xx(i+3)=xx(i-1)+0.75*xi-ctbcx*fbd
         yy(i+3)=yy(i-1)+0.75*yi-ctbcy*fbd
         zz(i+3)=zz(i-1)+0.75*zi-ctbcz*fbd
         if(resd(i).eq."TYR")then
            xx(i+5)=xx(i+4)+0.937*cx  !!OH
            yy(i+5)=yy(i+4)+0.937*cy
            zz(i+5)=zz(i+4)+0.937*cz
         endif
         ASSrd(i)=1
         ASSrd(i+1)=1
         ASSrd(i+2)=1
         ASSrd(i+3)=1
      elseif(resd(i).eq."TRP")then
         fbd=1.19/r1
         xx(i)=xx(i-1)+0.63*cx+fbd*ctbcx !CD1
         yy(i)=yy(i-1)+0.63*cy+fbd*ctbcy
         zz(i)=zz(i-1)+0.63*cz+fbd*ctbcz
         xx(i+1)=xx(i-1)+0.448*cx-fbd*ctbcx !!CD2
         yy(i+1)=yy(i-1)+0.448*cy-fbd*ctbcy
         zz(i+1)=zz(i-1)+0.448*cz-fbd*ctbcz
         fbd=0.688/r1
         xx(i+2)=xx(i)+0.819*cx-fbd*ctbcx  !!NE1
         yy(i+2)=yy(i)+0.819*cy-fbd*ctbcy
         zz(i+2)=zz(i)+0.819*cz-fbd*ctbcz
         xx(i+3)=xx(i+1)+0.774*cx+fbd*ctbcx !!CE2
         yy(i+3)=yy(i+1)+0.774*cy+fbd*ctbcy
         zz(i+3)=zz(i+1)+0.774*cz+fbd*ctbcz
         fbd=1.19/r1
         xx(i+4)=xx(i+1)-fbd*ctbcx-0.448*cx !CE3
         yy(i+4)=yy(i+1)-fbd*ctbcy-0.448*cy
         zz(i+4)=zz(i+1)-fbd*ctbcz-0.448*cz 
         xx(i+5)=xx(i+3)-fbd*ctbcx+0.448*cx !CZ2
         yy(i+5)=yy(i+3)-fbd*ctbcy+0.448*cy
         zz(i+5)=zz(i+3)-fbd*ctbcz+0.448*cz      
         xx(i+6)=xx(i+4)-fbd*ctbcx+0.448*cx !CZ3
         yy(i+6)=yy(i+4)-fbd*ctbcy+0.448*cy
         zz(i+6)=zz(i+4)-fbd*ctbcz+0.448*cz      
         xx(i+7)=xx(i+6)+xx(i+3)-xx(i+1)    !CH2
         yy(i+7)=yy(i+6)+yy(i+3)-yy(i+1)
         zz(i+7)=zz(i+6)+zz(i+3)-zz(i+1)  
         do j=1,7
            ASSrd(i+j)=1
         enddo
      elseif(resd(i).eq."HIS")then
         fbd=1.19/r1
         xx(i)=xx(i-1)+0.6*cx+fbd*ctbcx  !!ND1
         yy(i)=yy(i-1)+0.6*cy+fbd*ctbcy
         zz(i)=zz(i-1)+0.6*cz+fbd*ctbcz
         xx(i+1)=xx(i-1)+0.63*cx-fbd*ctbcx !!CD2
         yy(i+1)=yy(i-1)+0.63*cy-fbd*ctbcy
         zz(i+1)=zz(i-1)+0.63*cz-fbd*ctbcz
         fbd=0.688/r1
         xx(i+2)=xx(i-1)+1.35*cx+fbd*ctbcx  !!CE1
         yy(i+2)=yy(i-1)+1.35*cy+fbd*ctbcy
         zz(i+2)=zz(i-1)+1.35*cz+fbd*ctbcz
         xx(i+3)=xx(i-1)+1.41*cx-fbd*ctbcx !!NE2
         yy(i+3)=yy(i-1)+1.41*cy-fbd*ctbcy
         zz(i+3)=zz(i-1)+1.41*cz-fbd*ctbcz
         do j=1,3
            ASSrd(i+j)=1
         enddo
      else
         x=0.5*(xx(ip2)+xx(ip3))
         y=0.5*(yy(ip2)+yy(ip3))
         z=0.5*(zz(ip2)+zz(ip3))
         r1=sqrt((x-ax)*(x-ax)+(y-ay)*(y-ay)+(z-az)*(z-az))
         if(r1>dismin2)then
            fbd=bd/r1
            xx(i)=ax+fbd*(ax-x)
            yy(i)=ay+fbd*(ay-y)
            zz(i)=az+fbd*(az-z)
         else
            ASSrd(i)=2           
            ip3=ipp(1)
            xx(i)=ax+xx(ip2)-xx(ip3)
            yy(i)=ay+yy(ip2)-yy(ip3)
            zz(i)=az+zz(ip2)-zz(ip3)
         endif
      endif
   endif  !!ring finish
endif
end if
!print*,i,atomty(i),resd(i),xx(i),yy(i),zz(i)
!if(xx(i)>990.0)print*,i,atomty(i),resd(i),xx(i)
enddo  !!heavy atom scan finish
!print*,"HEAVY add finish"
end subroutine HEAVY_ADD

!!this subroutine add hydrogen atoms
subroutine HADD
implicit none

!call HEAVY_ADD
i=natom+1
do while(i>2)
   i=i-1
   if(xx(i)<990.or.aty(i)/='H')cycle
      icc=bonda(i,1)  !!bonded heavy atom
      bd=bond(i,2)
      ax=xx(icc)  !!origin point
      ay=yy(icc)
      az=zz(icc)
      ires=ress2(i)
      kx0=0
 !     bd=bd*0.864
      do j=1,bonda(icc,5) !!the connect heavy atom  !!ipx-ip2-icc-H (1,2,3)
         ii=bonda(icc,j)
        if(xx(ii)>990)cycle
        ip2=bonda(icc,j)
          kx0=kx0+1
         ipp0(kx0)=ip2
      enddo    
      ip2=ipp0(1)

      ASSrd(i)=1
!!!get the angle ip2-icc-i
      do ii=Lang*(ires-1)+1,ires*Lang
         if(anga(ii,1)<1)cycle
         if(anga(ii,2)==icc.and.(anga(ii,1)==ip2.and.anga(ii,3)==i).or.(anga(ii,1)==i.and.anga(ii,3)==ip2))then
            angg=ang(ii,2)*hpi
            goto 21
         endif
      enddo
21    continue 
      kx=0
      do j=1,bonda(ip2,5)
         if(bonda(ip2,j)==icc.or.ATY(bonda(ip2,j))=='H'.or.xx(bonda(ip2,j))>990)cycle
         ip3=bonda(ip2,j)
         kx=kx+1
         ipp(kx)=ip3
      enddo
      vr0(1)=xx(ip2)-ax
      vr0(2)=yy(ip2)-ay
      vr0(3)=zz(ip2)-az
!!!rotation axis , cross of vr0 and vr1, first H is transto ip2 
if(atom(i,1)(1:2)=='3H')then  !!term     sp3-sp3  trans-, Gauche-, Gauche+
   if(aty(icc).eq."C")bd=bd*0.95 !1.05
            ASSrd(i-1)=1
            ASSrd(i-2)=1
         do k=1,kx
            vr1(k,1)=xx(ipp(k))-xx(ip2)   !!connect heavy atoms vectors
            vr1(k,2)=yy(ipp(k))-yy(ip2)   !!connect heavy atoms vectors
            vr1(k,3)=zz(ipp(k))-zz(ip2)   !!connect heavy atoms vectors
         enddo
         x=xx(ip2)
         y=yy(ip2)
         z=zz(ip2)
         xi=vr1(1,2)*vr0(3)-vr1(1,3)*vr0(2)
         yi=vr1(1,3)*vr0(1)-vr1(1,1)*vr0(3)
         zi=vr1(1,1)*vr0(2)-vr1(1,2)*vr0(1)
!print*,"ccc",x,y,z,xi,yi,zi,ax,ay,az,angg
         call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !H on N, each H is trans    
         r1=sqrt((x-ax)**2+(y-ay)**2+(z-az)**2)
         if(r1<0.01.or.r1>5.0)then
          !  print*, "warning CH3-NH3-1",r1,bd,ress2(i),i,atom(i,1)
            r1=sqrt((xx(ip3)-xx(ip2))**2+(yy(ip3)-yy(ip2))**2+(zz(ip3)-zz(ip2))**2)
            if(r1>10.0.or.r1<0.001)then
!               print*,"neighbour position severa wrong CH3-NH3, again"
               xx(i)=ax+(xx(ip2)-xx(ip3))/bond(ip2,2)
               yy(i)=ay+(yy(ip2)-yy(ip3))/bond(ip2,2)
               zz(i)=az+(zz(ip2)-zz(ip3))/bond(ip2,2) 
               ASSrd(i)=2
            else
               fbd=bd/r1
               xx(i)=ax+(xx(ip2)-xx(ip3))*fbd
               yy(i)=ay+(yy(ip2)-yy(ip3))*fbd
               zz(i)=az+(zz(ip2)-zz(ip3))*fbd
            endif
         else
!print*,"ddd",x,y,z,xi,yi,zi,ax,ay,az,r1,bd,angg
            fbd=bd/r1  !!adjust bond length
            xx(i)=ax+fbd*(x-ax)
            yy(i)=ay+fbd*(y-ay)
            zz(i)=az+fbd*(z-az)
         endif
         angg=2.0*pi/3.0
            x=xx(i)
            y=yy(i)
            z=zz(i)
            xi=vr0(1)
            yi=vr0(2)
            zi=vr0(3)
            call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !H on N, each H is trans    
            r1=sqrt((x-ax)**2+(y-ay)**2+(z-az)**2)
            if(r1<0.01.or.r1>100.0)then
            !   print*, "warning CH3,NH3-2",r1,bd,ress2(i),i,atom(i,1)           
               xx(i-1)=ax+cos(109.5/180*pi)*(xx(i)-ax)
               yy(i-1)=ay+sin(109.5/180*pi)*(yy(i)-ay)
               zz(i-1)=az 
               ASSrd(i-1)=2
            else
               fbd=bd/r1  !!adjust bond length
               xx(i-1)=ax+fbd*(x-ax)
               yy(i-1)=ay+fbd*(y-ay)
               zz(i-1)=az+fbd*(z-az)  
            endif
            x=xx(i)
            y=yy(i)
            z=zz(i)
            call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,-angg)  !H on N, each H is trans  
            r1=sqrt((x-ax)**2+(y-ay)**2+(z-az)**2)
            if(r1<0.01.or.r1>10.0)then
!               print*, "warning CH3,NH3-2",r1,bd,ress2(i),i,atom(i,1)           
               xx(i-2)=ax+cos(109.5/180*pi)*(xx(i)-ax)
               yy(i-2)=ay+sin(-109.5/180*pi)*(yy(i)-ay)
               zz(i-2)=az 
               ASSrd(i-1)=2
            else
               fbd=bd/r1  !!adjust bond length
               xx(i-2)=ax+fbd*(x-ax)
               yy(i-2)=ay+fbd*(y-ay)
               zz(i-2)=az+fbd*(z-az)   
            endif
            i=i-2
!!!!sp3 -term finish ****************************************************************
      elseif(atom(i,1)(1:2)=='2H')then  !!in term or middle
         if(aty(icc).eq."C")bd=bd*0.95 !1.05
         ASSrd(i-1)=1
         if(ATY(icc)=='N')then   !!in term, sp2   
 !           if(kx==2)then    !!sp2-sp2 term
               if(ATY(ipp(1)).eq.'C')then   !!N=O -C
                  ip3=ipp(2)
               else
                  ip3=ipp(1)
               endif  !!the O
            !   if(resd(i).eq."ARG")then  !!N-C-N-H
            !      angg=1.02*angg
            !   else  !!O=C-N-H
            !      angg=0.98*angg
            !   endif
               vr1(1,1)=xx(ip3)-xx(ip2)
               vr1(1,2)=yy(ip3)-yy(ip2)
               vr1(1,3)=zz(ip3)-zz(ip2)
               x=xx(ip2)
               y=yy(ip2)
               z=zz(ip2)
               xi=vr1(1,2)*vr0(3)-vr1(1,3)*vr0(2)
               yi=vr1(1,3)*vr0(1)-vr1(1,1)*vr0(3)
               zi=vr1(1,1)*vr0(2)-vr1(1,2)*vr0(1)
               call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !H on N, each H is trans
               r1=sqrt((x-ax)**2+(y-ay)**2+(z-az)**2)
               if(r1<0.01.or.r1>5.0)then
                  ASSrd(i)=2
!                  print*, "warning NH2-1",r1,bd,ress2(i),i,atom(i,1)
                  r1=sqrt((xx(ip3)-xx(ip2))**2+(yy(ip3)-yy(ip2))**2+(zz(ip3)-zz(ip2))**2)
                  if(r1>10.0.or.r1<0.001)then
!                     print*,"neighbour bond position severa wrong NH2 again"
                     xx(i)=ax+(xx(ip2)-xx(ip3))*0.6
                     yy(i)=ay+(yy(ip2)-yy(ip3))*0.6
                     zz(i)=az+(zz(ip2)-zz(ip3))*0.6  
                  else
                     fbd=bd/r1
                     xx(i)=ax+(xx(ip2)-xx(ip3))*fbd
                     yy(i)=ay+(yy(ip2)-yy(ip3))*fbd
                     zz(i)=az+(zz(ip2)-zz(ip3))*fbd       
                  endif
               else
                  fbd=bd/r1  !!adjust bond length
                  xx(i)=ax+fbd*(x-ax)
                  yy(i)=ay+fbd*(y-ay)
                  zz(i)=az+fbd*(z-az)  
               endif
               x=xx(ip2)
               y=yy(ip2)
               z=zz(ip2)
               call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,-angg)  !H on N, each H is trans              
               r1=sqrt((x-ax)**2+(y-ay)**2+(z-az)**2)
               if(r1<0.01.or.r1>5.0)then
!                  print*, "warning NH2-2",r1,bd,ress2(i),i,atom(i,1)
                  r1=sqrt((xx(ip3)-xx(ip2))**2+(yy(ip3)-yy(ip2))**2+(zz(ip3)-zz(ip2))**2)
                  if(r1>5.0.or.r1<0.001)then
                     xx(i-1)=ax+(xx(ip2)-xx(ip3))*0.6
                     yy(i-1)=ay+(yy(ip2)-yy(ip3))*0.6
                     zz(i-1)=az+(zz(ip2)-zz(ip3))*0.6 
                     ASSrd(i-1)=2
                     print*,"severa wrong NH2-2",i-1,xx(i-1),yy(i-1)
                  else
                     fbd=bd/r1
                     xx(i-1)=ax+(xx(ip3)-xx(ip2))*fbd
                     yy(i-1)=ay+(yy(ip3)-yy(ip2))*fbd
                     zz(i-1)=az+(zz(ip3)-zz(ip2))*fbd  
!                     print*,">>>2000",i-1,fbd,bd,r1,xx(i-1),yy(i-1)
                  endif
               else
                  fbd=bd/r1  !!adjust bond length
                  xx(i-1)=ax+fbd*(x-ax)
                  yy(i-1)=ay+fbd*(y-ay)
                  zz(i-1)=az+fbd*(z-az)
               endif
            !if(atom(i,3)=='PRO'.and.ress2(i)==1)then   !!PRO N-term
            if(resd(i)=='PRO'.and.ress2(i)==1)then   !!PRO N-term
               xi=vr0(1)
               yi=vr0(2)
               zi=vr0(3)
               do k=-1,0
                  x=xx(i+k)
                  y=yy(i+k)
                  z=zz(i+k)
                  call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,0.5*pi)  !H on N, each H is trans  
                  xx(i+k)=x
                  yy(i+k)=y
                  zz(i+k)=z
               enddo
            endif       
         else    !!!C inter connected, not in term, rotate first H on of ip2 around icc-ip4
            !!the connect heavy atom  !!ip2-icc-ip4 -H (1,2,3)
         !   if(ipp0(1)==ip2)then
            if(ipp0(2)==ip2)then
               ip4=ipp0(1)
               if(abs(icc-ip4)>20)print*,"ip4 wrong!!!"
            else
               ip4=ipp0(2)
            endif
            angg=1.1*angg  !!!120/110 degree
         do ii=1,2
            if(ii==2)then
               i2=ip2
               ip2=ip4
               ip4=i2
               angg=-angg
            endif               
            x=xx(ip2)
            y=yy(ip2)
            z=zz(ip2)
            xi=xx(ip4)-ax
            yi=yy(ip4)-ay
            zi=zz(ip4)-az
            call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !H on C, each H is trans   
            r1=sqrt((x-ax)**2+(y-ay)**2+(z-az)**2)
            if(r1<0.01.or.r1>5.0)then
!               print*, "warning CH2-1",r1,bd,ress2(i),i,atom(i,1)
               r1=sqrt((xx(ip3)-xx(ip2))**2+(yy(ip3)-yy(ip2))**2+(zz(ip3)-zz(ip2))**2)
               if(r1>10.0.or.r1<0.001)then
!                  print*,"neighbour bond position severa wrong CH2-1 again"
                  xx(i)=ax+(xx(ip2)-xx(ip3))/bond(ip2,2)
                  yy(i)=ay+(yy(ip2)-yy(ip3))/bond(ip2,2)
                  zz(i)=az+(zz(ip2)-zz(ip3))/bond(ip2,2) 
                  ASSrd(i)=2
               else
                  fbd=bd/r1
                  xx(i)=ax+(xx(ip3)-xx(ip2))*fbd
                  yy(i)=ay+(yy(ip3)-yy(ip2))*fbd
                  zz(i)=az+(zz(ip3)-zz(ip2))*fbd  
               endif
            else
               fbd=bd/r1  !!adjust bond length
               if(ii<2)then
               xx(i)=ax+fbd*(x-ax)
               yy(i)=ay+fbd*(y-ay)
               zz(i)=az+fbd*(z-az)              
               else
                  xx(i)=0.5*(xx(i)+ax+fbd*(x-ax))
                  yy(i)=0.5*(yy(i)+ay+fbd*(y-ay))
                  zz(i)=0.5*(zz(i)+az+fbd*(z-az))
               endif
            endif
  
            x=xx(ip2)
            y=yy(ip2)
            z=zz(ip2)
            call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,-angg)  !H on C, each H is trans   
            r1=sqrt((x-ax)**2+(y-ay)**2+(z-az)**2)
            if(r1<0.01.or.r1>5.0)then
!               print*, "warning CH2-2",r1,bd,ress2(i),i,atom(i,1)
               r1=sqrt((xx(ip3)-xx(ip2))**2+(yy(ip3)-yy(ip2))**2+(zz(ip3)-zz(ip2))**2)
               if(r1>10.0.or.r1<0.001)then
                  print*,"neighbour bond position severa wrong CH2-2",i,xx(i)
                  xx(i)=ax+(xx(ip2)-xx(ip3))/bond(ip2,2)
                  yy(i)=ay+(yy(ip2)-yy(ip3))/bond(ip2,2)
                  zz(i)=az+(zz(ip2)-zz(ip3))/bond(ip2,2) 
                  ASSrd(i)=2
               else
                  fbd=bd/r1
                  xx(i-1)=ax+(xx(ip2)-xx(ip3))*fbd
                  yy(i-1)=ay+(yy(ip2)-yy(ip3))*fbd
                  zz(i-1)=az+(zz(ip2)-zz(ip3))*fbd 
               endif
            else
               fbd=bd/r1  !!adjust bond length
               if(ii<2)then
               xx(i-1)=ax+fbd*(x-ax)
               yy(i-1)=ay+fbd*(y-ay)
               zz(i-1)=az+fbd*(z-az)   
               else
                  xx(i-1)=0.5*(xx(i-1)+ax+fbd*(x-ax))
                  yy(i-1)=0.5*(yy(i-1)+ay+fbd*(y-ay))
                  zz(i-1)=0.5*(zz(i-1)+az+fbd*(z-az))
               endif
            endif
         enddo  !!!ii=1,2
!print*,">>",x,y,z,xi,yi,zi,r1,fbd,i,atom(i,1),ax,ay,az 
         endif   !!  2H finished
         i=i-1
      else   !!1H case, sp2 has been assign, O-H and S-H also done trans-; only for sp3 C case
         if(kx0==3)then   !!sp3 case            
   !         if(ca(ress2(i),2)==2.or.ca(ress2(i),2)==6.or.ca(ress2(i),2)==8)bd=bd*0.95        
            x=(xx(ipp0(1))+xx(ipp0(2))+xx(ipp0(3)))/3.0
            y=(yy(ipp0(1))+yy(ipp0(2))+yy(ipp0(3)))/3.0
            z=(zz(ipp0(1))+zz(ipp0(2))+zz(ipp0(3)))/3.0
            r1=sqrt((x-ax)**2+(y-ay)**2+(z-az)**2)
            if(r1<0.01.or.r1>10.0)then
!               print*, "warning H1-sp3",ress2(i),atom(i,1),i,r1
               ASSrd(i)=2
               xx(i)=ax+0.3
               yy(i)=ay+0.6
               zz(i)=az+0.7
            else
               fbd=bd/r1  !!adjust bond length
               xx(i)=ax+fbd*(ax-x)
               yy(i)=ay+fbd*(ay-y)
               zz(i)=az+fbd*(az-z)
            endif
         elseif(kx0==2)then   !!sp2, aromatic ring H, H-N
            bd=bd*0.95
            x=(xx(ipp0(1))+xx(ipp0(2)))*0.5
            y=(yy(ipp0(1))+yy(ipp0(2)))*0.5
            z=(zz(ipp0(1))+zz(ipp0(2)))*0.5
            r1=sqrt((x-ax)**2+(y-ay)**2+(z-az)**2)
            if(r1<0.01.or.r1>5.0)then
               ASSrd(i)=2
               xx(i)=ax+0.3
               yy(i)=ay+0.6
               zz(i)=az+0.7
            else
               fbd=bd/r1  !!adjust bond length
               xx(i)=ax+fbd*(ax-x)
               yy(i)=ay+fbd*(ay-y)
               zz(i)=az+fbd*(az-z)
            endif
         elseif(kx0==1)then   !!H-O,H-S
            bd=bd*0.95
            vr1(1,1)=xx(ipp(1))-xx(ip2)
            vr1(1,2)=yy(ipp(1))-yy(ip2)
            vr1(1,3)=zz(ipp(1))-zz(ip2)
            x=xx(ip2)
            y=yy(ip2)
            z=zz(ip2)
            xi=vr1(1,2)*vr0(3)-vr1(1,3)*vr0(2)
            yi=vr1(1,3)*vr0(1)-vr1(1,1)*vr0(3)
            zi=vr1(1,1)*vr0(2)-vr1(1,2)*vr0(1)
            call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !H on N, each H is trans            
            r1=sqrt((x-ax)**2+(y-ay)**2+(z-az)**2)
            if(r1<0.01.or.r1>5.0)then
!               print*, "warning O-H",ress2(i),atom(i,1),i,r1
               ASSrd(i)=2
               xx(i)=ax+0.3
               yy(i)=ay+0.6
               zz(i)=az+0.7  
            else
               fbd=bd/r1  !!adjust bond length
               xx(i)=ax+fbd*(x-ax)
               yy(i)=ay+fbd*(y-ay)
               zz(i)=az+fbd*(z-az)
            endif
         endif
      endif   !!3H
   enddo  !!kk<NHHig  
!print*,"adjust 1H"   
if(icontrol.eq."1")call AD_1H  !!!this incluse Imerge(1)
410 continue
    end subroutine HADD

subroutine AD_1H
implicit none
integer icc,i,ii,ip2,ind,np0,ir0,ir1,Mh1
real arrin(50)
real bd,dx,dy,dz,rf,r,vx1,vy1,vz1


  do i=1,natom
 !    if(atomty(i).eq." H  ".or.atomty(i).eq." HG ".or.atomty(i).eq." HG1".or.atomty(i).eq." HH ")then
    ! if(atomty(i).eq." HG ".or.atomty(i).eq." HG1".or.atomty(i).eq." HH ")then
     if(aty(i).eq."H")then
        icc=bonda(i,1)  !!bonded heavy atom
        if(.not.(aty(icc).eq."O".or.aty(icc).eq."S"))cycle
  !      print*,"ADD1H00",i,atomty(i)
        bd=bond(i,1)
        ax=xx(icc)  !!origin point
        ay=yy(icc)
        az=zz(icc)
        ip2=bonda(icc,1) !ip2-icc-iH
        np0=0 !1record the number of atoms in contact
        j=0
        ir0=ress(i)
        do while(j<natom)
           j=j+1
           ir1=ress(j)
           if(ir1.eq.ir0)cycle
           !           if(j.eq.i.or.j.eq.icc.or.j.eq.ip2)cycle
           r=(xx(i)-xx(j))**2+(yy(i)-yy(j))**2+(zz(i)-zz(j))**2
         !  print*,i,j,r,ir1,Nres
           if(r>144.0)then
              if(ir1<Nres)then !!12A no contact with this residue
                 j=ca(ir1+1,1)-2
         !        print*,">>",i,j,r,np0
                 cycle
              else
                 goto 231
              endif
           endif
           if(r>0.01.and.r<25.0)then
              np0=np0+1
              HH1(np0)=j
              rHH1(np0)=r !!use aty(j) to judge whether its overlap
           endif
        enddo
231     continue
        if(np0<1)cycle
 !       print*,"nop",i,np0,atomty(i)
        do j=1,np0
           arrin(j)=rHH1(j)
        enddo
        if(np0>1)then
           call sortindex(np0,arrin,indx) !!increaing order
        else
           indx(1)=1           
        endif
!!decide the vecoter summ  
        x=0
        y=0
        z=0
        do j=1,np0
!!four cases, Hbond yes/not, attractive/repulsive
           ind=indx(j)
           ip=HH1(ind)  !!atom id
           if(rHH1(ind)<0.01)cycle
           r=(atmpar(i,3)+1.0)**2
           if(rHH1(ind)<r.and.aty(ip).ne."O")then !!repulsion and Hbond
              MH1=-1
           elseif(aty(ip).eq."O")then
              MH1=7
           elseif(aty(ip).eq."N")then
              MH1=1
           else
              MH1=0              
           endif      
           vx1=(xx(ip)-xx(i))/rHH1(ind)
           vy1=(yy(ip)-yy(i))/rHH1(ind)
           vz1=(zz(ip)-zz(i))/rHH1(ind)
           x=x+vx1*MH1
           y=y+vy1*MH1
           z=z+vz1*MH1
        enddo  !!the vector from current position
        r=sqrt(x*x+y*y+z*z)
        xi=xx(ip2)-ax
        yi=yy(ip2)-ay
        zi=zz(ip2)-az
        if(r<0.01)r=0.01
        x=x/r
        y=y/r
        z=z/r
        if(aty(icc).eq."O")then
           rf=(x*(xx(i)-xx(icc))+y*(yy(i)-yy(icc))+z*(zz(i)-zz(icc)))/bond(i,1)/0.956 !cos(0.1)
        else !S
           rf=(x*(xx(i)-xx(icc))+y*(yy(i)-yy(icc))+z*(zz(i)-zz(icc)))/bond(i,1)/0.996 
        endif
        if(abs(rf)>=1.0)cycle
        angg=acos(rf)
        x=xx(i)
        y=yy(i)
        z=zz(i)
        call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !H on N, each H is trans   
   !     r=sqrt((x-xx(i))**2+(y-yy(i))**2+(z-zz(i))**2)
!        r=sqrt((x-xx(icc))**2+(y-yy(icc))**2+(z-zz(icc))**2)
!write(*,"A10,A5,I5,f6.3,7f8.3"),"rotation angg",atomty(i),i,angg,xx(i),yy(i),zz(i),x,y,z,r  
        xx(i)=x
        yy(i)=y
        zz(i)=z
     endif !!H
  end do !!all spH finish
end subroutine AD_1H

!!flag 0 shift !!the first step
!!flag 1 shift and imerge
!!flag 2 only imerge
subroutine IMERGE(iflag)
implicit none
integer iflag,nkatm
!!imerge chain into a LATTICE******************************************************************
if(NNZR>10)then
   do i=1,NNZR
      ix=NZR(i,1)
      iy=NZR(i,2)
      iz=NZR(i,3)
      SITE(ix,iy,iz)=0
   enddo
   do i=1,7
      do j=1,natom
         SITT(j,i)=0
      enddo
   enddo
   NNZR=0
else
   site=0   !!!!here need optimization if used in 
   SITT=0  !!clash
endif

!do i=1,natom   !!the replica 1 as rt
!   if(xx(i)>990)then
!      ic=bonda(i,1)
!      xx(i)=xx(ic)+0.6    !be carefull about here  !!!, only affect energy calculation and other things
!      yy(i)=yy(ic)+0.6
 !     zz(i)=zz(ic)+0.6
!   endif
!enddo
if(iflag.eq.1.or.iflag.eq.0)then
   nkatm=0
   do i=1,natom
      if(xx(i).ne.999.0)then
         nkatm=nkatm+1
         xx0(nkatm)=xx(i)
         yy0(nkatm)=yy(i)
         zz0(nkatm)=zz(i)
      endif
   enddo
    shiftx=0
    shifty=0
    shiftz=0
      call sort(xx0,nkatm)
      NMAXx=int(xx0(nkatm)-xx0(1))*rmesh
      call sort(yy0,nkatm)
      NMAXy=int(yy0(nkatm)-yy0(1))*rmesh
      call sort(zz0,nkatm)
      NMAXz=int(zz0(nkatm)-zz0(1))*rmesh
      if(NMAXx>=Nfbc.or.NMAXy>=Nfbc.or.NMAXz>=Nfbc)then
 !        print*,">>>",nkatm
 512 format(A15,3I5,6f8.3)
         write(*,512),"protein too large",NMAXx,NMAXy,NMAXz,xx0(nkatm),xx0(1),yy0(nkatm),yy0(1),zz0(nkatm),zz0(1)
         stop "one of the protein length exceed simulation lattice"
      endif
      if(xx0(1)<Ledge*rmesh.or.xx0(1)+NMAXx+1>(La-ledge)*rmesh)then
         shiftx=-xx0(1)+ledge*rmesh
         do i=1,natom
            if(xx(i)<999.0)xx(i)=xx(i)+shiftx
         enddo
      endif
      if(yy0(1)<Ledge*rmesh.or.yy0(1)+NMAXy+1>(La-ledge)*rmesh)then
         shifty=-yy0(1)+ledge*rmesh
         do i=1,natom
            if(yy(i)<999.0)yy(i)=yy(i)+shifty
         enddo
      endif
      if(zz0(1)<Ledge*rmesh.or.zz0(1)+NMAXz+1>(La-ledge)*rmesh)then
         shiftz=-zz0(1)+ledge*rmesh
         do i=1,natom
            if(zz(i)<999.0)zz(i)=zz(i)+shiftz
         enddo
      endif
!print*,"shift",Nhbonds
!pause
      if(iflag.eq.0)goto 101
   endif

    ip=0
    do i=1,natom
       if(xx(i)>Nfbc/rmesh.or.xx(i)<1.0.or.yy(i)>Nfbc/rmesh.or.yy(i)<1.0.or.zz(i)>Nfbc/rmesh.or.zz(i)<1.0)cycle
       ix=Nint(xx(i)*rmesh)
       iy=Nint(yy(i)*rmesh)
       iz=Nint(zz(i)*rmesh)
!print*,i,atomty(i),ress(i),resd(i),ix,iy,iz,xx(i),yy(i),zz(i)
  !     if(ix>1.and.ix<200.and.iz>1.and.iz<200.and.iz>1.and.iz<200)icc=icc+1
       ip=SITE(ix,iy,iz)
       if(ip<1.or.ip>natom)then
          SITE(ix,iy,iz)=i
          NNZR=NNZR+1
          NZR(NNZR,1)=ix
          NZR(NNZR,2)=iy
          NZR(NNZR,3)=iz
          goto 121
       endif
       do j=1,NSITT
          if(SITT(i,j)==ip.or.SITT(ip,j)==i)goto 121
       enddo             
       do j=1,NSITT
          if(SITT(i,j)<1)then
             SITT(i,j)=ip    !!!clashed IP, single direction
             do k=1,NSITT
                if(SITT(ip,k)<1)then
                   SITT(ip,k)=i
                   goto 121
                else
      !             if(k==NSITT)print*,"warning, one clash lost",i,site(ix,iy,iz)
                endif
             enddo
             goto 121
          else
      !       if(j==NSITT)print*,"warning, one clash lost",i,site(ix,iy,iz)                
          endif
       enddo
121    continue
    enddo

   if(icontrol.eq."5")then
      call angle
      call calclash
      cx=0
      cy=0
      cz=0
      do j=1,Nres
         i=ca(j,1)
         cx=cx+xx(i)
         cy=cy+yy(i)
         cz=cz+zz(i)
      enddo
      cx=cx/Nres
      cy=cy/Nres
      cz=cz/Nres
      Rg=0
      do j=1,Nres
         i=ca(j,1)
         Rg=Rg+(xx(i)-cx)**2+(yy(i)-cy)**2+(zz(i)-cz)**2
      enddo
      Rg=sqrt(Rg/Nres)
511 format(A16,I6,A8,I5,A4,f12.3)
      write(*,511),"CACLASH(<3.6A):",Ncaclash," BROKEN",Nbroken," Rg",Rg
   endif
101 continue
 end subroutine IMERGE

!!this subroutine calculate angles and output chi1
subroutine angle
implicit none
integer iresin,iatom,iw,i,j,ires
     do ires=1,Nres
         iatom=ca(ires,1)
         iresin=ca(ires,2)
!print*,"angle",ires,iresin,iatom,xx(iatom+5)
         if(ires>1)then
            iw=ca(ires-1,1)+1  !C
            phipsichi(ires,1)=torsion_angle(iw,iatom-1,iatom,iatom+1)  !CNCaC  phi
         endif
         if(ires<Nres)then
            iw=ca(ires+1,1)-1
            phipsichi(ires,2)=torsion_angle(iatom-1,iatom,iatom+1,iw) !NCaCN psi
         endif
         if(xx(iatom+4)>990.0)cycle
         if(iresin>2)then   !!GLY and ALA has no chi1
            phipsichi(ires,3)=torsion_angle(iatom-1,iatom,iatom+3,iatom+4) !NCaCb-Cg(Cg1)(O ser,S  Cys) chi1
            if(iresin>6)then
               phipsichi(ires,4)=torsion_angle(iatom-1,iatom,iatom+3,iatom+5) !NCaCb-CG2
!               if(iresin.eq.9.or.(iresin>=13.and.iresin<=16))then  !MET GLU GLN && LYS ARG
!                  phipsichi(i,5)=torsion_angle(iatom,iatom+3,iatom+4,iatom+5) !CaCbCg-Cd
!               endif
               !    if(ires.eq.13.or.ires.eq.16)then !!LYS and ARG
              !       phipsichi(i,6)=torsion_angle(iatom+3,iatom+4,iatom+5,iatom+6) !CbCG-Cd-Ce
               !   endif
            endif
         endif
!         print*,ires,iatom,natom,iw
      enddo

      if(icontrol.eq."7".or.icontrol.eq."8".or.icontrol.eq."5")then
         open(71,file="chi1")
513 format(I5,A5,4f9.4)
         do j=1,Nres
            write(71,513),j,resd(ca(j,1)),phipsichi(j,3),phipsichi(j,4),phipsichi(j,1),phipsichi(j,2) !!chi1 chi2 phi psi
         enddo
         close(71)
 !        print*,"chi1 write finish",j
      endif
end subroutine angle

!!iflag =1, count all Hbond, =2 only count backbone Hbond
SUBROUTINE Hbonds(iflag)
   integer ipacc,ipdn,ipab,ih,nk,jj,ipacc2,ipacc0
   integer ia0,ib0,ic0,nhb,iflag
   integer Lamin,Lbmin,Lcmin,Lamax,Lbmax,Lcmax,iaa,ibb,icc,idd
   real drdh,drha,draa,drda,drha2  !from donor to acceptor
   real hphi,htheta,hchi,e1,e2,e3,e4,eh,ehh

Nhbonds=1
EHbond=0

!if(iflag==0)then   !!count the hydrogen bond only, excluded 1-4
do ih=1,natom
if(ATMHB(ih)>-0.5)cycle   !!only consider the H donor
!if(bonda(ih,1).eq.ipdn)cycle  !!for first residue
if(iflag.eq.2.and.atomty(ih).ne." H  ")cycle  !!ignore the first residue
ipdn=bonda(ih,1)
drdh=bond(ih,2)          !!H-D
nhb=0  !1number of H-bond with this hydrogen atoms
   ia0=Nint(xx(ih)*rmesh)
   ib0=Nint(yy(ih)*rmesh)
   ic0=Nint(zz(ih)*rmesh)
   Lamin=max(1,ia0-LintH)
   Lbmin=max(1,ib0-LintH)    
   Lcmin=max(1,ic0-LintH)
   Lamax=min(La,ia0+LintH)
   Lbmax=min(La,ib0+LintH)
   Lcmax=min(La,ic0+LintH)
   do ia=Lamin,Lamax
      do ib=Lbmin,Lbmax
         do ic=Lcmin,Lcmax
            if(SITE(ia,ib,ic)<1)cycle
            ipacc0=SITE(ia,ib,ic)
            if(ipacc0>natom)cycle
            do jj=0,NSITT
               if(jj>0)then
                  if(SITT(ipacc0,jj)>0)then
                     ipacc=SITT(ipacc0,jj)
                  else
                     goto 522
                  endif
               else
                  ipacc=ipacc0
               endif
               if(ATMHB(ipacc)<=0.or.(atomty(ipdn).eq.' N  '.and.atomty(ipacc).eq.' O  '.and.abs(ress(ipdn)-ress(ipacc))<=1))cycle  !!search acceptor only
     !          do k=1,bnd4(ih,0)
     !             if(bnd4(ih,k)==ipacc)goto 225
     !          enddo
               ipab=bonda(ipacc,1)  !!acceptor connect heavy  
               drda=sqrt((xx(ipdn)-xx(ipacc))**2+(yy(ipdn)-yy(ipacc))**2+(zz(ipdn)-zz(ipacc))**2)   !!D-A
               if(drda>rDAcut)goto 225    
               drha=sqrt((xx(ipacc)-xx(ih))**2+(yy(ipacc)-yy(ih))**2+(zz(ipacc)-zz(ih))**2)          !!H-A 
               if(drha>rHAcut.or.drha<rHAcutmin)goto 225 !.or.drha<rHAcutmin)goto 225
               draa=sqrt((xx(ipab)-xx(ipacc))**2+(yy(ipab)-yy(ipacc))**2+(zz(ipab)-zz(ipacc))**2)   !!A-AB
               drha2=sqrt((xx(ipab)-xx(ih))**2+(yy(ipab)-yy(ih))**2+(zz(ipab)-zz(ih))**2)  !!H-AB
               hphi=0  !!<DHA
               htheta=0      !!<HA-Ab       
               if(drdh*drda>dismin)hphi=acos((drdh*drdh+drha*drha-drda*drda)*0.5/drdh/drha)/hpi      !!<DHA, 90~180
               if(hphi<DHAcut)goto 225
               if(drdh*drda>dismin)htheta=acos((drha*drha+draa*draa-drha2*drha2)*0.5/drha/draa)/hpi  !!<HA(AB)  90~180
               if(htheta<HAAbcut)goto 225 
!!!!record the energy term and select the minimum as the energy of this hydrogen bond, get the best positioned Hbond
                e4=0.0
                hchi=0.0
                idd=1
               if(bonda(ipab,5)<3)then  !!in aromatic ring planar atomty(ipacc)=='O')then   !!only for sp2
                  if(bonda(ipab,1).ne.ipacc)then
                     hchi=torsion_angle(bonda(ipab,1),ipab,ipacc,ih)/hpi
                  elseif(bonda(ipab,2).ne.ipacc)then
                     hchi=torsion_angle(bonda(ipab,2),ipab,ipacc,ih)/hpi
                  endif                     
                  if(abs(hchi)>dHAAx)goto 225
                  idd=int(hchi/4.5)+40     !!-180~180 
                  if(H_BOND(idd,4)>0)e4=1.1*log(1.0-H_BOND(idd,4))
               endif
               iaa=int((drha-1.4)/0.02)
               ibb=int((hphi-20)/2)
               icc=int((htheta-20)/2)
               e1=0.0
               e2=0.0
               e3=0.0
               if(H_BOND(iaa,1)>0)e1=0.82*log(1.0-H_BOND(iaa,1))
               if(H_BOND(ibb,2)>0)e2=0.35*log(1.0-H_BOND(ibb,2))
               if(H_BOND(icc,3)>0)e3=0.82*log(1.0-H_BOND(icc,3))
           !    if(H_BOND(iaa,1)>0)e1=-log(H_BOND(iaa,1))
           !    if(H_BOND(ibb,2)>0)e2=-log(H_BOND(ibb,2))
           !    if(H_BOND(icc,3)>0)e3=-log(H_BOND(icc,3))
           !    if(H_BOND(idd,4)>0)e4=-log(H_BOND(idd,4))
               Eh=e1+e2+e3 !+e4
!               if(e1*e2*e3*e4<dismin)goto 225
               nhb=nhb+1
               if(nhb==1)then
                  Ehh=Eh
                  ipacc2=ipacc
                  htbon(NHbonds,1)=drha
                  htbon(NHbonds,2)=hphi
                  htbon(NHbonds,3)=htheta
                  htbon(NHbonds,4)=Ehh !hchi
               else
      !            if(atomty(ipdn).eq." N  ".and.atomty(ipacc).eq." O  ")Eh=1.2*Eh
                  if(Eh<Ehh)then
                     Ehh=Eh
                     ipacc2=ipacc
                     htbon(NHbonds,1)=drha
                     htbon(NHbonds,2)=hphi
                     htbon(NHbonds,3)=htheta
                     htbon(NHbonds,4)=Ehh !hchi
                   endif
               endif
225            continue
            enddo  !!NSITT
522         continue
     enddo
  enddo
enddo
226 continue

if(nhb>0)then
   if(iflag==2)then  !!only account backbone Hbond
      if(atomty(ipdn).eq." N  ".and.atomty(ipacc2).eq." O  ")then
         EHbond=EHbond+Ehh
         hbn(NHbonds,1)=ipdn
         hbn(NHbonds,2)=ipacc2
         Nhbonds=Nhbonds+1
      endif
   else
      EHbond=EHbond+Ehh
      hbn(NHbonds,1)=ipdn
      hbn(NHbonds,2)=ipacc2
      Nhbonds=Nhbonds+1
   endif
endif
!print*,"ih",ih,ress(ih),Nhbonds,EHbond,Ehh
enddo  !ih natom
Nhbonds=Nhbonds-1
if(iflag==2)then  !!count the hit number
   Nhbhit=0
   do i=1,Nreshb0
      i1=reshbn(i,1)
      i2=reshbn(i,2)
      ir1=ress(i1)
      ir2=ress(i2)
      do j=1,Nhbonds        
         if(hbn(j,1).eq.i1.and.hbn(j,2).eq.i2)then
            Nhbhit=Nhbhit+1     
            if(ir1>1)hbvar(ir1-1)=2  !!0 need move, 1 keep frozen
            hbvar(ir2)=2
            goto 92
         endif
      enddo
      if(.not.templatelog)then
         if(ir1>1)hbvar(ir1-1)=0
         hbvar(ir2)=0
      endif
92    continue
   enddo
end if  !!2 count hit
!pause
 end subroutine Hbonds

subroutine ass2nd
implicit none
real,parameter::phi0hx=-2.44346,psi0bt=1.48353,psi1bt2=-2.9067 
real,parameter::pi2=1.570796327,pi3=1.047197551,pi4=0.785398164,pi6=0.523598776
!integer,dimension(Lmax,4)::seq2 !1 type, 2 index, 3 4 phi, psi bias
integer,dimension(Nres,2)::seq3  !1 type 2 start region 3 end residue
real,dimension(Nres,2)::seq4    !average phi, psi of each pitch 
integer i,j,id,i0,i1
integer iflag
real phii,psii

!print*,"GGGG",ires0,ires1
      seq2=1

do i=2,Nres-1
      phii=phipsichi(i,1)
      psii=phipsichi(i,2)
      id=int(0.1*((phii*37+psii)/hpi+6840))+1  
!      seq2(i,2)=id
   if(((phii>phi0hx.and.phii<-1.0*pi4.and.psii>-1.0*pi3.and.psii<pi3).or.(phii>pi4.and.phii<pi2.and.psii>0.and.psii<pi3)).and.RAMchra(id,1)>0.005.and.RAMchra(id,1)>=RAMchra(id,2))then !helix
      seq2(i,1)=2
   elseif((phii>-1.0*pi.and.phii<-1.0*pi4.and.(psii>psi0bt.and.psii<pi.or.psii>-1.0*pi.and.psii<psi1bt2)).and.RAMchra(id,2)>0.005.and.RAMchra(id,2)>=RAMchra(id,1))then   !1sheet   
      seq2(i,1)=4
   endif
enddo  !!first assign finish

!!!smooth seq and decide pitch index
    do i=2,Nres-2  !clean seq.dat
       i0=seq2(i,1)
       if(i0.ne.seq2(i-2,1).and.i0.ne.seq2(i-1,1).and.i0.ne.seq2(i+1,1).and.i0.ne.seq2(i+2,1))seq2(i,1)=1
    enddo
    do i=2,Nres-1  !clean seq.dat
       i0=seq2(i,1)
       if(i0.ne.seq2(i-1,1).and.i0.ne.seq2(i+1,1).and.seq2(i-1,1).eq.seq2(i+1,1).and.ca(i,2).ne.8)seq2(i,1)=seq2(i-1,1)
    enddo
seq2(1,1)=seq2(2,1)
seq2(nres,1)=seq2(nres-1,1)
if(icontrol.eq."8")then
open(10,file="seq0.dat")
515 format(I5,3x,A3,1x,I4,1x,I4)
do i=1,Nres
   write(10,515),i,resd(ca(i,1)),seq2(i,1),9
enddo
close(10)
endif
end subroutine ass2nd
!!this subroutine to get the restraint structure
!!read the structure, and rotate the native or the comco to xx,yy,zz
subroutine restr_stru
implicit none
real,parameter::bshcacut=34.81 !37.21 !40.96 !25.0 !22.09 !!4.7 for CA in two strands
integer,dimension(Lmax,2)::hbnal,hbnsh!1 start of the pitch,2 end of the pitch (residue series)
integer,dimension(NMAX,2)::mptch  !!record the number of HB in each pitch, based on seq, 2 pitch type
integer,dimension(500,2)::mpbsh,mpbhe  !the bsheet, and helix pitches,start/end residue index 1/2
integer,dimension(500,3)::shpad !!record beta-strand pair, 1-3 pitch pair index
integer,dimension(Lmax)::selc
real,dimension(Lmax)::arrin !!helix only equal for helix, label in samepitch, helix>0, sheet<0

integer ir1,ir2,ibh,i0,i1,i,k,ipca1,ipca2
integer jj,nhb0,mbsh,mah,npitch,nhb0al,nhb0sh
integer k2,kn,km,k1,krdi,kd,np1,np2
integer Nsecd
real rr
character*4 aa

!print*,"icontrl 66??",icontrol

      seq2(1,1)=seq2(2,1)
      seq2(Nres,1)=seq2(Nres-1,1)
      k=1
      jj=0
      open(12,file="PRHB",status="old",err=201)
      do while(k>0)
         read(12,*,end=201),i,aa,i0,aa,i1
     !    ibh=hra(ca(i0,2))-1
     !    if(i0.eq.Nres)ibh=ibh+1
         jj=jj+1
         reshbn(jj,1)=ca(i0,1)-1  !ca(i0,1)+ibh
         reshbn(jj,2)=ca(i1,1)+2
         k=k+1
      enddo     
201 close(12)
      nhb0=jj
      Nreshb0=nhb0
      PRHBlog=.true.
      if(nhb0<2)then
         PRHBlog=.false.
         if(.not.(icontrol.eq."6".or.icontrol.eq."66"))goto 800
      endif

!print*,"constru",jj,Nreshb0,nhb0

!!!make concensus of seq and seq2 , then smooth
      Nsecd=0
      seq(1,1)=seq(2,1)
      seq(Nres,1)=seq(Nres-1,1)
      do i=1,Nres
         if(seq(i,1).eq.2.or.seq2(i,1).eq.2)then
            seq(i,1)=2
            Nsecd=Nsecd+1
         elseif(seq(i,1).eq.4.and.seq2(i,1).eq.4)then
            seq(i,1)=4
            Nsecd=Nsecd+1  !if(Nreshb0<Nsecd)then  !!use looser boudary for HB restraints
         else
            seq(i,1)=1
         endif
      enddo
!!!smooth seq and decide pitch index
    do i=2,Nres-2  !clean seq.dat
       i0=seq(i,1)
       if(i0.ne.seq(i-2,1).and.i0.ne.seq(i-1,1).and.i0.ne.seq(i+1,1).and.i0.ne.seq(i+2,1))seq(i,1)=1
    enddo
!    do i=2,Nres-3
!       i0=seq(i,1)
!      if(i0.ne.0.and.i0.ne.seq(i-2,1).and.i0.ne.seq(i-1,1).and.i0.ne.seq(i+2,1).and.i0.ne.seq(i+3,1))seq(i,1)=0    
!    enddo
    do i=2,Nres-1  !clean seq.dat
       i0=seq(i,1)
       if(i0.ne.seq(i-1,1).and.i0.ne.seq(i+1,1).and.seq(i-1,1).eq.seq(i+1,1))seq(i,1)=seq(i-1,1)
    enddo
    seq(1,1)=seq(2,1)
    seq(Nres,1)=seq(Nres-1,1)
 !   open(10,file="qes.dat")
 !  do i=1,Nres
 !      write(10,"I5,3x,A3,1x,I4,1x,I4"),i,resd(ca(i,1)),seq(i),9
 !   enddo
 !   close(10)


!!!assign pitches by concensus of seq.dat and seq2.dat
   i=0
   mbsh=0  !!number of bsheet strands
   mah=0  !!number of helix strands
   npitch=0
   do while(i<Nres)
      i=i+1
      if(seq(i,1).eq.4)then !sheet
         j=i
         do while(seq(j,1).eq.4.and.j<=Nres) !.or.seq2(j,2).eq.k2).and.j<=Nres)
            j=j+1
         enddo
         k2=j-i-1  !!number of residues
         if(k2>2)then   
            npitch=npitch+1
            mbsh=mbsh+1
            mpbsh(mbsh,1)=i
            mpbsh(mbsh,2)=j-1
            do ii=i,j
               seq2(ii,2)=npitch  !!index
            !   seq2(ii,3)=i   !!start residue
            !   seq2(ii,4)=j   !!end residue
            enddo
            i=j
         endif
      elseif(seq(i,1).eq.2)then  !!helix
          j=i
         do while(seq(j,1).eq.2.and.j<=Nres)  ! .or.seq2(i,2).eq.k2)
            j=j+1
         enddo
         k2=j-i-1  !!number of residues
         if(k2>2)then  
            npitch=npitch+1
            mah=mah+1
            mpbhe(mah,1)=i
            mpbhe(mah,2)=j-1
            do ii=i,j
               seq2(ii,2)=npitch  !!index
             !  seq2(ii,3)=i   !!start residue
             !  seq2(ii,4)=j   !!end residue
            enddo
            i=j
         endif
      endif
   enddo
!!assign sheet pitches
   shpad=0
   selc=0
   if(mbsh<2)goto 22  !!no b sheet for this protein
   do i=1,mbsh
!write(*,"A4,I3,5I5"),"be00",i,mpbsh(i,1),mpbsh(i,2),shpad(i,1),shpad(i,2),shpad(i,3)
      i1=mpbsh(i,1) !!start residue
      i2=mpbsh(i,2) !!end residue
      kn=0  !final Hbond pair
      km=0
      kk=0
      do k=1,mbsh
         if(k.eq.i)cycle
         k1=mpbsh(k,1)
         k2=mpbsh(k,2)
         do ii=i1,i2
            ipca1=ca(ii,1)
            do jj=k1,k2
               ipca2=ca(jj,1)
               r=(xx(ipca1)-xx(ipca2))**2+(yy(ipca1)-yy(ipca2))**2+(zz(ipca1)-zz(ipca2))**2  !!change to combo
  !             r=(xx22(ii)-xx22(jj))**2+(yy22(ii)-yy22(jj))**2+(zz22(ii)-zz22(jj))**2      !!this is combo
               if(r<=bshcacut)then
                  km=km+1
               endif
            enddo
         enddo
         if(km>kn)then
            kn=km
            kk=k
         endif
      enddo
      if(kk>0)then !.and.kn>5.and.kk.ne.i)then
         jj=-1*i
         do ii=i1,i2
            selc(ii)=jj  !!in same pitch
         enddo
         if(shpad(i,1)<1.or.kk.eq.shpad(i,1))then
            shpad(i,1)=kk
         elseif(shpad(i,2)<1.or.kk.eq.shpad(i,2))then
            shpad(i,2)=kk
         elseif(shpad(i,3)<1.or.kk.eq.shpad(i,3))then
            shpad(i,3)=kk
         endif
         if(shpad(kk,1)<1.or.i.eq.shpad(kk,1))then
            shpad(kk,1)=i
         elseif(shpad(kk,2)<1.or.i.eq.shpad(kk,2))then
            shpad(kk,2)=i
         elseif(shpad(kk,3)<1.or.i.eq.shpad(kk,3))then
            shpad(kk,3)=i
         endif
      else  !!without any pair
         if(i>1)then
            ii=i-1
         else
            ii=2
         endif
         do jj=1,3
            if(shpad(ii,jj)>0)then
               kk=shpad(ii,jj)+1
               if(i==1)kk=mbsh
               if(shpad(kk,1)<1.or.i.eq.shpad(kk,1))then
                  shpad(kk,1)=i                  
               elseif(shpad(kk,2)<1.or.i.eq.shpad(kk,2))then
                  shpad(kk,2)=i
               elseif(shpad(kk,3)<1.or.i.eq.shpad(kk,3))then
                  shpad(kk,3)=i
               else
                  kk=ii
                  if(shpad(kk,1)<1.or.i.eq.shpad(kk,1))then
                     shpad(kk,1)=i                  
                  elseif(shpad(kk,2)<1.or.i.eq.shpad(kk,2))then
                     shpad(kk,2)=i
                  elseif(shpad(kk,3)<1.or.i.eq.shpad(kk,3))then
                     shpad(kk,3)=i
                  endif
               endif
               shpad(i,jj)=kk
            endif
         enddo
      endif
!write(*,"A4,I3,5I5"),"beta",i,mpbsh(i,1),mpbsh(i,2),shpad(i,1),shpad(i,2),shpad(i,3)
   enddo  !!all mbsh
!!!fixed those strand without pair to form sheet

!!assign helix
22 continue
   do i=1,mah
      i1=mpbhe(i,1)
      i2=mpbhe(i,2)
      do ii=i1,i2
         selc(ii)=i
      enddo
   enddo
!!helix assign finish

!!1) filter the Hbond list, only consider those with secondary information
      NHB0al=0 !number Hbond in helix
      NHB0sh=0 !number Hbond in sheet
      hbnal=0
      hbnsh=0
      do i=1,Nhbonds
         if(hbn(i,1)<1)cycle !!after filtered
         if(atomty(hbn(i,1)).ne." N  ".or.atomty(hbn(i,2)).ne." O  ")cycle  !!only consider backbone hbond
         ir1=ress(hbn(i,1))
         ir2=ress(hbn(i,2))
         k1=selc(ir1)
         k2=selc(ir2)
         krdi=abs(ir1-ir2)
         kd=seq(ir1,1)+seq(ir2,1)
         if(kd>=1.and.kd<=4)then !.and.krdi>2.and.krdi<6.and.abs(k1-k2)<2)then     !!helix
           ! if(k1*k2==0)cycle  !!no 2nd structure0  
           ! if(i<Nhbonds.and.hbn(i,1).eq.hbn(i+1,1).and.abs(ir2-ir1).ne.4)cycle
           ! if(i>1.and.hbn(i,1).eq.hbn(i-1,1).and.abs(ir2-ir1).ne.4)cycle   
            Nhb0al=Nhb0al+1
            hbnal(Nhb0al,1)=hbn(i,1)
            hbnal(Nhb0al,2)=hbn(i,2)
            mptch(k1,1)=mptch(k1,1)+1
            mptch(k1,2)=1   !!helix
         else                                                    !!sheet .and.   
            if(i<Nhbonds-1.and.hbn(i,1).eq.hbn(i+1,1))then
               if(selc(ress(hbn(i+2,1))).eq.k1)then
                  if(abs(ress(hbn(i+2,2))-ress(hbn(i+1,2))).eq.2)then
                     cycle
                  else
                     hbn(i+1,1)=0
                  endif
               endif
            endif
            np1=-k1
            np2=-k2
      !      if((np1<1.or.np2<1).and.(abs(ir1-ir2)<6.or.abs(ir1-ir2)>16))cycle   !! turn and others, 7Sep 2008, !
            
            mptch(np1+mah,1)=mptch(np1+mah,1)+1
            mptch(np1+mah,2)=-4    
            mptch(np2+mah,1)=mptch(np2+mah,1)+1
            mptch(np2+mah,2)=-4          
            Nhb0sh=Nhb0sh+1
            hbnsh(Nhb0sh,1)=hbn(i,1)
            hbnsh(Nhb0sh,2)=hbn(i,2)
!print*,"BteHbond",Nhb0sh,ress(hbnsh(nhb0sh,1)),ress(hbnsh(nhb0sh,2))
         endif
      enddo
!!!
do i=1,Nreshb0
   j=NHB0sh+i
  hbnsh(j,1)=reshbn(i,1)
  hbnsh(j,2)=reshbn(i,2)
  reshbn(i,1)=0
  reshbn(i,2)=0
enddo
do i=1,NHB0al
   j=j+1
   hbnsh(j,1)=hbnal(i,1)
   hbnsh(j,2)=hbnal(i,2)
enddo

nhb0=j
j=0
do i=1,nhb0-1
   jj=0
   i1=hbnsh(i,1)
   i2=hbnsh(i,2)
!print*,"befor 11",NHB0sh,i,nhb0,i1,i2
   if(i1*i2<1)cycle
   k1=seq(ress(i1),1)
   k2=seq(ress(i2),1)
   do k=i+1,nhb0
      if(hbnsh(k,1)<1)cycle
      if(ress(hbnsh(k,1)).eq.ress(i1).and.ress(hbnsh(k,2)).eq.ress(i2))then
         hbnsh(k,1)=0
         hbnsh(k,2)=0  
   !      goto 234
     ! elseif(hbnsh(k,1).eq.i1.or.hbnsh(k,2).eq.i2)then  !!the highest accuracy
     !    ir1=ress(hbnsh(k,1))
     !    ir2=ress(hbnsh(k,2))
     !    if(seq(ir1,1).eq.seq(ir2,1).and.k1.ne.k2)then            
     !       hbnsh(i,1)=0
     !       hbnsh(i,2)=0  
     !       goto 234
     !    else
     !       hbnsh(k,1)=0
     !       hbnsh(k,2)=0
     !    endif
      endif
   enddo
   j=j+1
   reshbn(j,1)=hbnsh(i,1)
   reshbn(j,2)=hbnsh(i,2)
234 continue
enddo
if(i.eq.nhb0.and.hbnsh(i,1)>0)then  !!the final one
   j=j+1
   reshbn(j,1)=hbnsh(i,1)
   reshbn(j,2)=hbnsh(i,2)
endif
if(j>0)Nreshb0=j
!print*,"2222r",Nhbonds,NHB0al,NHB0sh
!!sorting the list and decide the parellel type, combine with seq(i,1)
if(Nreshb0>1)then
   do i=1,Nreshb0
      arrin(i)=reshbn(i,1)
   enddo
   call sortindex(Nreshb0,arrin,selc)
   do i=1,Nreshb0
      j=selc(i)
      hbnsh(i,1)=reshbn(j,1)
      hbnsh(i,2)=reshbn(j,2)
 !     print*,"reshbn",hbnsh(i,1),hbnsh(i,2)
   enddo
   do i=1,Nreshb0
      reshbn(i,1)=hbnsh(i,1)
      reshbn(i,2)=hbnsh(i,2)
   enddo
!!assign sheet type to HB, use selc record, 0 coil, 1 helix, 2 an-sheet, 3 p-sheet
   ir1=0
   do i=1,Nreshb0
      reshbn(i,3)=0
      ir1=ress(reshbn(i,1))
      ir2=ress(reshbn(i,2))
!!!assign Hbond in which 2nd pitch
      if(seq(ir1,1).eq.2.or.seq(ir2,1).eq.2)then !!helix Hbond  !1and/or ????? 
         reshbn(i,3)=1
      elseif(seq(ir1,1).eq.4.and.seq(ir2,1).eq.4)then !!sheet
         if(i>1.and.i<Nreshb0)then
            km=ress(reshbn(i-1,1))-ir1
            kn=ress(reshbn(i-1,2))-ir2
            ii=ress(reshbn(i+1,1))-ir1
            jj=ress(reshbn(i+1,2))-ir2
            if(km-kn==0.or.ii-jj==0)then  !!p-sheet
               reshbn(i,3)=3  !!only affect the CA distance
            else !if(km+kn==0.or.ii+jj==0)then  !!anti-sheet
               reshbn(i,3)=2
            endif
         else
            reshbn(i,3)=2
         endif
      else
         if(abs(ir1-ir2)<4)reshbn(i,3)=1
      endif
71    continue
   enddo
endif  !!Nreshbn>0

!!smooth reshbn(i,3)
reshbn(1,3)=reshbn(2,3)
do i=2,Nreshb0-1
   if(reshbn(i,3)>=1)cycle
   if(reshbn(i,3).ne.reshbn(i-1,3).and.reshbn(i,3).ne.reshbn(i+1,3).and.reshbn(i-1,3).eq.reshbn(i+1,3))then
      if(abs(reshbn(i,2)-reshbn(i-1,2))<5)reshbn(i,3)=reshbn(i-1,3)
   endif
   if(reshbn(i-1,3).ge.2)then
      ii=ress(reshbn(i,1))-ress(reshbn(i-1,1))
      if(ii.eq.2)reshbn(i,3)=reshbn(i-1,3)
   endif
   if(reshbn(i+1,3).ge.2)then
      ii=ress(reshbn(i+1,1))-ress(reshbn(i,1))
      if(ii.eq.2)reshbn(i,3)=reshbn(i+1,3)
   endif
   if(i>2)then
      ii=ress(reshbn(i,1))-ress(reshbn(i-2,1))
      jj=ress(reshbn(i,2))-ress(reshbn(i-2,2))
      if(reshbn(i-2,3).eq.2.and.ii+jj.eq.0)reshbn(i,3)=2
      if(reshbn(i-2,3).eq.3.and.ii-jj.eq.0.and.abs(ress(reshbn(i,1))-ress(reshbn(i,2)))>5)reshbn(i,3)=3
   endif
   if(i<Nreshb0-1)then
      ii=ress(reshbn(i+2,1))-ress(reshbn(i,1))
      jj=ress(reshbn(i+2,2))-ress(reshbn(i,2))
      if(reshbn(i+2,3).eq.2.and.ii+jj.eq.0)reshbn(i,3)=2
      if(reshbn(i+2,3).eq.3.and.ii-jj.eq.0.and.abs(ress(reshbn(i,1))-ress(reshbn(i,2)))>5)reshbn(i,3)=3
   endif
enddo
reshbn(Nreshb0,3)=reshbn(Nreshb0-1,3)  !!add 18/9/2008


k=0
   do i=1,Nreshb0
      if(reshbn(i,1)<1)cycle
      ir1=ress(reshbn(i,1))
      ir2=ress(reshbn(i,2))
      k=k+1
      hbnsh(k,1)=reshbn(i,1)
      hbnsh(k,2)=reshbn(i,2)
      selc(k)=reshbn(i,3)
   enddo
   Nreshb0=k
   do i=1,Nreshb0
      reshbn(i,1)=hbnsh(i,1)
      reshbn(i,2)=hbnsh(i,2)
      reshbn(i,3)=selc(i)
   enddo

if(icontrol.eq.'6'.or.icontrol.eq.'66')then
print*,"begin to write PRHB2",Nreshb0
799 continue
open(11,file="PRHB2")
k=0
if(icontrol.eq."6")then
   do i=1,Nreshb0
      if(reshbn(i,1)<1)cycle
      ir1=ress(reshbn(i,1))
      ir2=ress(reshbn(i,2))
      if(abs(ir1-ir2)<2)cycle
      if(abs(ir1-ir2)<=3)reshbn(i,3)=0
      k=k+1
!      print*,"here??",reshbn(i,3),ir1,ir2,seq(ir1,1),seq(ir2,1)
516 format(I3,A2,I4,A2,I4,I3,2I3)
      write(11,516),k,"N",ir1,"O",ir2,reshbn(i,3),seq(ir1,1),seq(ir2,1)  !0 coil, 1 helix, 2 an-sheet, 3 p-sheet
   enddo
elseif(icontrol.eq."66")then
   j=0
   do i=1,Nhbonds
      if(atomty(hbn(i,1)).ne." N  ".or.atomty(hbn(i,2)).ne." O  ")cycle
      j=j+1
      ir1=ress(hbn(i,1))
      ir2=ress(hbn(i,2))  
      if(abs(ir1-ir2)<2)cycle
      if(abs(ir1-ir2)<=3)reshbn(i,3)=0
!      if(abs(ir1-ir2)<=3)cycle   
      k=0
      if(seq(ir1,1).eq.1.or.seq(ir2,1).eq.1)then !!helix Hbond
         k=1
      elseif(seq(ir1,1).eq.4.and.seq(ir2,1).eq.4)then !!sheet
!print*,"beta",ir1,ir2,seq(ir1,1),seq(ir2,1)
         if(i>1.and.i<Nhbonds)then
            km=ress(hbn(i-1,1))-ir1
            kn=ress(hbn(i-1,2))-ir2
            ii=ress(hbn(i+1,1))-ir1
            jj=ress(hbn(i+1,2))-ir2
            if(km-kn==0.or.ii-jj==0)then  !!p-sheet
               k=3  !!only affect the CA distance
            else !if(km+kn==0.or.ii+jj==0)then  !!anti-sheet
               k=2
            endif
         else
            k=2
         endif
      endif
!!k 0 coil 1 helix 2 anti-
517 format(I3,A2,I4,A2,I4,I3,2I3)
      write(11,517),j,"N",ir1,"O",ir2,k,seq(ir1,1),seq(ir2,1)
   enddo
endif
close(11)
endif
800 continue
end subroutine restr_stru

 function torsion_angle(tia1,tia2,tia3,tia4)   !atom series
   implicit none
   real torsion_angle
   real ax,ay,az,bx,by,bz,cx,cy,cz,r1,r2,r3
   real tab,tac,tbc,t1,ctbcx,ctbcy,ctbcz,ctbax,ctbay,ctbaz,t2
   integer tia1,tia2,tia3,tia4
   if(xx(tia1)>990.0.or.xx(tia2)>990.0.or.xx(tia3)>990.0.or.xx(tia4)>990.0)then  !!this should not use for compare chi1
      torsion_angle=10.0
      goto 33
   endif
    ax=xx(tia2)-xx(tia1)
    ay=yy(tia2)-yy(tia1)
    az=zz(tia2)-zz(tia1)
    bx=xx(tia3)-xx(tia2)
    by=yy(tia3)-yy(tia2)
    bz=zz(tia3)-zz(tia2)
    cx=xx(tia4)-xx(tia3)
    cy=yy(tia4)-yy(tia3)
    cz=zz(tia4)-zz(tia3)
    r1=sqrt(ax*ax+ay*ay+az*az)
    r2=sqrt(bx*bx+by*by+bz*bz)
    r3=sqrt(cx*cx+cy*cy+cz*cz)
    if(r1*r2*r3<0.001)then
       torsion_angle=0.0
       goto 33
    endif
    ax=ax/r1
    ay=ay/r1
    az=az/r1
    bx=bx/r2
    by=by/r2
    bz=bz/r2
    cx=cx/r3
    cy=cy/r3
    cz=cz/r3
    tab=ax*bx+ay*by+az*bz  !dot product
    tac=ax*cx+ay*cy+az*cz
    tbc=bx*cx+by*cy+bz*cz
    t1=tab*tbc-tac
    ctbcx=by*cz-bz*cy      !cross product  bxc
    ctbcy=bz*cx-bx*cz
    ctbcz=bx*cy-by*cx
    ctbax=by*az-bz*ay      !cross product bxa
    ctbay=bz*ax-bx*az
    ctbaz=bx*ay-by*ax
    t2=ax*ctbcx+ay*ctbcy+az*ctbcz  !last term
       torsion_angle=acos(t1/sqrt(t1*t1+t2*t2))   !!0~pi
!!judge the r2xr3 angle with r1, if acute (-180,0),else(-180,0)
    if(t2<0)torsion_angle=-1.0*torsion_angle    !-pi~0
33       return
  end function torsion_angle

!this sub for sort, output in increase series
!     puts into a the permutation vector which sorts v into increasing order. 
SUBROUTINE sort(RA,N)
  implicit none
      integer N,ir,L,i,j
      real RA(N)
      real rra

      L=N/2+1
      IR=N
10    IF (L.GT.1) THEN
        L=L-1
        RRA=RA(L)
      ELSE
        RRA=RA(IR)
        RA(IR)=RA(1)
        IR=IR-1
        IF (IR.EQ.1) THEN
          RA(1)=RRA
          RETURN
        END IF
      END IF
      I=L
      J=L+L
20    IF (J.LE.IR) THEN
        IF (J.LT.IR) THEN
          IF (RA(J).LT.RA(J+1)) J=J+1
        END IF
        IF (RRA.LT.RA(J)) THEN
          RA(I)=RA(J)
          I=J
          J=J+J
        ELSE
          J=IR+1
        END IF
        GO TO 20
      END IF
      RA(I)=RRA
      GO TO 10
END subroutine sort 
!!this subroutine to sort array a and return accerating index after sorting
!!Note the input array will change value
SUBROUTINE sortindex(n,arrin,indx)
   implicit none
   integer i,j,k,ir,indxt,n
   real arrin(n)
   integer indx(n)
   real q
   do j=1,n
      indx(j)=j
   end do
   k=n/2+1
   ir=n
   do
      if(k>1)then
         k=k-1
         indxt=indx(k)
         q=arrin(indxt)
      else
         indxt=indx(ir)
         q=arrin(indxt)
         indx(ir)=indx(1)
         ir=ir-1
         if(ir==1) then
            indx(1)=indxt
            return
         endif
      endif
      i=k
      j=k+k
      do while(j<=ir) 
         if(j<ir) then
            if(arrin(indx(j))<arrin(indx(j+1)))j=j+1
         endif
         if(q<arrin(indx(j))) then
            indx(i)=indx(j)
            i=j
            j=j+j
         else
            j=ir+1
         endif
      end do
      indx(i)=indxt
   end do
 END SUBROUTINE sortindex

  subroutine rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,an) !rotate (x,y,z) with origin (ax,ay,az) around axis (xi,yi,zi) an
    implicit none
    real x,y,z,ax,ay,az,xi,yi,zi,an
    real r,r12,r23,r13,rcor
    real xr,yr,zr
    real x0,y0,z0    !!only use in test
    integer i,j,k
r=sqrt(xi*xi+yi*yi+zi*zi)
if(r<0.1)goto 101
x0=x
y0=y
x0=z
xi=xi/r
yi=yi/r
zi=zi/r
r12=xi*xi+yi*yi
r23=yi*yi+zi*zi
r13=xi*xi+zi*zi
rcor=xi*x+yi*y+zi*z
xr=ax*r23+xi*(-ay*yi-az*zi+rcor)+((x-ax)*r23+xi*(ay*yi+az*zi-yi*y-zi*z))*cos(an)+(ay*zi-az*yi-zi*y+yi*z)*sin(an)
yr=ay*r13+yi*(-ax*xi-az*zi+rcor)+((y-ay)*r13+yi*(ax*xi+az*zi-xi*x-zi*z))*cos(an)+(-ax*zi+az*xi+zi*x-xi*z)*sin(an)
zr=az*r12+zi*(-ax*xi-ay*yi+rcor)+((z-az)*r12+zi*(ax*xi+ay*yi-xi*x-yi*y))*cos(an)+(ax*yi-ay*xi-yi*x+xi*y)*sin(an)
x=xr
y=yr
z=zr
!if(abs(x)+abs(y)+abs(z)<0.001)then
!   print*,"warning",xi,yi,zi,r,x,y,z,an,x0,y0,z0
!endif
101 continue
  end subroutine rotate_matrix2

!!!this sub rotate a dehedral angle
!!!give series a-b-c-(d), the d ccordinate will be produce and a-b-c--(d), totate dhang to d, 
!!then adjust bond angle b-c-d to bcang, and rbn the bond length of c-d
  subroutine dihe_rotate(ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz,dhang,bcang) !rotate (x,y,z) with origin (ax,ay,az) around axis (xi,yi,zi) an
    implicit none
    real ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz,dhang,bcang
    real xii,yii,zii,vx1,vy1,vz1,vx2,vy2,vz2
    vx1=ax-bx
    vy1=ay-by
    vz1=az-bz
    vx2=bx-cx
    vy2=by-cy
    vz2=bz-cz    
    call rotate_matrix2(vx1,vy1,vz1,0.0,0.0,0.0,vx2,vy2,vz2,dhang)
   if(abs(vx1)+abs(vy1)+abs(vz1)<0.01)then
      print*,"warning dihe rotate",vx1,vy1,vz1
      vx1=vx1*cos(dhang)-vy1*sin(dhang)
      vy1=vx1*sin(dhang)+vy1*cos(dhang)  !!z keep fixed
!      goto 201
   endif 
    xii=vy2*vz1-vz2*vy1   !!cross product
    yii=vz2*vx1-vx2*vz1
    zii=vx2*vy1-vy2*vx1
    call rotate_matrix2(vx2,vy2,vz2,0.0,0.0,0.0,xii,yii,zii,bcang)
   if(abs(xii)+abs(yii)+abs(zii)<0.01)then
      print*,"warning dihe rotate22",xii,yii,zii,vx1,vy1,vz1,vx2,vy2,vz2,dhang,bcang
      dx=cx+bx-ax
      dy=cy+by-ay
      dz=cz+bz-az
      goto 201
   endif 
    dx=vx2+cx
    dy=vy2+cy
    dz=vz2+cz
201 continue
  end subroutine dihe_rotate
!!this subroutine use relaxmethod to solve the 3x3 matrix
!!B input and output
subroutine gaussj(A,B)
implicit none
integer,parameter::N=3
real A(N,N),B(N)
integer IPIV(N),INDXR(N),INDXC(N)
INTEGER np,J,I,IROW,ICOL,K,L,LL
real big,dum,pivinv
   DO J=1,N
      IPIV(J)=0
   enddo
   DO I=1,N
      BIG=0.
      DO J=1,N
         IF(IPIV(J).NE.1)THEN
            DO K=1,N
               IF(IPIV(K).EQ.0)THEN
                  IF(ABS(A(J,K)).GE.BIG)THEN
                     BIG=ABS(A(J,K))
                     IROW=J
                     ICOL=K
                  ENDIF
               ELSEIF(IPIV(K).GT.1)THEN
!                  print*,"Warning!!! singular maxtrix in GaussJ"                  
                  GO TO 999
               ENDIF
            enddo
         ENDIF
    enddo
    IPIV(ICOL)=IPIV(ICOL)+1
    IF(IROW.NE.ICOL)THEN
       DO L=1,N
          DUM=A(IROW,L)
          A(IROW,L)=A(ICOL,L)
          A(ICOL,L)=DUM
       enddo
       DUM=B(IROW)
       B(IROW)=B(ICOL)
       B(ICOL)=DUM
    ENDIF
    INDXR(I)=IROW
    INDXC(I)=ICOL
    IF(A(ICOL,ICOL).EQ.0.)THEN
!       print*,"Warning!!singular maxtrix GaussJ 0000"
 !      print*,a(1,1),a(1,2),a(1,3),b(1)
 !      print*,a(2,1),a(2,2),a(2,3),b(2)
 !     print*,a(3,1),a(3,2),a(3,3),b(3)
       b(1)=0
       b(2)=0
       b(3)=0
       GO TO 999
    ENDIF
    PIVINV=1.0/A(ICOL,ICOL)
    A(ICOL,ICOL)=1.0
    DO L=1,N
       A(ICOL,L)=A(ICOL,L)*PIVINV
    endDO
    B(ICOL)=B(ICOL)*PIVINV
    DO LL=1,N
       IF(LL.NE.ICOL)THEN
          DUM=A(LL,ICOL)
          A(LL,ICOL)=0.0
          DO L=1,N
             A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
          enddo
          B(LL)=B(LL)-B(ICOL)*DUM
       ENDIF
    enddo
 enddo
 DO L=N,1,-1
    IF(INDXR(L).NE.INDXC(L))THEN
       DO K=1,N
          DUM=A(K,INDXR(L))
          A(K,INDXR(L))=A(K,INDXC(L))
          A(K,INDXC(L))=DUM
       enddo
    ENDIF
 enddo
999 RETURN
END subroutine gaussj

!!random length 10^17, (0,1)
	FUNCTION RAND(ISEED) 
	integer,PARAMETER::IM1=2147483563,IM2=2147483399,IR2=3791,NTAB=32
        integer,parameter::IMM1=IM1-1,IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211
        integer,parameter::NDIV=1+IMM1/NTAB
        real,parameter::AM=1./IM1,EPS=1.2E-7,RNMX=1-EPS
	REAL RAND
	INTEGER ISEED2,J,K,IV(NTAB),IY,iseed
	SAVE IV,IY,ISEED2
	DATA ISEED2/123456789/,IV/NTAB*0/,IY/0/
	IF(ISEED.LE.0)THEN
	   ISEED=MAX(-ISEED,1)
	   ISEED2=ISEED
	   DO J=NTAB+8,1,-1
	      K=ISEED/IQ1
              ISEED=IA1*(ISEED-K*IQ1)-K*IR1
	   IF(ISEED.LT.0)ISEED=ISEED+IM1
	   IF(J.LE.NTAB)IV(J)=ISEED
         ENDDO
	   IY=IV(1)
	ENDIF
	K=ISEED/IQ1
	ISEED=IA1*(ISEED-K*IQ1)-K*IR1
	IF(ISEED.LT.0)ISEED=ISEED+IM1
	K=ISEED2/IQ2
	ISEED2=IA2*(ISEED2-K*IQ2)-K*IR2
	IF(ISEED2.LT.0)ISEED2=ISEED2+IM2
	J=1+IY/NDIV
	IY=IV(J)-ISEED2
	IV(J)=ISEED
	IF(IY.LT.1)IY=IY+IMM1
	RAND=MIN(AM*IY,RNMX)
	RETURN
      END FUNCTION RAND

!!this subroutien calculate the CA clash
subroutine calclash
implicit none
integer i,j
real r,rcla,rbr

if(icontrol.eq."5")then
   rcla=3.6
   rbr=4.1
else
   rcla=3.75
   rbr=3.9
endif

Ncaclash=0
Nbroken=0
eclash=0.0
do i=1,Nres-1
   ip1=ca(i,1)
   bx=xx(ip1)
   by=yy(ip1)
   bz=zz(ip1)
   do j=i+1,Nres
      ip2=ca(j,1)
      cx=xx(ip2)
      cy=yy(ip2)
      cz=zz(ip2)
      r=sqrt((bx-cx)*(bx-cx)+(by-cy)*(by-cy)+(bz-cz)*(bz-cz))
      if(r<rcla)then
         Ncaclash=Ncaclash+1
         eclash=eclash+0.5+(r-rcla)**2
!         print*,Ncaclash,i,j,r
      elseif(j-i.eq.1.and.r>rbr)then
         Nbroken=Nbroken+1
         eclash=eclash+0.5+(r-rbr)**2
      endif
   enddo
enddo
end subroutine calclash

!!try to reduce clash adn broken
subroutine move_clash(fk,nkchbr)
implicit none
real fk,fb,av,bv,cv,rclasht
integer i,j,k,ip1,ip2,nkchbr
do i=1,Nres
bvv2(i,1)=0.0
bvv2(i,2)=0.0
bvv2(i,3)=0.0
numclash(i)=0
enddo
nkchbr=0
do i=1,nres-1 
   ip1=ca(i,1)
   do j=i+1,nres
      ip2=ca(j,1)
      ax=xx(ip1)-xx(ip2)
      ay=yy(ip1)-yy(ip2)
      az=zz(ip1)-zz(ip2)
      r=max(0.01,sqrt(ax*ax+ay*ay+az*az))
      if(j-i.eq.1.and.ca(i,2).eq.8.and.ca(j,2).eq.8)then  !PRO in turn
         rclasht=2.8
      else
         rclasht=rclash
      endif
      if(r<rclasht)then
         fb=fk*(rclash-r)/r
         bvv2(i,1)=bvv2(i,1)+ax*fb
         bvv2(i,2)=bvv2(i,2)+ay*fb
         bvv2(i,3)=bvv2(i,3)+az*fb
         bvv2(j,1)=bvv2(j,1)-ax*fb
         bvv2(j,2)=bvv2(j,2)-ay*fb
         bvv2(j,3)=bvv2(j,3)-az*fb  
         numclash(i)=numclash(i)+1
         numclash(j)=numclash(j)+1
         nkchbr=nkchbr+1         
      elseif(j-i.eq.1.and.r>rbroken)then
         fb=fk*(rbroken-r)/r
         bvv2(i,1)=bvv2(i,1)+ax*fb
         bvv2(i,2)=bvv2(i,2)+ay*fb
         bvv2(i,3)=bvv2(i,3)+az*fb
         bvv2(j,1)=bvv2(j,1)-ax*fb
         bvv2(j,2)=bvv2(j,2)-ay*fb
         bvv2(j,3)=bvv2(j,3)-az*fb  
         numclash(i)=numclash(i)+1
         numclash(j)=numclash(j)+1
         nkchbr=nkchbr+1 
      endif
   enddo
enddo

   do i=1,nres
      if(numclash(i)<1)cycle
      k=0
!96    continue
    !  av=2.0*rand(iseed)-1.0
    !  bv=2.0*rand(iseed)-1.0
    !  cv=2.0*rand(iseed)-1.0
      k=k+1
      ax=bvv2(i,1) !*av
      ay=bvv2(i,2) !*bv
      az=bvv2(i,3) !*cv
!      r=ax*bvv(i,1)+ay*bvv(i,2)+az*bvv(i,3)
!      if(r<0.and.k<100)goto 96   !!!to make in same direction

      if(numclash(i)<3)then  !!!only one direction
         ax=rand(iseed)*ax !bvv2(i,1)
         ay=rand(iseed)*ay !bvv2(i,2)
         az=rand(iseed)*az !bvv2(i,3)         
      endif
      ip=ca(i,1)
      r=sqrt(ax*ax+ay*ay+az*az)
      fb=1.0
      if(r>fk)fb=fk/r+fk
      if(hbvar(i)<1)then   !!can move
         xx(ip)=xx(ip)+ax*fb
         yy(ip)=yy(ip)+ay*fb
         zz(ip)=zz(ip)+az*fb
      elseif(hbvar(i).eq.2)then  !!keep fix as possible
         xx(ip)=xx(ip)+ax*fb*0.5
         yy(ip)=yy(ip)+ay*fb*0.5
         zz(ip)=zz(ip)+az*fb*0.5 
      else  !!keep fix as possible
         xx(ip)=xx(ip)+ax*fb*ffrozen !0.1
         yy(ip)=yy(ip)+ay*fb*ffrozen !0.1
         zz(ip)=zz(ip)+az*fb*ffrozen !0.1
      endif
   enddo
end subroutine move_clash

!!this subroutine calculation the CA RMSD energy
subroutine CARMSD
implicit none
integer j,i
   RMSD=0
   do j=1,Nres !natom
      i=ca(j,1)      
 !     RMSD=RMSD+(xx(i)-bbkxx(i))**2+(yy(i)-bbkyy(i))**2+(zz(i)-bbkzz(i))**2
      carmsdd(j)=(xx(i)-bbkxx(j))**2+(yy(i)-bbkyy(j))**2+(zz(i)-bbkzz(j))**2
      RMSD=RMSD+carmsdd(j)
!      write(*,"I4,8f8.3")j,carmsdd(j),RMSD,xx(i),yy(i),zz(i),bbkxx(j),bbkyy(j),bbkzz(j)
   enddo
   RMSD=sqrt(RMSD/Nres)
end subroutine CARMSD

!!the HBlist restraints
!!rotate the CN planar of two residues to make Hbond in the list
!!iflag=1, move CA iflash=2, no move CA
!!only adjust the Hbond except in Beta
subroutine HBrotate(iflag)
implicit none
integer i,j,ichoo,iflag,kg,kgg
if(iflag.eq.1)then
Nsuc=0
   do i=1,natom
      xx0(i)=xx(i)
      yy0(i)=yy(i)
      zz0(i)=zz(i)
   enddo
   call energy_CAL
   E0=energy 
   kg=0
   kgg=0
92 continue
!!this part is three kinds of movement
 !  ichoo=mod(labelcycle+kg,12)
   ichoo=mod(labelcycle+kg,12)
   if(ichoo<2)then
      call relax_Lmprot
      if(Mlog)then
         kg=kg+1
         goto 92
      endif
!      print*,"LMprot get one",kg
      ichoo=1
   elseif(ichoo<6)then
      call CaCarot !!!some error!!!!!!!!!!!!!!!!!!!!!!!!
      ichoo=2
   else
      if(kgg<1)then  !!?????
         call glo_move(0)
         kgg=1
      else
         call glo_move(1)
      endif
      ichoo=3
   endif

   relax(ichoo,2)=relax(ichoo,2)+1

!   print*,"movement",ichoo,kgg
!   do i=1,Nres
!      if(xx(ca(i,1))>200.or.xx(ca(i,1))<0)then
!         print*,"err",i,ires0,ires1,xx(ca(i,1)),ichoo
!      endif
!   enddo
!!!Metropolis judgement
!   write(*,"A10,3f10.3,2I4,3f9.3,f7.3")">>Metropo",E0,EE,DE,Ncaclash,Nbroken,cacontactk*CAcontact,cashortk*CAdist,calongk*CAdistL,RMSD !rclash,rbroken
   call energy_CAL
   EE=energy
!print*,"Energy done",EE,e0
   kg=kg+1
   DE=EE-E0     
   if((DE<=0.or.(DE>0.and.DE<10.and.rand(iseed)<exp(-DE))))then !.and.Ncaclash+2*Nbroken<=Ncaclash0)then
      relax(ichoo,3)=relax(ichoo,3)+1
      Nsuc=Nsuc+1
!      write(*,"A3,I3,3f8.3,I5,4I4,4f14.2"),"cy",ichoo,RMSD,RMSDcut,RMSD0,labelcycle,Nhbonds,Nhbhit,Ncaclash,Nbroken,ee,e0,eg0,etasser
      E0=EE
      RMSDcut=RMSD      
      NHB0=NHbonds
      mmhb0=Nhbhit
      eg0=e0      
      do j=1,natom
         bkxx(j)=xx(j)
         bkyy(j)=yy(j)
         bkzz(j)=zz(j)
      enddo
      if(templatelog.and.icontrol.eq."4")then
         do i=1,Nres
            CArmsdd0(i)=CArmsdd(i)
            if(CArmsdd0(i)<0.01)then
               hbvar(i)=1
               k=k+1
            else
               hbvar(i)=0
            endif
         enddo
      endif
      goto 93
   else
      do i=1,natom
         xx(i)=xx0(i)
         yy(i)=yy0(i)
         zz(i)=zz0(i)
      enddo
   endif
   if(kg<500)goto 92

93 continue
 !  if(Nsuc>0)call BBADD(1) !!built C N O
!   print*,"BBADD(1) donne"
else  !!iflag==1
!!!adjust the phi-psi plane  ***************************************
do i=1,Nreshb0
   ir1=ress(reshbn(i,1))
   ir2=ress(reshbn(i,2))
   ip1=ca(ir1,1)
   ip2=ca(ir2,1)
  !!adjust donor plane
   if(ir1>1.and.hbvar(ir1-1)<2)then
      ip3=ca(ir1-1,1)
      angg=torsion_angle(ip1-1,ip1,ip3,ip2)  !!N-CA-CA(-1)-CA(Hbond)
      if(abs(angg)>0.001)then
         xi=xx(ip3)-xx(ip1)
         yi=yy(ip3)-yy(ip1)
         zi=zz(ip3)-zz(ip1)
         ax=xx(ip1)
         ay=yy(ip1)
         az=zz(ip1)
         do ii=1,4 !ip3+1,ip1-2 !C(-1) to all, except N
            if(ii.eq.1)then
               j=ip3+1
            elseif(ii.eq.2)then
               j=ip3+2 !O
            elseif(ii.eq.3)then
               j=ip1-1  !N
            else
               if(ca(ir1,2).eq.8)cycle
               j=ip1+hra(ca(ir1,2))-1 !H
               if(ir1.eq.Nres)j=j+1
            endif
            x=xx(j)
            y=yy(j)
            z=zz(j)
            call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)
            xx(j)=x
            yy(j)=y
            zz(j)=z
!print*,"rotate2 don",atomty(j),j,angg
         enddo
      endif
   endif
 !!adjust acceptor plane
   if(ir2<Nres.and.hbvar(ir2)<2)then
      ip3=ca(ir2+1,1)
      angg=torsion_angle(ip2+1,ip2,ip3,ip1) !!C-CA-CA(+1)-CA(Hbond)
      if(abs(angg)>0.001)then
         xi=xx(ip3)-xx(ip2)
         yi=yy(ip3)-yy(ip2)
         zi=zz(ip3)-zz(ip2)
         ax=xx(ip2)
         ay=yy(ip2)
         az=zz(ip2)
         x=xx(ip3-1)
         y=yy(ip3-1)
         z=zz(ip3-1)
         call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)
         xx(ip3-1)=x
         yy(ip3-1)=y
         zz(ip3-1)=z
         do ii=1,4 !ip2+1,ip3-2
            if(ii.eq.1)then
               j=ip2+1
            elseif(ii.eq.2)then
               j=ip2+2 !O
            elseif(ii.eq.3)then
               j=ip3-1  !N
            else
               if(ca(ir2+1,2).eq.8)cycle
               j=ip3+hra(ca(ir2+1,2))-1 !H
               if(ir2+1.eq.Nres)j=j+1
            endif
            x=xx(j)
            y=yy(j)
            z=zz(j)
            call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)
            xx(j)=x
            yy(j)=y
            zz(j)=z
!print*,"rotate2 acc",atomty(j),j,angg,ir2+1,resd(j)
         enddo
      endif
   endif
enddo
endif  !!iflag==2

end subroutine HBrotate

!!this subroutine move globularly through remove clash, HB to template and so on
!kiy=0, calculate all
!kiy=1, ignore bvv calculation
subroutine glo_move(kiy)
implicit none
real fb,fk13,fk14,rL13,rU13,rL14,rU14
real r,r0,rd,fk,fkk,ftmp
real r01,r11,r22,av,bv,cv
integer iflag,i11,i22,i,j,k,ip1,ip2,ip,ii,i1,i2
integer kg,Mcah,nkchbr,kiy,ires0,ires1
   relax(3,1)=relax(3,1)+1
   fk=faredu*faredu
   fkk=fk !2.0*fk
   ftmp=1.0 !0.1*fk  !!restraints to template
if(kiy>0)goto 92
   do i=1,Nres
      bvv(i,1)=0.0
      bvv(i,2)=0.0
      bvv(i,3)=0.0
   enddo
do i=1,Nreshb0
   i1=reshbn(i,1)
   i2=reshbn(i,2)
   ir1=ress(i1)
   ir2=ress(i2)
   ip1=ca(ir1,1)
   ip2=ca(ir2,1)
   do j=1,Nhbonds        
      if(hbn(j,1).eq.i1.and.hbn(j,2).eq.i2)then
         goto 91
      endif
   enddo
!!this HBbond need change to form
   if(reshbn(i,3).eq.1)then !!helix
      if(abs(ir1-ir2).eq.4)then !!alpha
         r0=6.15
         rd=0.53
      else
         r0=5.25
         rd=0.6
      endif
   elseif(reshbn(i,3).eq.2)then  !!anti-p sheet
      r0=5.2
      rd=0.6
   elseif(reshbn(i,3).eq.3)then  !!parellel
      r0=6.2
      rd=0.5
   else  !!coil or turn
      r0=5.5
      rd=0.8
   endif 
   ax=xx(ip1)-xx(ip2)  !!donor-acceptor
   ay=yy(ip1)-yy(ip2)
   az=zz(ip1)-zz(ip2)
   r=sqrt(ax**2+ay**2+az**2)
   if(r<0.001)cycle  !!do not treat here
   if(abs(r-r0)>rd)then
      if(r>r0)then
         fb=fkk*(r-(r0+0.7*rd))/r
      else
         fb=fkk*(r-(r0-0.7*rd))/r
      endif
      bvv(ir1,1)=bvv(ir1,1)-ax*fb
      bvv(ir1,2)=bvv(ir1,2)-ay*fb
      bvv(ir1,3)=bvv(ir1,3)-az*fb
      bvv(ir2,1)=bvv(ir2,1)+ax*fb
      bvv(ir2,2)=bvv(ir2,2)+ay*fb
      bvv(ir2,3)=bvv(ir2,3)+az*fb
   endif
91 continue
enddo  !!all Hbonds

!!move 2nd neighbouring residues, this is not good
do i=1,Nres
   if(seq(i,1).eq.1)cycle  !!no restraint for coil, must before mkrestraints
   ip1=ca(i,1)
   if(seq(i,1).eq.2)then   !!helix (give 13, 14 and 15 restraints)
      rL13=5.0
      rU13=6.1
      rL14=4.5
      rU14=6.1
      fk13=0.2*fk
      fk14=0.2*fk
   elseif(seq(i,1).eq.4)then   !!sheet
      rL13=5.8
      rU13=7.35
      rL14=6.5
      rU14=10.8
      fk13=0.1*fk
      fk14=0.1*fk
   else
      rL13=4.85
      rU13=7.6
      rL14=4.45
      rU14=10.95
      fk13=0.1*fk
      fk14=0.1*fk
   endif
   if(i+2<=Nres)then
      ip2=ca(i+2,1)
      j=i+2
   else
      ip2=ca(i-2,1)
      j=i-2
   endif
   ax=xx(ip1)-xx(ip2)
   ay=yy(ip1)-yy(ip2)
   az=zz(ip1)-zz(ip2)
   r=sqrt(ax*ax+ay*ay+az*az)
   r01=r
   if(r>=rL13.and.r<=rU13)goto 102
   if(r<rL13.and.r>0.1)then
      fb=(rL13-r)/r*fk13
   elseif(r>rU13)then
      fb=(rU13-r)/r*fk13
   endif
   bvv(i,1)=bvv(i,1)+ax*fb  !!non-symetric
   bvv(i,2)=bvv(i,2)+ay*fb
   bvv(i,3)=bvv(i,3)+az*fb
   bvv(j,1)=bvv(j,1)-ax*fb
   bvv(j,2)=bvv(j,2)-ay*fb
   bvv(j,3)=bvv(j,3)-az*fb
102 continue
   if(i-3>=1)then
      ip2=ca(i+3,1)
      j=i-3
   else
      ip2=ca(i-3,1)
      j=i+3
   endif
   ax=xx(ip1)-xx(ip2)
   ay=yy(ip1)-yy(ip2)
   az=zz(ip1)-zz(ip2)
   r=sqrt(ax*ax+ay*ay+az*az)
   if(r>=rL13.and.r<=rU13)goto 103
   fb=0.0
   if(r<rL14.and.r>0.1)then
      fb=(rL14-r)/r*fk14
   elseif(r>rU14)then
      fb=(rU14-r)/r*fk14
   endif
   bvv(i,1)=bvv(i,1)+ax*fb*1.4
   bvv(i,2)=bvv(i,2)+ay*fb*1.4
   bvv(i,3)=bvv(i,3)+az*fb*1.4
   bvv(j,1)=bvv(j,1)-ax*fb*0.6
   bvv(j,2)=bvv(j,2)-ay*fb*0.6
   bvv(j,3)=bvv(j,3)-az*fb*0.6 
103 continue
enddo   !!1for 2nd restraints

!!!vectors to template *************************adjustable
if(templatelog.and.RMSD>0.001)then
do i=1,nres 
   ip1=ca(i,1)
   ax=bbkxx(i)-xx(ip1) !+bvv(i,1))
   ay=bbkyy(i)-yy(ip1) !+bvv(i,2))
   az=bbkzz(i)-zz(ip1) !+bvv(i,3))
   r=sqrt(ax*ax+ay*ay+az*az)
   if(r<0.01)cycle
   bvv(i,1)=bvv(i,1)+ax*ftmp
   bvv(i,2)=bvv(i,2)+ay*ftmp
   bvv(i,3)=bvv(i,3)+az*ftmp
!print*,i,bvv(i,1),bvv(i,2),bvv(i,3)
enddo
endif

!!tasser restraints
if(.false..and.tarest)then
   do i=1,NcombCA   !!combCA.dat
      ir1=combsca(i,1)
      ir1=combsca(i,2)
      ip1=ca(ir1,1)
      ip2=ca(ir2,1)
      ax=xx(ip1)-xx(ip2)
      ay=yy(ip1)-yy(ip2)
      az=zz(ip1)-zz(ip2)
      r=sqrt(ax*ax+ay*ay+az*az)
      if(r>6.0)then !cacut(j1,j2))then
         fb=0.2*cacontactk*(r-5.8)/r
         bvv(ir1,1)=bvv(ir1,1)-fb*ax
         bvv(ir1,2)=bvv(ir1,2)-fb*ay
         bvv(ir1,3)=bvv(ir1,3)-fb*az
         bvv(ir2,1)=bvv(ir2,1)+fb*ax
         bvv(ir2,2)=bvv(ir2,2)+fb*ay
         bvv(ir2,3)=bvv(ir2,3)+fb*az
      endif
   enddo
   do i=1,0 !Ndist
      ir1=rsd(i,1)
      ir1=rsd(i,2)
      ip1=ca(ir1,1)
      ip2=ca(ir2,1)
      ax=xx(ip1)-xx(ip2)
      ay=yy(ip1)-yy(ip2)
      az=zz(ip1)-zz(ip2)
      r=sqrt(ax*ax+ay*ay+az*az)
      if(r>0.01.and.abs(r-drsd(i,1))>drsd(i,2))then !cacut(j1,j2))then
         if(r>drsd(i,1))then
            fb=0.001*cashortk*(r-drsd(i,1)-drsd(i,2))/r
         else
            fb=0.001*cashortk*(r-drsd(i,1)+drsd(i,2))/r
         endif
         bvv(ir1,1)=bvv(ir1,1)-fb*ax
         bvv(ir1,2)=bvv(ir1,2)-fb*ay
         bvv(ir1,3)=bvv(ir1,3)-fb*az
         bvv(ir2,1)=bvv(ir2,1)+fb*ax
         bvv(ir2,2)=bvv(ir2,2)+fb*ay
         bvv(ir2,3)=bvv(ir2,3)+fb*az
      endif
   enddo
   j=mod(labelcycle,4)
   do i=1+j,NdistL,3
      ir1=rsd(i,1)
      ir1=rsd(i,2)
      ip1=ca(ir1,1)
      ip2=ca(ir2,1)
      ax=xx(ip1)-xx(ip2)
      ay=yy(ip1)-yy(ip2)
      az=zz(ip1)-zz(ip2)
      r=sqrt(ax*ax+ay*ay+az*az)
      if(r>0.01.and.abs(r-drsdL(i))>1.0)then
         fb=0.1*calongk*(r-drsdL(i))/r
         bvv(ir1,1)=bvv(ir1,1)-fb*ax
         bvv(ir1,2)=bvv(ir1,2)-fb*ay
         bvv(ir1,3)=bvv(ir1,3)-fb*az
         bvv(ir2,1)=bvv(ir2,1)+fb*ax
         bvv(ir2,2)=bvv(ir2,2)+fb*ay
         bvv(ir2,3)=bvv(ir2,3)+fb*az
      endif
   enddo
endif
!!select movement ******************************************************
92 continue
    ires0=int(rand(iseed)*Nres-10)
    if(ires0<1)ires0=1
    ires1=ires0+int(rand(iseed)*Nres/2)+10
    if(ires1>Nres)ires1=Nres

  !  ires0=1
  !  ires1=Nres
   do i=ires0,ires1 !1,nres
   !   if((.not.tasserlog).and.abs(bvv(i,1))+abs(bvv(i,2))+abs(bvv(i,3))<0.001)cycle
  !    if(abs(bvv(i,1))+abs(bvv(i,2))+abs(bvv(i,3))<0.001)cycle
      
     ! if(rand(iseed)<0.3)cycle !!move half of the residues
  !    av=2.0*rand(iseed)-1.0
  !    bv=2.0*rand(iseed)-1.0
  !    cv=2.0*rand(iseed)-1.0
       av=rand(iseed)
       bv=rand(iseed)
       cv=rand(iseed)
   !    if(templatelog)then
   !       r=sqrt(CArmsdd0(i))+0.01
   !       ax=(2.0*av-1.0)*r !bvv(i,1)*av
   !       ay=(2.0*bv-1.0)*r !bvv(i,2)*bv
   !       az=(2.0*cv-1.0)*r !bvv(i,3)*cv
       if(labelcycle<20.or.(labelcycle>40.and.labelcycle<60).or.labelcycle>70)then
          ax=bvv(i,1)*av
          ay=bvv(i,2)*bv
          az=bvv(i,3)*cv
       else
          ax=2.0*av-1.0
          ay=2.0*bv-1.0
          az=2.0*cv-1.0
       endif
       if(abs(ax)+abs(ay)+abs(az)<0.001)cycle
 !    print*,"mvd",i,ax,ay,az
  !    r=ax*bvv(i,1)+ay*bvv(i,2)+az*bvv(i,3)
      ip=ca(i,1)
      r=sqrt(ax*ax+ay*ay+az*az)
      if(r<0.0001)cycle
      fb=1.0
      if(r>fk)fb=fk/r+0.7*fk
!print*,"i",r,fk,fb
      if(hbvar(i)<1)then   !!can move
         xx(ip)=xx(ip)+ax*fb
         yy(ip)=yy(ip)+ay*fb
         zz(ip)=zz(ip)+az*fb
      elseif(hbvar(i).eq.2)then  !!galf flexible
         xx(ip)=xx(ip)+ax*fb*0.5
         yy(ip)=yy(ip)+ay*fb*0.5
         zz(ip)=zz(ip)+az*fb*0.5 
      else  !!keep fix as possible
         xx(ip)=xx(ip)+ax*fb*ffrozen !0.1
         yy(ip)=yy(ip)+ay*fb*ffrozen !0.1
         zz(ip)=zz(ip)+az*fb*ffrozen !0.1
      endif
   enddo
!remove clash ***********************************************

 !  do i=1,Nres
 !     ip=ca(i,1)
 !     if(xx(ip)>200.or.xx(ip)<0)then
 !        print*,"err00",i,hbvar(i),xx(ip),xx0(ip),kiy,bvv(i,1),fk !,fb !ax*fb,ay*fb,az*fb,fk,fb,r
  !    endif
 !  enddo
   call calclash
   
  if(Ncaclash>5.or.Nbroken>1)then
!print*,"remove clash",Ncaclash,Nbroken,fk
      Mcah=min(labelcycle/4+1,16)
      do j=1,Mcah
         if(j<=6)then
            fk=(faredu-0.006*j)*0.9
         elseif(j<=12)then
            fk=(faredu-0.006*j+0.165)**2
         else
            fk=(faredu-0.002*j)**3
         endif
 !        fk=0.6
         call move_clash(fk,nkchbr)
!         call calclash
!         if(Ncaclash<5.and.Nbroken<2)goto 69
         if(nkchbr<10)goto 69
      enddo
   endif
69 continue
call BBADD(1)  !!!for Hbond count
end subroutine glo_move
!!rotate side chain around Ca-Ca axis rotate phi-planar (psi)
!!rotate together with both the two residue side chain and HA
!!iflagcaca=0 phi (ca i--ca i-idd); =1 psi (ca i -->i+idd)
!!when ifgcac=0 phi, ires0=pro, side chain are forbid;ifgcaca=1, psim ires=pro are forbid
!!18 Oct, change icaca0 -1 to +1
subroutine CaCarot !(Ksuc(irep))
implicit none
real an
real ax,ay,az,x,y,z,xi,yi,zi
integer i,j,ires0,ires1,ip1,ip2
integer ichild,ifgcaca

relax(2,1)=relax(2,1)+1
ires=mod(int(rand(iseed)*7621),Nres)+1
ichild=mod(int(rand(iseed)*9533),Nchild)+1
ifgcaca=mod(int(rand(ISEED)*62483),2)
an=pi*(rand(iseed)*2.0-1.0)
if(abs(an)<0.001)goto 122
  if(ifgcaca==0)then     !!move phi planar, ires0 move C=O, ires1 move to N-term
     ires1=ires
     ires0=max(ires-ichild,1)
     if(ires0>=ires1)goto 122   !!this can not move
  elseif(ifgcaca==1)then    !!rotate psi planar around i-->i+idd Ca , move to C-term
     ires1=min(Nres,ires+ichild)
     ires0=ires
     if(ires0>=ires1)goto 122   !!this can not move
  endif

   ip1=ca(ires0,1)
   ip2=ca(ires1,1)
   ax=xx(ip1)   !! middle point as origin, i-->i-idd
   ay=yy(ip1)
   az=zz(ip1)
   xi=xx(ip1)-xx(ip2)
   yi=yy(ip1)-yy(ip2)
   zi=zz(ip1)-zz(ip2)    
   if(abs(xi)+abs(yi)+abs(zi)<0.01)then
      print*,"warning,cacarot no direction defined",ires,resd(iatom),ifgcaca,ichild,ires0,ires1,resd(ca(ires0,1))
      goto 122
   endif
   do i=ires0,ires1
      ip1=ca(i,1)
      do j=-1,1  !!O and H will added
         if(i.eq.ires0.and.j<=0.or.i.eq.ires1.and.j>=0)cycle
         ip2=ip1+j
         x=xx(ip2)
         y=yy(ip2)
         z=zz(ip2)
         call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,an)  !H on N in i residue
!write(*,"A3,I4,A5,6f8.3"),"Ca",ip2,atomty(ip2),xx(ip2),x,yy(ip2),y,zz(ip2),z
         xx(ip2)=x
         yy(ip2)=y
         zz(ip2)=z 
      enddo
   enddo
122 continue 
!print*,"Caa",an,ires0,ires1,relax(2,1),relax(2,2),relax(2,3)
call ADD_HO
end subroutine CaCarot

!!this subroutine to relax us LMprot method:: Silva Biophys. J. 87, 2004, 1567-1577.
!!LLmp=7, always plus
!1. move backbone, 400 times try, if sucess
!!1-2 bond length Ca-C 1.49(2.2201) C-N 1.345(1.809025) N-Ca 1.43(2.0449)   
!!1-3 UB length  Ca-C-N 2.412 (5.817530455) C-N-Ca 2.404 (5.777275) N-Ca-C 2.348(5.510912783)
!!1-4 dihedral  Ca-C-N-Ca 3.81(14.5161,helix), 3.83 (14.6689)sheet) 3.79 (14.3641,coil) 
!!PRO CD-N 1.455(2.117025), CD-N-C 2.388 (5.702946116), Ca-C-N-Ca 2.95 (8.7025,cis-PRO, dihedral 90o, phi)
!!omega 0(cis-PRO) or 180o
!!residue 2-Nres-1
subroutine relax_Lmprot
implicit none
!!!calculate the old restraints, if the new one is better than old one, accept together
real,dimension(5,10):: ddcaold   !!!LLMM+1
real,dimension(3,10):: ddnold,ddcold
integer KATT,kc,MSUC,katcy,LLMM
integer i,ir,j,jk,ittat,iresin
integer ipca,ip2,ik,nsd,ip0,k
real xn,yn,zn,x,y,z,ax,ay,az
real dx,dy,dz,rb,r1,dd,an

relax(1,1)=relax(1,1)+1
Mlog=.true.
LLMM=mod(int(rand(iseed)*141707),5)+1
ires0=mod(int(rand(iseed)*7621),Nres-2)+1
ires1=min(ires0+LLMM,Nres-1)
if(ires0>ires1)goto 122

jk=1  !1atom series
do ires=ires0,ires1   !old positions
ipca=ca(ires,1)
iresin=ca(ires,2)
   i=ipca-1
do j=1,2   !N
   if(j.eq.1)then
      ip2=ca(ires-1,1)  
   elseif(j.eq.2)then
      ip2=ipca+1
   endif
   ddNold(j,jk)=(xx0(i)-xx0(ip2))**2+(yy0(i)-yy0(ip2))**2+(zz0(i)-zz0(ip2))**2
enddo
   i=ipca
do j=1,4
   if(j.eq.1)then
      ip2=ca(ires-1,1)  !!Ca
   elseif(j.eq.2)then
      ip2=ca(ires+1,1)
   elseif(j.eq.3)then
      ip2=ca(ires+1,1)-1
   elseif(j.eq.4)then
      ip2=ca(ires-1,1)+1
   endif
   ddCAold(j,jk)=(xx0(i)-xx0(ip2))**2+(yy0(i)-yy0(ip2))**2+(zz0(i)-zz0(ip2))**2
enddo
   i=ipca+1
do j=1,2
   if(j.eq.1)then
      ip2=ipca-1 !!C
   elseif(j.eq.2)then
      ip2=ca(ires+1,1)
   endif
   ddCold(j,jk)=(xx0(i)-xx0(ip2))**2+(yy0(i)-yy0(ip2))**2+(zz0(i)-zz0(ip2))**2
enddo
jk=jk+1
enddo  !!!old finish

jk=1
do katcy=1,3
   MSUC=0  !!sucessful move
   KATT=0  !!total attempts
   do ires=ires0,ires1
      ipca=ca(ires,1)
!!!!MOVE bb atoms
!if(ires>ires0)then  !!move N except first N
      i=ipca-1   !!N change psi 
601 continue
      dx=dlmp*(rand(iseed)*2.0-1.0)  !!interval is 1.0
      dy=dlmp*(rand(iseed)*2.0-1.0)
      dz=dlmp*(rand(iseed)*2.0-1.0)
      KATT=KATT+1
      if(KATT>NLMAT)goto 131  !!end of subroutine
      xn=bkxx(i)+dx
      yn=bkyy(i)+dy
      zn=bkzz(i)+dz  !!new position
      !write(*,'A4,6f8.3'),"fst0",xn,yn,zn,dx,dy,dz
      do j=1,3
         dd=ddN(j)
         if(j.eq.1)then
            ip2=ca(ires-1,1)  
         elseif(j.eq.2)then
            ip2=ca(ires,1)+1
         else
            ip2=ca(ires-1,1)+1
         endif
         if(ip2>i)then
            dx=xn-bkxx(ip2)  !!CaN
            dy=yn-bkyy(ip2)  
            dz=zn-bkzz(ip2)
            r1=dx*dx+dy*dy+dz*dz
            if((r1>dd*dtrl.or.r1>ddNold(j,jk)).and.(r1<dd*dtrh.or.r1<ddNold(j,jk)).or.r1<dismin2)cycle
            if(r1<0.0001)print*,"RN dis warning!",r1,i,ip2 !goto 601  !!!for next attempt
            rb=sqrt(dd/r1)
            xn=bkxx(ip2)+rb*dx
            yn=bkyy(ip2)+rb*dy
            zn=bkzz(ip2)+rb*dz
         else
            dx=xn-xx(ip2)  !!CaN
            dy=yn-yy(ip2)  
            dz=zn-zz(ip2)
            r1=dx*dx+dy*dy+dz*dz
            if((r1>dd*dtrl.or.r1>ddNold(j,jk)).and.(r1<dd*dtrh.or.r1<ddNold(j,jk)).or.r1<dismin2)cycle
            if(r1<0.0001)print*,"RN dis warn2!",r1,i,ip2 !goto 601  !!!for next attempt
            rb=sqrt(dd/r1)
            xn=xx(ip2)+rb*dx
            yn=yy(ip2)+rb*dy
            zn=zz(ip2)+rb*dz
         endif
      enddo
      kc=0
      do j=1,2  !!final distance do not need verify
         if(j.eq.1)then
         ip2=ca(ires-1,1)  
      elseif(j.eq.2)then
         ip2=ca(ires,1)+1
      endif
      if(ip2>i)then
         dx=xn-bkxx(ip2)  !!CaN
         dy=yn-bkyy(ip2)  
         dz=zn-bkzz(ip2)
      else
         dx=xn-xx(ip2)  !!CaN
         dy=yn-yy(ip2)  
         dz=zn-zz(ip2)
      endif
      r1=dx*dx+dy*dy+dz*dz
!      write(*,'A3,A4,I4,I4,3f8.3,I4,I4,2f8.3'),"NfF",resd(i),i,j,r1,ddN(j),ddNold(j),katt,kN,ddN(j)*dtrl,ddN(j)*dtrh
      if((r1<ddN(j)*dtrl.and.r1<ddnold(j,jk)).or.(r1>ddN(j)*dtrh.and.r1>ddnold(j,jk)))then
 !        KN=kn+1
         goto 601
      endif
      kc=kc+1         
   enddo
   if(kc>=2)then
      MSUC=MSUC+1
      xx(i)=xn
      yy(i)=yn
      zz(i)=zn
   else
      goto 601
   endif
!!!MOVE Ca*****************************************************************
   i=ipca   !!Ca
602 continue
   dx=dlmp*(rand(iseed)*2.0-1.0)  !!interval is 1.0
   dy=dlmp*(rand(iseed)*2.0-1.0)
   dz=dlmp*(rand(iseed)*2.0-1.0)
   KATT=KATT+1
   if(KATT>NLMAT)goto 131  !!end of subroutine
   xn=bkxx(i)+dx
   yn=bkyy(i)+dy
   zn=bkzz(i)+dz  !!new position
   do j=1,5
      dd=ddCa(j)
      if(j.eq.1)then
         ip2=ca(ires-1,1)  !!Ca
       elseif(j.eq.2)then
         ip2=ca(ires+1,1)
      elseif(j.eq.3)then
         ip2=ca(ires+1,1)-1
      elseif(j.eq.4)then
         ip2=ca(ires-1,1)+1
      else
         ip2=ipca-1
      endif
      if(ip2>i)then !old position
         dx=xn-bkxx(ip2)  
         dy=yn-bkyy(ip2)  
         dz=zn-bkzz(ip2)
         r1=dx*dx+dy*dy+dz*dz
     if((r1>dd*dtrl.or.r1>ddcaold(j,jk)).and.(r1<dd*dtrh.or.r1<ddcaold(j,jk)))cycle
         if(r1<0.0001)print*,"CA dis warning!",r1 !goto 602  !!!for next attempt 
         rb=sqrt(dd/r1)
         xn=bkxx(ip2)+rb*dx
         yn=bkyy(ip2)+rb*dy
         zn=bkzz(ip2)+rb*dz
      else  !!new position
         dx=xn-xx(ip2)  
         dy=yn-yy(ip2)  
         dz=zn-zz(ip2)
         r1=dx*dx+dy*dy+dz*dz
     if((r1>dd*dtrl.or.r1>ddcaold(j,jk)).and.(r1<dd*dtrh.or.r1<ddcaold(j,jk)))cycle
         if(r1<0.0001)print*,"CA dis warning!",r1 !goto 602  !!!for next attempt 
         rb=sqrt(dd/r1)
         xn=xx(ip2)+rb*dx
         yn=yy(ip2)+rb*dy
         zn=zz(ip2)+rb*dz
      endif
   enddo
   kc=0
   do j=1,4  !!final distance do not need verify
      dd=ddCa(j)
      if(j.eq.1)then
         ip2=ca(ires-1,1)  !!Ca
      elseif(j.eq.2)then
         ip2=ca(ires+1,1)
      elseif(j.eq.3)then
         ip2=ca(ires+1,1)-1
      elseif(j.eq.4)then
         ip2=ca(ires-1,1)+1
      endif
      if(ip2>i)then
         dx=xn-bkxx(ip2)  !!CaN
         dy=yn-bkyy(ip2)  
         dz=zn-bkzz(ip2)
      else
         dx=xn-xx(ip2)  !!CaN
         dy=yn-yy(ip2)  
         dz=zn-zz(ip2)
      endif
      r1=dx*dx+dy*dy+dz*dz
      if((r1<dd*dtrl.and.r1<ddcaold(j,jk)).or.(r1>dd*dtrh.and.r1>ddcaold(j,jk)))then
 !        KCA=KCA+1
          goto 602
      endif
      kc=kc+1         
   enddo
   if(kc>=4)then
      MSUC=MSUC+1
      xx(i)=xn
      yy(i)=yn
      zz(i)=zn
   else
      goto 602
   endif
!!!endof CA*************************, C

   i=ipca+1   !!C
603 continue
   dx=dlmp*(rand(iseed)*2.0-1.0)  !!interval is 1.0
   dy=dlmp*(rand(iseed)*2.0-1.0)
   dz=dlmp*(rand(iseed)*2.0-1.0)
   KATT=KATT+1
   if(KATT>NLMAT)goto 131  !!end of subroutine
   xn=bkxx(i)+dx
   yn=bkyy(i)+dy
   zn=bkzz(i)+dz  !!new position
   do j=1,3
      dd=ddC(j)
      if(j.eq.1)then
         ip2=ca(ires,1)-1 !!N
      elseif(j.eq.2)then
         ip2=ca(ires+1,1)
      else 
         ip2=ipca
      endif

      if(ip2>i)then !old position
         dx=xn-bkxx(ip2)  
         dy=yn-bkyy(ip2)  
         dz=zn-bkzz(ip2)
         r1=dx*dx+dy*dy+dz*dz
     if((r1>dd*dtrl.or.r1>ddcold(j,jk)).and.(r1<dd*dtrh.or.r1<ddcold(j,jk)))cycle
         if(r1<0.0001)print*,"CC dis warning!",r1 !goto 603  !!!for next attempt 
         rb=sqrt(dd/r1)
         xn=bkxx(ip2)+rb*dx
         yn=bkyy(ip2)+rb*dy
         zn=bkzz(ip2)+rb*dz
      else  !!new position
         dx=xn-xx(ip2)  
         dy=yn-yy(ip2)  
         dz=zn-zz(ip2)
         r1=dx*dx+dy*dy+dz*dz
     if((r1>dd*dtrl.or.r1>ddcold(j,jk)).and.(r1<dd*dtrh.or.r1<ddcold(j,jk)))cycle
         if(r1<0.0001)print*,"CC dis warning!",r1 !goto 603  !!!for next attempt 
         rb=sqrt(dd/r1)
         xn=xx(ip2)+rb*dx
         yn=yy(ip2)+rb*dy
         zn=zz(ip2)+rb*dz
      endif
   enddo
   kc=0
   do j=1,2 !!final distance do not need verify
      dd=ddC(j)
      if(j.eq.1)then
         ip2=ca(ires,1)-1  !!N
      elseif(j.eq.2)then
         ip2=ca(ires+1,1)  !CA
      endif
      if(ip2>i)then
         dx=xn-bkxx(ip2)  
         dy=yn-bkyy(ip2)  
         dz=zn-bkzz(ip2)
      else
         dx=xn-xx(ip2)  
         dy=yn-yy(ip2)  
         dz=zn-zz(ip2)
      endif
      r1=dx*dx+dy*dy+dz*dz
!  write(*,'A3,A4,I4,I4,3f8.3,I4,I4,2f8.3'),"KC",resd(i),i,j,r1,ddc(j),ddcold(j),katt,kCc,ddc(j)*dtrl,ddc(j)*dtrh
      if((r1<dd*dtrl.and.r1<ddcold(j,jk)).or.(r1>dd*dtrh.and.r1>ddcold(j,jk)))then
!         KCC=KCC+1
         goto 603
      endif
      kc=kc+1         
   enddo
   kc=kc+1
   if(kc>=3)then
      MSUC=MSUC+1
      xx(i)=xn
      yy(i)=yn
      zz(i)=zn
   else
      goto 603
   endif
jk=jk+1
enddo   !!!all the residue for backbone atoms finish (all the residues)
if(MSUC>=3*(ires1-ires0+1))then
   Mlog=.false.
   relax(1,2)=relax(1,2)+1
   goto 122
endif
131 continue
enddo
call ADD_HO
122 continue 
end subroutine relax_Lmprot


!!the dist.dat contains the 2nd information
!!distance restraints
!!this sub calculated the energy for tasser restraints

subroutine rest_tasser !txyz must be correct
implicit none
integer ip1,ip2,i
real dr,r

!!combCA.dat restraints
CAcontact=0
do i=1,NcombCA   !!combCA.dat
   ip1=ca(combsca(i,1),1)
   ip2=ca(combsca(i,2),1)
   r=sqrt((xx(ip1)-xx(ip2))**2+(yy(ip1)-yy(ip2))**2+(zz(ip1)-zz(ip2))**2)                  !new position
   if(r<=6.0)then !cacut(j1,j2))then
      CAcontact=CAcontact-pcombca(i)
   endif
enddo
!!dist.dat tasser restraints
CAdist=0
do i=1,Ndist
      ip1=ca(rsd(i,1),1)
      ip2=ca(rsd(i,2),1)
      r=sqrt((xx(ip1)-xx(ip2))**2+(yy(ip1)-yy(ip2))**2+(zz(ip1)-zz(ip2))**2)
      if(abs(r-drsd(i,1))<drsd(i,2))then
         CAdist=CAdist-1.0
      endif
enddo
!!distL.dat tasser restraints
CAdistL=0
do i=1,NdistL
   ip1=ca(rsdL(i,1),1)
   ip2=ca(rsdL(i,2),1)
   r=sqrt((xx(ip1)-xx(ip2))**2+(yy(ip1)-yy(ip2))**2+(zz(ip1)-zz(ip2))**2)
   dr=abs(r-drsdL(i))
   if(dr<1.0)dr=1.0
   CAdistL=CAdistL-1.0/dr
enddo
230 continue
end subroutine rest_tasser

!!!for sidechain optimization
!!***********************************************************************************************************
!!rotate side chain around CA-CB axis and C=O, H-N and CA rotate together, use rotate_matrix2
!!must keep the peptide planar
!!ifgg=0 based on phi planar; ifgg=1 based psi planar
subroutine CABrot(ires,angg)
implicit none
real angg,ax,ay,az,x,y,z,xi,yi,zi
integer i,ires,iatom,iatom0,iatom1
iatom=ca(ires,1)
iatom0=iatom+4
iatom1=iatom+hra(ca(ires,2))-2
     ax=xx(iatom0)  
     ay=yy(iatom0)
     az=zz(iatom0)
     xi=ax-xx(iatom)
     yi=ay-yy(iatom)
     zi=az-zz(iatom)
     if(abs(xi)+abs(yi)+abs(zi)<0.01)then
        print*,"WARNING!!, CABrot ri is no direction",xi,yi,zi,ax,ay,az
        goto 121
     endif
     do i=iatom0,iatom1   
        x=xx(i)
        y=yy(i)
        z=zz(i)
        call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !C
        if(angg>100)goto 121
        xx(i)=x
        yy(i)=y
        zz(i)=z        
     enddo
121  continue
   end subroutine CABrot

!!rotate side chain around CB-CG axis 
subroutine CBGrot(ires,angg) !(Ksuc(irep))
implicit none
real angg,ax,ay,az,x,y,z,xi,yi,zi
integer i,ires,iatom,iatom0,iatom1   !!the neighbour Ca position

iatom=ca(ires,1)
iatom0=iatom+5
iatom1=iatom+hra(ca(ires,2))-2
     ax=xx(iatom0)  
     ay=yy(iatom0)
     az=zz(iatom0)
     xi=ax-xx(iatom+3)
     yi=ay-yy(iatom+3)
     zi=az-zz(iatom+3)
     if(abs(xi)+abs(yi)+abs(zi)<0.01)then
        print*,"WARNING!!, CABrot ri is no direction",xi,yi,zi,ax,ay,az
        goto 121
     endif
     do i=iatom0,iatom1   
        x=xx(i)
        y=yy(i)
        z=zz(i)
        call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !C
        xx(i)=x
        yy(i)=y
        zz(i)=z        
     enddo
121  continue
end subroutine CBGrot

!!***********************************************************************************************************
!!rotate side chain around CG-CD axis  use rotate_matrix2
subroutine CGDrot(ires,angg) !(Ksuc(irep))
implicit none
real angg,ax,ay,az,x,y,z,xi,yi,zi
integer i,ires,iatom,iatom0,iatom1   !!the neighbour Ca position
iatom=ca(ires,1)
iatom0=iatom+6
iatom1=iatom+hra(ca(ires,2))-2
     ax=xx(iatom0)  
     ay=yy(iatom0)
     az=zz(iatom0)
     xi=ax-xx(iatom+4)
     yi=ay-yy(iatom+4)
     zi=az-zz(iatom+4)
     if(abs(xi)+abs(yi)+abs(zi)<0.01)then
        print*,"WARNING!!, CABrot ri is no direction",xi,yi,zi,ax,ay,az
        goto 121
     endif
     do i=iatom0,iatom1   
        x=xx(i)
        y=yy(i)
        z=zz(i)
        call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !C
        xx(i)=x
        yy(i)=y
        zz(i)=z        
     enddo
121  continue
   end subroutine CGDrot

!!rotate side chain around CB-X axis, X different to difefernt residue
subroutine CBZrot(ires,angg) !(Ksuc(irep))
implicit none
real angg,ax,ay,az,x,y,z,xi,yi,zi
integer i,ires,iresin,ip2,iatom,iatom0,iatom1   !!the neighbour Ca position
iatom=ca(ires,1)
iatom0=iatom+3
iresin=ca(ires,2)
if(iresin.eq.9)then  !MET
   ip2=iatom+6
elseif(iresin.eq.7.or.(iresin>=10.and.iresin<=12).or.iresin.eq.14.or.iresin.eq.15)then
   ip2=iatom+5
elseif(iresin.eq.13.or.iresin.eq.16.or.iresin.eq.17)then
   ip2=iatom+7
elseif(iresin.eq.18.or.iresin.eq.19)then
   ip2=iatom+9
elseif(iresin.eq.20)then
   ip2=iatom+12
endif
iatom1=iatom+hra(ca(ires,2))-2
     ax=xx(ip2)  
     ay=yy(ip2)
     az=zz(ip2)
     xi=xx(iatom0)-ax
     yi=yy(iatom0)-ay
     zi=zz(iatom0)-az
     if(abs(xi)+abs(yi)+abs(zi)<0.01)then
        print*,"WARNING!!, CABrot ri is no direction",xi,yi,zi,ax,ay,az
        goto 121
     endif
     do i=iatom0,iatom1   
        x=xx(i)
        y=yy(i)
        z=zz(i)
        call rotate_matrix2(x,y,z,ax,ay,az,xi,yi,zi,angg)  !C
        xx(i)=x
        yy(i)=y
        zz(i)=z        
     enddo
121  continue
   end subroutine CBZrot

!cccccccccccccccc Calculate sum of (r_d-r_m)^2 cccccccccccccccccccccccccc
!c  w    - w(m) is weight for atom pair  c m           (given)
!c  x    - x(i,m) are coordinates of atom c m in set x       (given)
!c  y    - y(i,m) are coordinates of atom c m in set y       (given)
!c  n    - n is number of atom pairs                         (given)
!c  mode  - 0:calculate rms only                             (given)
!c          1:calculate rms,u,t                              (takes longer)
!c  rms   - sum of w*(ux+t-y)**2 over all atom pairs         (result)
!c  u    - u(i,j) is   rotation  matrix for best superposition  (result)
!c  t    - t(i)   is translation vector for best superposition  (result)
!c  ier  - 0: a unique optimal superposition has been determined(result)
!c       -1: superposition is not unique but optimal
!c       -2: no result obtained because of negative weights w
!c           or all weights equal to zero.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       subroutine u3b(w,x,y,n,mode,rms,u,t,ier)
         implicit none
      integer ip(9), ip2312(4)
      integer i,j,k,l,m1,m,ier,n,mode
      double precision w(n),x(3,n),y(3,n),u(3,3),t(3),r(3,3),xc(3),yc(3),wc,a(3,3),b(3,3),e0,e(3)
      double precision sigma,d,spur,det,cof,h,g,cth,sth,sqrth,p,tol,rms
      double precision rr(6), ss(6)
      double precision zero,one,two,three,sqrt3
      data ip/1,2,4,2,3,5,4,5,6/
      data ip2312/2,3,1,2/
!c 156 "rms.for"
      wc=0.0d+00
      rms=0.0
      e0=0.0d+00
      do 1 i= 1,3
      xc(i)=0.0d+00
      yc(i)=0.0d+00
      t(i)=0.0
      do 1 j=1,3
      d = 0.0d+00
      if(i.eq.j)d =1.0d+00
      u(i,j)=d
      a(i,j)=d
    1 r(i,j)=0.0d+00
      ier=-1
!c**** DETERMINE CENTROIDS OF BOTH VECTOR SETS X AND Y
      if(n.lt.1)return 
      ier=-2
      do 2 m=1,n
      if(w(m).lt.0.0)return 
      wc=wc+w(m)
      do 2 i = 1, 3
      xc(i)=xc(i)+w(m)*x(i,m)
    2 yc(i)=yc(i)+w(m)*y(i,m)
      if (wc.le.0.0d+00) return 
      do 3 i = 1, 3
      xc(i)=xc(i)/wc
!c**** DETERMINE CORRELATION MATRIX R BETWEEN VECTOR SETS Y AND X
    3 yc(i)=yc(i)/wc
      do 4 m= 1,n
      do 4 i= 1,3
      e0=e0+w(m)*((x(i,m)-xc(i))*(x(i,m)-xc(i))+(y(i,m)-yc(i))*(y(i,m)-yc(i)))
      d=w(m)*(y(i,m) - yc(i))
      do 4 j=1,3
!c**** CALCULATE DETERMINANT OF R(I,J)
    4 r(i,j)=r(i,j)+(d*(x(j,m)-xc(j)))
   det=r(1,1)*(r(2,2)*r(3,3)-r(2,3)*r(3,2))-(r(1,2)*(r(2,1)*r(3,3)-r(2,3)*r(3,1)))+(r(1,3)*(r(2,1)*r(3,2)-r(2,2)*r(3,1)))
!c**** FORM UPPER TRIANGLE OF TRANSPOSED(R)*R
      sigma=det
      m=0
      do 5 j=1,3
      do 5 i=1,j
      m=m+1
!c***************** EIGENVALUES *****************************************
!c**** FORM CHARACTERISTIC CUBIC  X**3-3*SPUR*X**2+3*COF*X-DET=0
    5 rr(m)=r(1,i)*r(1,j)+r(2,i)*r(2,j)+r(3,i)* r(3,j)
      spur=(rr(1)+rr(3)+rr(6))/3.0d+00
      cof=(rr(3)*rr(6)-rr(5)*rr(5)+rr(1)*rr(6)-rr(4)*rr(4)+rr(1)*rr(3)-rr(2)*rr(2))/3.0d+00
      det=det*det
      do 6 i = 1, 3
    6 e(i) = spur
!c**** REDUCE CUBIC TO STANDARD FORM Y**3-3HY+2G=0 BY PUTTING X=Y+SPUR
      if(spur.le.0.0d+00)goto 40
      d=spur*spur
      h=d-cof
!c**** SOLVE CUBIC. ROOTS ARE E(1),E(2),E(3) IN DECREASING ORDER
      g=0.5*(spur*cof-det)-spur*h
      if(h.le.0.0)goto 8
      sqrth=dsqrt(h)
      d =h*h*h-g*g
      if(d.lt.0.0)d=0.0
      d=datan2(dsqrt(d),-g)/3.0
      cth=sqrth*dcos(d)
      sth=sqrth*1.73205080756888*dsin(d)
      e(1)=spur+cth+cth
      e(2)=spur-cth+sth
      e(3)=spur-cth-sth
!c.....HANDLE SPECIAL CASE OF 3 IDENTICAL ROOTS
!c 224 "rms.for"
      if (mode) 10, 50, 10
!c**************** EIGENVECTORS *****************************************
    8 if (mode) 30, 50, 30
   10 do 15 l=1,3,2
      d=e(l)
      ss(1)=(d-rr(3))*(d-rr(6))-rr(5)*rr(5)
      ss(2)=(d-rr(6))*rr(2)+rr(4)*rr(5)
      ss(3)=(d-rr(1))*(d-rr(6))-rr(4)*rr(4)
      ss(4)=(d-rr(3))*rr(4)+rr(2)*rr(5)
      ss(5)=(d-rr(1))*rr(5)+rr(2)*rr(4)
      ss(6)=(d-rr(1))*(d-rr(3))-rr(2)*rr(2)
      j = 1
      if(dabs(ss(1)).ge.dabs(ss(3)))goto 12
      j = 2
      if(dabs(ss(3)).ge.dabs(ss(6)))goto 13
   11 j= 3
      goto 13
   12 if(dabs(ss(1)).lt.dabs(ss(6)))goto 11
   13 d=0.0d+00
      j=3*(j - 1)
      do 14 i = 1, 3
      k=ip(i + j)
      a(i,l) = ss(k)
   14 d = d + (ss(k) * ss(k))
      if (d .gt. 0.0d+00) d = 1.0d+00 / dsqrt(d)
      do 15 i = 1, 3
   15 a(i,l) = a(i,l) * d
      d=a(1,1)*a(1,3)+a(2,1)*a(2,3)+a(3,1)*a(3,3)
      m1=3
      m=1
      if((e(1) - e(2)).gt.(e(2)-e(3)))goto 16
      m1=1
      m=3
   16 p=0.0d+00
      do 17 i = 1, 3
      a(i,m1) = a(i,m1) - (d*a(i,m))
   17 p =p+a(i,m1) *a(i,m1)
      if (p .le. 1.0d-2) goto 19
      p = 1.0d+00 / dsqrt(p)
      do 18 i = 1, 3
   18 a(i,m1) = a(i,m1)*p
      goto 21
   19 p = 1.0d+00
      do 20 i = 1, 3
      if (p .lt. dabs(a(i,m))) goto 20
      p = dabs(a(i,m))
      j = i
   20 continue
      k = ip2312(j)
      l = ip2312(j + 1)
      p = dsqrt(a(k,m)*a(k,m)+a(l,m)*a(l,m))
      if (p.le.1.0d-2) goto 40
      a(j,m1) = 0.0d+00
      a(k,m1) =-a(l,m)/p
      a(l,m1) =a(k,m)/p
   21 a(1,2)=a(2,3)*a(3,1)-a(2,1)*a(3,3)
      a(2,2)=a(3,3)*a(1,1)-a(3,1)*a(1,3)
      a(3,2)=a(1,3)*a(2,1)-a(1,1)*a(2,3)
   30 do 32 l = 1, 2
      d = 0.0d+00
      do 31 i = 1, 3
      b(i,l)=r(i,1)*a(1,l)+r(i,2)*a(2,l)+r(i,3)*a(3,l)
   31 d = d + (b(i,l) ** 2)
      if(d.gt.0.0d+00)d= 1.0d+00/dsqrt(d)
      do 32 i = 1, 3
   32 b(i,l) = b(i,l) * d
      d=b(1,1)*b(1,2)+b(2,1)*b(2,2)+b(3,1)*b(3,2)
      p = 0.0d+00
      do 33 i = 1, 3
      b(i,2)=b(i,2)-d*b(i,1)
   33 p =p+b(i,2)*b(i,2)
      if (p .le. 1.0d-2) goto 35
      p=1.0d+00/dsqrt(p)
      do 34 i = 1, 3
   34 b(i,2)=b(i,2)*p
      goto 37
   35 p=1.0d+00
      do 36 i = 1, 3
      if (p.lt.dabs(b(i,1))) goto 36
      p=dabs(b(i,1))
      j=i
   36 continue
      k=ip2312(j)
      l=ip2312(j + 1)
      p=dsqrt(b(k,1)*b(k,1)+b(l,1)*b(l,1))
      if(p.le.1.0d-2)goto 40
      b(j,2)=0.0d+00
      b(k,2)=-b(l,1)/p
      b(l,2)=b(k,1)/p
   37 b(1,3)=b(2,1)*b(3,2)-b(2,2)*b(3,1)
      b(2,3)=b(3,1)*b(1,2)-b(3,2)*b(1,1)
      b(3,3)=b(1,1)*b(2,2)-b(1,2)*b(2,1)
      do 39 i = 1, 3
      do 39 j = 1, 3
!c****************** TRANSLATION VECTOR *********************************
   39 u(i,j)=b(i,1)*a(j,1)+b(i,2)*a(j,2)+b(i,3)*a(j,3)
   40 do 41 i = 1, 3
!c********************** RMS ERROR **************************************
   41 t(i)=yc(i)-u(i,1)*xc(1)-u(i,2)*xc(2)-u(i,3)*xc(3)
   50 do 51 i = 1, 3
      if (e(i) .lt. 0.0d+00) e(i) = 0.0d+00
   51 e(i) = dsqrt(e(i))
      ier = 0
      if(e(2).le.(e(1)*1.0d-05))ier=-1
      d=e(3)
      if(sigma.ge.0.0)goto 52
      d =-d
      if((e(2)-e(3)).le.(e(1) * 1.0d-05))ier=-1
   52 d=(d + e(2)) + e(1)
      rms=(e0 - d)- d
      if(rms.lt.0.0)rms=0.0
      return 
!c.....END U3B...........................................................
  end subroutine u3b 

end program main
