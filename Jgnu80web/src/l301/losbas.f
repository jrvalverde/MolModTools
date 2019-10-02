
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 losbas"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "losbas.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 31 "losbas.web"
      subroutine losbas(ITYPE,IPOLAR,NUMD,NUMF,NATOMS,IAN,C,NBASIS)
      implicit none
      real*8 C,cd,cp,cs,e,one,Scal1,Scal1x,Scal2,Scal2x,Scal3,Scal3x,Sca
     &l4,Scal4x,Scale,sfact,xcoord,ycoord,zcoord
      integer i,ia,IAN,iao,Icount,ifunc,ihsp,In,Iout,IPOLAR,iprim,Ipunch
     &,Iscal,ishc,isht,ITYPE,j,j1,j2,maxcon
      integer mdim,mm,mmdf,NATOMS,NBASIS,nconst,ncont,nfunc,ngauss,ngf,n
     &start,NUMD,NUMF
      
      
      dimension IAN(*),C(3,*)
      integer MAXSHL,MAXPRM,MAXSH1,MAXS21,Jan,Shella,Shelln,Shellt,Shell
     &c,Shladf,Aos,Aon,Nshell,Maxtyp
      real*8 Exx,C1,C2,C3,C4,X,Y,Z
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/scalsp/Scal1(35),Scal1x(35),Scal2(35),Scal2x(35),Scal3(35),
     &Scal3x(35),Scal4(35),Scal4x(35),Iscal(35),Icount
      common/scale/Scale(MAXSHL)
      common/io/In,Iout,Ipunch
      dimension e(20),cs(20),cp(20),cd(20),isht(20),ishc(20),nfunc(3,4),
     &nconst(3,4),ngauss(20),iao(4)
      save mdim,maxcon,nfunc,nconst,one,iao,ihsp
      data mdim/20/,maxcon/10/
      data nfunc/1,0,1,4,3,4,10,0,6,0,0,10/
      data nconst/0,0,0,0,1,0,0,0,4,0,0,10/
      data one/1.0D0/,iao/1HS,1HP,1HD,1HF/,ihsp/2HSP/
99001 format(' ILLEGAL ITYPE=',i10,' IN LANL1.')
99002 format(' POLARIZATION FUNCTIONS ARE NOT AVAILABLE FOR ',/,' LOS AL
     &AMOS BASES; REQUEST IGNORED')
      
      if(ITYPE.LT.0.OR.ITYPE.GT.3)write(Iout,99001)
      if(ITYPE.LT.0.OR.ITYPE.GT.3)call lnk1e
      if(IPOLAR.NE.0)write(Iout,99002)
      Maxtyp=1
      mm=1
      mmdf=1
      Nshell=0
      nstart=1
      nfunc(1,3)=NUMD+4
      nfunc(3,3)=NUMD
      nfunc(3,4)=NUMF
      call aclear(MAXPRM,Exx)
      call aclear(MAXPRM,C1)
      call aclear(MAXPRM,C2)
      call aclear(MAXPRM,C3)
      call putlbl(0,0,-1)
      do 100 i=1,NATOMS
      xcoord=C(1,i)
      ycoord=C(2,i)
      zcoord=C(3,i)
      ia=IAN(i)
      if(ia.NE.0)then
      call putlbl(i,ia,0)
      call iclear(20,isht)
      call iclear(20,ishc)
      call la1bas(mdim,ITYPE,e,cs,cp,cd,ia,ncont,ngauss,isht,ishc)
      iprim=0
      do 20 ifunc=1,ncont
      ngf=ngauss(ifunc)
      do 10 j=1,ngf
      j1=mm-1+j
      sfact=one
      if(ITYPE.EQ.3)then
      if(ia.LE.10)then
      if(ia.GT.2)then
      if(ifunc.EQ.1)sfact=Scal2(i)
      if(ifunc.EQ.2)sfact=Scal2x(i)
      else
      if(ifunc.EQ.1)sfact=Scal1(i)
      if(ifunc.EQ.2)sfact=Scal1x(i)
      endif
      endif
      endif
      C1(j1)=cs(j+iprim)
      C2(j1)=cp(j+iprim)
      Exx(j1)=e(j+iprim)*sfact*sfact
      j2=mmdf-1+j
      if(isht(ifunc).EQ.2)C3(j2)=cd(j+iprim)
10    continue
      iprim=iprim+ngauss(ifunc)
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shellt(Nshell)=isht(ifunc)
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      if(Shellt(Nshell).GE.2)Shladf(Nshell)=mmdf
      Shelln(Nshell)=ngauss(ifunc)
      Shellc(Nshell)=ishc(ifunc)
      Scale(Nshell)=sfact
      Aos(Nshell)=nstart-nconst(ishc(ifunc)+1,isht(ifunc)+1)
      if(isht(ifunc).EQ.1.AND.ishc(ifunc).NE.1)call putlbl(ifunc,0,1)
      call putlbl(ifunc,isht(ifunc),1)
      Aon(Nshell)=iao(isht(ifunc)+1)
      if(Shellt(Nshell).EQ.1.AND.Shellc(Nshell).EQ.0)Aon(Nshell)=ihsp
      nstart=nstart+nfunc(Shellc(Nshell)+1,Shellt(Nshell)+1)
      if(Shellt(Nshell).GE.2)mmdf=mmdf+Shelln(Nshell)
      mm=mm+Shelln(Nshell)
20    continue
      endif
100   continue
      Aos(Nshell+1)=nstart
      NBASIS=nstart-1
      return
      end
C* :1 * 
      
