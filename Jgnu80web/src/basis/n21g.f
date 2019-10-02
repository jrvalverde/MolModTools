
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 n21g"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "n21g.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 44 "n21g.web"
      subroutine n21g(IOP,NATOMS,IAN,C,NBASIS)
      implicit none
      double precision C,C1,C2,C3,C4,cp,cs,e,Exx,hescal,hhscl1,hhscl2,hs
     &cal1,hscal2,one,pol,Scale,X,xcoord,Y
      double precision ycoord,Z,zcoord,zero
      integer i,ia,IAN,iao,idx,ierr1,In,IOP,Iout,ipold,ipolp,Ipunch,j,j1
     &,Jan,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp
      integer mm,mmdf,NATOMS,NBASIS,nd,ngauss,ngin,Nshell,nstart,numd
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      double precision namp,namd
      dimension IAN(*),IOP(50),C(*)
      dimension e(15),cs(15),cp(9),pol(10)
      dimension iao(6),nd(4),namd(2)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/scale/Scale(MAXSHL)
      common/io/In,Iout,Ipunch
      data zero/0.0D0/,one/1.0D0/
      data pol/2*1.1D0,8*0.8D0/
      data iao/4H  1S,4H2SPI,4H2SPO,4H 2SP,4H3SPI,4H3SPO/,nd/5,5,6,6/
      data namp/6H     P/,namd/6H  D(5),6H  D(6)/
      data hscal1/1.1D0/,hscal2/1.1D0/,hescal/1.0D0/
      data ierr1/6/
      
      
      
99001 format(' ',53('*'),/,' POLARIZATION FUNCTIONS NOT GUARANTEED IN N-
     &21G BASES'/,53('*'),/)
      
      
      mm=1
      mmdf=1
      Nshell=0
      nstart=1
      ngauss=IOP(6)
      if(ngauss.EQ.0)ngauss=3
      ngin=2
      ipold=IOP(7)
      ipolp=IOP(7)/2
      if(ipolp+ipold.GT.0)write(Iout,99001)
      numd=IOP(8)+1
      numd=nd(numd)
      do 100 i=1,240
      Exx(i)=zero
      C1(i)=zero
      C2(i)=zero
      C3(i)=zero
100   continue
      call putlbl(0,0,-1)
      
      
      do 300 i=1,NATOMS
      
      ia=IAN(i)
      idx=3*(i-1)
      xcoord=C(idx+1)
      ycoord=C(idx+2)
      zcoord=C(idx+3)
      call putlbl(i,ia,0)
      if(ia.LE.2)then
      
      
      call dn21g(e,cs,cp,ia,ngauss,ierr1)
      
      hhscl1=hscal1
      hhscl2=hscal2
      if(ia.EQ.2)hhscl1=hescal
      if(ia.EQ.2)hhscl2=hescal
      
      do 120 j=1,ngin
      j1=mm-1+j
      Exx(j1)=e(j)*hhscl1**2
      C1(j1)=cs(j)
120   continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=ngin
      Shellt(Nshell)=0
      Scale(Nshell)=hscal1
      Aon(Nshell)=iao(1)
      Aos(Nshell)=nstart
      call putlbl(1,0,2)
      nstart=nstart+1
      mm=mm+ngin
      
      Exx(mm)=e(ngin+1)*hhscl2**2
      C1(mm)=one
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=1
      Shellt(Nshell)=0
      Scale(Nshell)=hscal2
      Aon(Nshell)=iao(1)
      Aos(Nshell)=nstart
      call putlbl(1,0,4)
      nstart=nstart+1
      mm=mm+1
      
      if(ipolp.GT.0)then
      Nshell=Nshell+1
      Shellc(Nshell)=1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=1
      Shellt(Nshell)=1
      Aon(Nshell)=namp
      Scale(Nshell)=one
      Exx(mm)=pol(ia)
      C2(mm)=one
      Aos(Nshell)=nstart-1
      call putlbl(0,1,1)
      nstart=nstart+3
      mm=mm+1
      endif
      
      elseif(ia.LE.10)then
      
      
      call dn21g(e,cs,cp,ia,ngauss,ierr1)
      
      do 140 j=1,ngauss
      j1=mm-1+j
      Exx(j1)=e(j)
      C1(j1)=cs(j)
140   continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=ngauss
      Shellt(Nshell)=0
      Scale(Nshell)=one
      Aos(Nshell)=nstart
      Aon(Nshell)=iao(1)
      call putlbl(1,0,1)
      nstart=nstart+1
      mm=mm+ngauss
      
      do 160 j=1,ngin
      j1=mm-1+j
      Exx(j1)=e(j+ngauss)
      C1(j1)=cs(j+ngauss)
      C2(j1)=cp(j)
160   continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=ngin
      Shellt(Nshell)=1
      Scale(Nshell)=one
      Aon(Nshell)=iao(2)
      Aos(Nshell)=nstart
      call putlbl(2,0,2)
      call putlbl(2,1,2)
      nstart=nstart+4
      mm=mm+ngin
      
      Exx(mm)=e(ngauss+ngin+1)
      C1(mm)=one
      C2(mm)=one
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=1
      Shellt(Nshell)=1
      Scale(Nshell)=one
      Aon(Nshell)=iao(3)
      Aos(Nshell)=nstart
      call putlbl(2,0,4)
      call putlbl(2,1,4)
      nstart=nstart+4
      mm=mm+1
      
      if(ipold.GT.0)then
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=mmdf
      Shelln(Nshell)=1
      Shellt(Nshell)=2
      Scale(Nshell)=one
      Aon(Nshell)=namd(numd-4)
      Exx(mm)=pol(ia)
      C3(mmdf)=one
      mm=mm+1
      mmdf=mmdf+1
      Aos(Nshell)=nstart-4
      call putlbl(0,2,1)
      nstart=nstart+numd
      endif
      else
      
      if(ia.GT.18)call berror(ierr1)
      
      
      
      
      call dn21g(e,cs,cp,ia,ngauss,ierr1)
      
      do 180 j=1,ngauss
      j1=mm-1+j
      Exx(j1)=e(j)
      C1(j1)=cs(j)
180   continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=ngauss
      Shellt(Nshell)=0
      Scale(Nshell)=one
      Aos(Nshell)=nstart
      Aon(Nshell)=iao(1)
      call putlbl(1,0,1)
      nstart=nstart+1
      mm=mm+ngauss
      
      do 200 j=1,ngauss
      j1=mm-1+j
      Exx(j1)=e(j+ngauss)
      C1(j1)=cs(j+ngauss)
      C2(j1)=cp(j)
200   continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=ngauss
      Shellt(Nshell)=1
      Scale(Nshell)=one
      Aon(Nshell)=iao(4)
      Aos(Nshell)=nstart
      call putlbl(2,0,1)
      call putlbl(2,1,1)
      nstart=nstart+4
      mm=mm+ngauss
      
      do 220 j=1,ngin
      j1=mm-1+j
      Exx(j1)=e(j+ngauss+ngauss)
      C1(j1)=cs(j+ngauss+ngauss)
      C2(j1)=cp(j+ngauss)
220   continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=ngin
      Shellt(Nshell)=1
      Scale(Nshell)=one
      Aon(Nshell)=iao(5)
      Aos(Nshell)=nstart
      call putlbl(3,0,2)
      call putlbl(3,1,2)
      nstart=nstart+4
      mm=mm+ngin
      
      Exx(mm)=e(ngauss+ngauss+ngin+1)
      C1(mm)=one
      C2(mm)=one
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=1
      Shellt(Nshell)=1
      Scale(Nshell)=one
      Aon(Nshell)=iao(6)
      Aos(Nshell)=nstart
      call putlbl(3,0,4)
      call putlbl(3,1,4)
      nstart=nstart+4
      mm=mm+1
      endif
      
300   continue
      
      Aos(Nshell+1)=nstart
      NBASIS=nstart-1
      
      return
      
      end
C* :1 * 
      
