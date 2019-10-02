
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 d1e"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "d1e.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "d1e.web"
      subroutine d1e(JUMP)
      implicit none
      double precision Atmchg,C,C1,C2,C3,Core,Exx,X,Y,Z
      integer i,i1x,i1y,i1z,i2x,i2y,i2z,i3x,i3y,i3z,i4x,i4y,i4z,i5x,i5y,
     &i5z,Ian,Icharg,idump,ie1xx
      integer iend,if1xyz,Ifill7,ii,In,inao,Iop,Iout,iprint,ipt,Ipunch,I
     &purd,Ipurf,Irwb,Irwfx,Irwpt,Irww,itest,iwt,Jan
      integer JUMP,LENB,lrwfx,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,Multip,
     &Mxcore,Nae,nat3,nat3tt,Natoms,nbas6d,Nbasis,Nbe,Ne,Nshell,ntt
      integer ntt6d
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon
      dimension ii(15)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      common/ipure/Ipurd,Ipurf
      common/io/In,Iout,Ipunch
      common/irw701/Irwb(2),Irwpt,Irww,Irwfx,Ifill7(3)
      common/memry/Core(49999),Mxcore
      equivalence(i1x,ii(1)),(i1y,ii(2)),(i1z,ii(3)),(i2x,ii(4)),(i2y,ii
     &(5)),(i2z,ii(6)),(i3x,ii(7)),(i3y,ii(8)),(i3z,ii(9)),(i4x,ii(10)),
     &(i4y,ii(11)),(i4z,ii(12)),(i5x,ii(13)),(i5y,ii(14)),(i5z,ii(15))
      
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(' ALLOCATION FAILURE IN D1E.'/5x,'NEEDS',i6,'   BUT ONLY HA
     &S',i6,' WORDS.')
      
      
      Mxcore=49999
      
      
      iprint=Iop(33)
      idump=Iop(34)
      if(Iop(34).EQ.3)call bdump(2)
      ntt=(Nbasis*(Nbasis+1))/2
      nat3=3*Natoms
      
      call tread(Irwb(1),Exx(1),LENB,1,LENB,1,0)
      
      call ilsw(2,2,Ipurd)
      call ilsw(2,16,Ipurf)
      ntt6d=ntt
      if(Ipurd.NE.1)then
      call getnb6(nbas6d)
      ntt6d=(nbas6d*(nbas6d+1))/2
      endif
      
      
      ie1xx=1
      if1xyz=ie1xx+1
      ipt=if1xyz+3*Natoms
      iwt=ipt+ntt6d
      ii(1)=iwt+ntt6d
      do 100 i=2,15
      ii(i)=ii(i-1)+Natoms
100   continue
      iend=ii(15)+Natoms-1
      if(iend.GT.Mxcore)then
      write(Iout,99001)iend,Mxcore
      call lnk1e
      endif
      
      call tread(Irwpt,Core(ipt),ntt,1,ntt,1,0)
      call tread(Irww,Core(iwt),ntt,1,ntt,1,0)
      
      if(Ipurd.NE.1)then
      inao=iend+1
      itest=inao+nbas6d-1
      if(itest.GT.Mxcore)then
      write(Iout,99001)itest,Mxcore
      call lnk1e
      endif
      call redob(Nbasis,Core(inao),iprint)
      call redop(Nbasis,nbas6d,Core(inao),Core(ipt),iprint)
      call redop(Nbasis,nbas6d,Core(inao),Core(iwt),iprint)
      Ipurd=1
      endif
      
      
      
      
      call dstvnt(Natoms,Atmchg,C,Core(ipt),Core(iwt),Core(ie1xx),Core(i
     &f1xyz),Core(i1x),Core(i1y),Core(i1z),Core(i2x),Core(i2y),Core(i2z)
     &,Core(i3x),Core(i3y),Core(i3z),Core(i4x),Core(i4y),Core(i4z),Core(
     &i5x),Core(i5y),Core(i5z),iprint,idump)
      
      nat3tt=(nat3*(nat3+1))/2
      lrwfx=1+nat3+nat3tt
      lrwfx=5671
      call twrite(Irwfx,Core(ie1xx),lrwfx,1,lrwfx,1,0)
      
      JUMP=0
      return
      
      end
C* :1 * 
      
