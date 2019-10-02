
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 d2espd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "d2espd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "d2espd.web"
      subroutine d2espd(JUMP)
      implicit none
      double precision Atmchg,C,C1,C2,C3,C4,Core,Exx,Shladf,X,Y,Z
      integer i,iacc,iad,Ian,Icharg,idm,idn,idump,iend,ifrc,ifxyz,In,ina
     &o,initf,Iop,Iout,ipad,iprint,Ipunch,Ipurd
      integer Ipurf,ipvec,Irwb,Irwfx,Irwpa,Irwpb,Irwpt,Irwpti,Irwsym,isc
     &f,iscfp,isymgr,itemp,ivee,j,Jan,jfxyz,jtrans,JUMP,jvee
      integer k,LENB,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,mout,Multip,Mxco
     &re,Nae,nat3,Natoms,nbas6d,Nbasis,Nbe,nden,Ne,neqatm,neqtmp
      integer nosym,Nshell,nsymop,ntt,ntt6d
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon
      logical usesym
      dimension neqtmp(800),ipvec(10)
      dimension jtrans(3,8),neqatm(100,8),mout(846),ipad(19)
      common/memry/Core(49999),Mxcore
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/io/In,Iout,Ipunch
      common/irw703/Irwb(2),Irwpt,Irwpa,Irwpb,Irwpti,Irwfx,Irwsym
      common/ipure/Ipurd,Ipurf
      equivalence(nsymop,mout(1))
      equivalence(jtrans(1,1),mout(2))
      equivalence(neqatm(1,1),mout(26))
      equivalence(ipad(1),mout(829))
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(' ALLOCATION FAILURE IN D2ESP.'/5x,'NEEDS',i6,'   BUT ONLY 
     &HAS',i6,' WORDS.')
99002 format(' NO CODE YET FOR COMPLEX UHF ... SORRY.')
99003 format(' NO CODE YET FOR ROHF ... SORRY.')
      
      
      Mxcore=49999
      
      call drum
      
      initf=Iop(27)
      iacc=Iop(28)
      isymgr=Iop(30)
      iprint=Iop(33)
      idump=Iop(34)
      ntt=(Nbasis*(Nbasis+1))/2
      nat3=3*Natoms
      
      call ilsw(2,24,ifrc)
      if(ifrc.EQ.1)goto 400
      
      call tread(Irwb(1),Exx(1),LENB,1,LENB,1,0)
      
      call ilsw(2,2,Ipurd)
      call ilsw(2,16,Ipurf)
      ntt6d=ntt
      if(Ipurd.NE.1)then
      call getnb6(nbas6d)
      ntt6d=(nbas6d*(nbas6d+1))/2
      endif
      
      call ilsw(2,1,iscf)
      iscfp=iscf+1
      nden=1
      if(iscf.NE.0)nden=2
      
      ivee=1
      ifxyz=ivee+1
      idm=ifxyz+3*Natoms
      idn=idm
      if(nden.EQ.2)idn=idm+ntt6d
      iend=idn+ntt6d-1
      if(iend.GT.Mxcore)then
      write(Iout,99001)iend,Mxcore
      call lnk1e
      endif
      
      if(iscfp.EQ.2)then
      
      call tread(Irwpa,Core(idm),ntt,1,ntt,1,0)
      call tread(Irwpb,Core(idn),ntt,1,ntt,1,0)
      elseif(iscfp.EQ.3)then
      
      call tread(Irwpt,Core(idm),ntt,1,ntt,1,0)
      call tread(Irwpti,Core(idn),ntt,1,ntt,1,0)
      elseif(iscfp.EQ.4)then
      
      write(Iout,99002)
      call lnk1e
      goto 100
      elseif(iscfp.EQ.5)then
      goto 100
      else
      
      call tread(Irwpt,Core(idm),ntt,1,ntt,1,0)
      endif
      goto 200
      
100   write(Iout,99003)
      call lnk1e
200   ipvec(1)=idm
      ipvec(2)=idn
      if(Ipurd.NE.1)then
      inao=iend+1
      itemp=inao+nbas6d-1
      if(itemp.GT.Mxcore)then
      write(Iout,99001)iend,Mxcore
      call lnk1e
      endif
      call redob(Nbasis,Core(inao),iprint)
      do 250 i=1,nden
      iad=ipvec(i)
      call redop(Nbasis,nbas6d,Core(inao),Core(iad),iprint)
250   continue
      Ipurd=1
      endif
      
      call ilsw(2,26,nosym)
      usesym=(nosym.EQ.0).AND.(isymgr.EQ.0)
      if(usesym)then
      call tread(Irwsym,nsymop,422,1,422,1,0)
      k=0
      do 300 j=1,nsymop
      do 260 i=1,Natoms
      k=k+1
      neqtmp(k)=neqatm(i,j)
260   continue
300   continue
      endif
      
      
      
      
      call dphnix(iacc,iscf,Core(idm),Core(idn),Core(ivee),Core(ifxyz),C
     &,Natoms,usesym,nsymop,jtrans,neqtmp,iprint,idump)
      
      
      jvee=idm
      jfxyz=jvee+1
      call aclear(nat3,Core(jfxyz))
      if(initf.EQ.1)call tread(Irwfx,Core(jvee),nat3+1,1,nat3+1,1,0)
      
      call aadd(nat3,Core(ifxyz),Core(jfxyz),Core(jfxyz))
      
      call twrite(Irwfx,Core(jvee),nat3+1,1,nat3+1,1,0)
      
400   JUMP=0
      return
      
      end
C* :1 * 
      
