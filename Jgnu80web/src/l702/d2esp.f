
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 d2esp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "d2esp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "d2esp.web"
      subroutine d2esp(JUMP)
      implicit none
      double precision Atmchg,C,C1,C2,C3,C4,Core,Exx,Shladf,X,Y,Z
      integer i,iacc,Ian,Icharg,icrit,idm,idn,idump,iend,ifrc,ifxyz,In,i
     &nitf,Iop,Iout,ipad,Ipunch,Irwb,Irwfx,Irwpa
      integer Irwpb,Irwpt,Irwpti,Irwsym,iscf,iscfp,isymgr,ivee,j,Jan,jtr
     &ans,JUMP,k,LENB,lrwfx,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp
      integer mout,Multip,Mxcore,Nae,nat3,nat3tt,Natoms,Nbasis,Nbe,nden,
     &Ne,neqatm,neqtmp,nosym,Nshell,nsymop,ntt
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon
      logical usesym
      dimension neqtmp(800)
      dimension jtrans(3,8),neqatm(100,8),mout(846),ipad(19)
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
      common/irw702/Irwb(2),Irwpt,Irwpa,Irwpb,Irwpti,Irwfx,Irwsym
      common/memry/Core(49999),Mxcore
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
      
      icrit=Iop(18)
      initf=Iop(27)
      iacc=Iop(28)
      isymgr=Iop(30)
      idump=Iop(34)
      ntt=(Nbasis*(Nbasis+1))/2
      nat3=3*Natoms
      
      if(iacc.LE.0)then
      call ilsw(2,24,ifrc)
      if(ifrc.EQ.1)goto 200
      
      call tread(Irwb(1),Exx(1),LENB,1,LENB,1,0)
      
      call ilsw(2,1,iscf)
      iscfp=iscf+1
      nden=1
      if(iscf.NE.0)nden=2
      
      ivee=1
      ifxyz=ivee+1
      idm=ifxyz+3*Natoms
      idn=idm
      if(nden.EQ.2)idn=idm+ntt
      iend=idn+ntt-1
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
      goto 50
      elseif(iscfp.EQ.5)then
      goto 50
      else
      
      call tread(Irwpt,Core(idm),ntt,1,ntt,1,0)
      endif
      goto 100
      
50    write(Iout,99003)
      call lnk1e
      
100   call ilsw(2,26,nosym)
      usesym=(nosym.EQ.0).AND.(isymgr.EQ.0)
      if(usesym)then
      call tread(Irwsym,nsymop,422,1,422,1,0)
      k=0
      do 120 j=1,nsymop
      do 110 i=1,Natoms
      k=k+1
      neqtmp(k)=neqatm(i,j)
110   continue
120   continue
      endif
      
      call aclear(nat3,Core(ifxyz))
      if(initf.EQ.1)call tread(Irwfx,Core(ivee),nat3+1,1,nat3+1,1,0)
      
      
      
      
      call twldrv(iscf,Core(idm),Core(idn),icrit,usesym,nsymop,jtrans,ne
     &qtmp,Natoms,C,Core(ivee),Core(ifxyz),idump)
      
      nat3tt=(nat3*(nat3+1))/2
      lrwfx=1+nat3+nat3tt
      lrwfx=5671
      call twrite(Irwfx,Core(ivee),lrwfx,1,lrwfx,1,0)
      endif
      
200   JUMP=0
      
      return
      
      end
C* :1 * 
      
