
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 frcout"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "frcout.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "frcout.web"
      subroutine frcout(JUMP)
      implicit none
      double precision Alpha,Atmchg,Beta,Bl,C,Core,Energy,Ex,f,Ffx,fmax,
     &fmaxi,Force,Frcnst,frms,frmsi,Fx,Fxyz,Gen,one
      double precision Phycon,toang,trot,zero
      integer i,i1,i2,i3,i4,i5,i6,i7,i8,Ian,Ianz,Icharg,idump,iend,ifd2e
     &,ifrc,ifreq,igeig,igrd,igrdnt
      integer In,iogen,Iop,Iout,iozmat,iprint,Ipunch,irwfx,iscf,isymm,Iz
     &,jtrans,JUMP,Lalpha,Lbeta,Lbl,lgrdnt,lrwfx,maxnz,mout
      integer msym,Multip,Mxcore,Nae,nat3,Natoms,Nbasis,Nbe,Ndum,Ne,neqa
     &tm,nnprm,nosym,nparm,Nsubs,ntt,Nvar,nwib,Nz
      logical intrnl
      dimension f(105),iozmat(2)
      character*6 nam2e(2)
      dimension jtrans(3,8),neqatm(100,8),mout(846),trot(3,3)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/io/In,Iout,Ipunch
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alpha(50),Beta(50),Lbl(50),La
     &lpha(50),Lbeta(50),Nz,Nsubs
      common/grdnt/Energy,Force(50),Frcnst(1275),Nvar,Ndum
      common/gen/Gen(47)
      common/force/Ex,Fx(105),Ffx(5565)
      common/fxyz/Fxyz(105)
      common/phycon/Phycon(30)
      common/memry/Core(49999),Mxcore
      equivalence(msym,mout(1))
      equivalence(jtrans(1,1),mout(2))
      equivalence(neqatm(1,1),mout(26))
      equivalence(trot(1,1),mout(829))
      data zero,one/0.D0,1.D0/
      data maxnz/50/,nwib/3200/
      data nam2e/6HDCLOSE,6HDOPEN /
      data igrdnt/511/,lgrdnt/1327/,iogen/501/
      data iozmat(1),iozmat(2)/507,351/
      data irwfx/521/,lrwfx/5671/,isymm/551/
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(1x,10x,'MAX ',f12.6,5x,'RMS ',f12.6)
99002 format(1x,17x,'MAX ',f12.6,5x,'RMS ',f12.6)
99003 format(' FORCE CONSTANTS IN CARTESIAN COORDINATES (','HARTREES/BOH
     &R)')
99004 format(' FORCE CONSTANTS IN INTERNAL COORDINATES (','ATOMIC UNITS)
     &')
99005 format('0 ERROR IN CONVERSION TO INTERNAL COORDINATES'/'  **** STO
     &P')
99006 format(' ***** AXES RESTORED TO ORIGINAL SET *****')
      
      Mxcore=49999
      
      toang=Phycon(1)
      call ilsw(2,26,nosym)
      if(nosym.NE.1)call tread(isymm,msym,423,1,423,1,0)
      call ilsw(2,24,ifrc)
      if(ifrc.NE.1)then
      iprint=Iop(33)
      ifd2e=Iop(25)
      idump=Iop(34)
      ifreq=Iop(8)
      igeig=Iop(31)
      nat3=3*Natoms
      ntt=(Nbasis*(Nbasis+1))/2
      nparm=nat3
      do 50 i=1,nat3
      Fx(i)=zero
50    continue
      
      
      if(Iop(29).NE.6)then
      
      
      
      call ilsw(2,1,iscf)
      
      
      if(Iop(29).EQ.5)then
      i1=1
      i2=i1+ntt
      i3=i2+ntt
      iend=i3+((Nbasis+1)/2)-1
      call tstcor(iend,Mxcore,6HDRVSRD)
      call aclear(nat3,Fxyz)
      call frcnn(Natoms,Atmchg,C,Fxyz)
      call drvsrd(Natoms,Nbasis,Fxyz,Core(i1),Core(i2),Core(i3),idump)
      endif
      if(Iop(29).GE.3)then
      i1=1
      i2=i1+ntt
      i3=i2+nwib
      iend=i3+Nbasis
      if(iscf.NE.0)then
      i4=i3+Nbasis+1
      iend=i4+ntt-1
      endif
      call tstcor(iend,Mxcore,nam2e(iscf+1))
      if(iscf.EQ.0)call dclose(Natoms,Nbasis,Fxyz,Core(i1),Core(i2),Core
     &(i2),Core(i3),idump)
      if(iscf.EQ.1)call dopen(Natoms,Nbasis,Fxyz,Core(i1),Core(i4),Core(
     &i2),Core(i2),Core(i3),idump)
      endif
      
      
      if(Iop(29).NE.6)call tread(irwfx,Ex,lrwfx,1,lrwfx,1,0)
      else
      call frcnn(Natoms,Atmchg,C,Fxyz)
      call ffrcnn(Natoms,Ian,C,Ffx)
      endif
      
      do 100 i=1,nat3
      Fx(i)=Fx(i)+Fxyz(i)
100   continue
      
      
      if(ifreq.NE.0)then
      call twrite(irwfx,Ex,lrwfx,1,lrwfx,1,0)
      i1=1
      i2=i1+Natoms
      i3=i2+nat3*nat3
      i4=i3+nat3*nat3
      i5=i4+nat3*nat3
      i6=i5+nat3
      i7=i6+nat3*3
      i8=i7+nat3
      iend=i8+3*nat3-1
      call tstcor(iend,Mxcore,6HVIBFRQ)
      call vibfrq(Natoms,Multip,Ian,C,nat3,Ffx,Core(i1),Core(i2),Core(i3
     &),Core(i4),Core(i5),Core(i6),Core(i7),Core(i8),Phycon)
      call tread(irwfx,Ex,lrwfx,1,lrwfx,1,0)
      endif
      
      
      if(nosym.NE.1)then
      write(Iout,99006)
      call rotf(Natoms,trot,Fx,Fx)
      call rotf(Natoms,trot,Fxyz,Fxyz)
      if(ifd2e.NE.0)call rotff(Natoms,trot,Ffx,Ffx)
      endif
      
      call twrite(irwfx,Ex,lrwfx,1,lrwfx,1,0)
      
      call ascale(nat3,-one,Fx,Fx)
      call ascale(nat3,-one,Fxyz,Fxyz)
      
      call rmsvec(nat3,Fx,frms,fmax)
      intrnl=.FALSE.
      write(Iout,99001)fmax,frms
      call fcorpr(Natoms,Ian,Fx,Iout)
      
      
      if(Iop(7).EQ.0)then
      call tread(iozmat(1),Ianz,iozmat(2),1,iozmat(2),1,0)
      if(idump.NE.0)call zprint(Nz,Ianz,Iz,Bl,Alpha,Beta,toang)
      nparm=3*Nz-6
      nparm=max(nparm,1)
      intrnl=.TRUE.
      
      i1=1
      i2=i1+3*4*nparm
      i3=i2+4*nparm
      i4=i3+nparm**2
      i5=i4+5*Nz
      i6=i5+3*Nz
      i7=i6+3*Nz
      i8=i7+nparm
      iend=i8+nparm
      call tstcor(iend,Mxcore,6HFORMBG)
      call formbg(maxnz,Nz,Ianz,Iz,Bl,Alpha,Beta,nparm,igeig,Core(i1),Co
     &re(i2),Core(i3),Core(i4),Core(i5),Core(i6),Core(i7),Core(i8),idump
     &,toang)
      call tranf(nparm,Nz,Ianz,Fx,f,Core(i2),Core(i1),Core(i3),Core(i7))
      call rmsvec(nparm,f,frmsi,fmaxi)
      endif
      if(intrnl)then
      call fzprnt(maxnz,Nz,Ianz(1),Iz(1,1),f,Iout)
      write(Iout,99002)fmaxi,frmsi
      endif
      
      
      
      if(ifd2e.NE.0)then
      nnprm=nat3*(nat3+1)/2
      write(Iout,99003)
      call ltoutd(nat3,Ffx,1)
      if(intrnl)then
      write(Iout,99004)
      i1=1
      i2=i1+3*Nz
      i3=i2+nparm
      iend=i3+3*Nz*nparm-1
      call tstcor(iend,Mxcore,6HTRANFF)
      call tranff(maxnz,Nz,Ianz,Iz,Bl,Alpha,Beta,nparm,Fxyz,Ffx,Core(i1)
     &,Core(i2),Core(i3),Mxcore-iend,Core(iend+1),idump,toang)
      call ltoutd(nparm,Ffx,1)
      endif
      endif
      endif
      
      
      call ilsw(2,23,igrd)
      call tread(iogen,Gen(1),47,1,47,1,0)
      Gen(21)=frms
      call twrite(iogen,Gen(1),47,1,47,1,0)
      if(igrd.NE.0)then
      call tread(igrdnt,Energy,lgrdnt,1,lgrdnt,1,0)
      Energy=Gen(43)
      
      
      if(ifd2e.NE.0)Energy=Gen(32)
      call ilsw(2,24,ifrc)
      if(ifrc.NE.1)then
      call putf(Nz,Lbl,Lalpha,Lbeta,nparm,Nvar,f,Force,iprint)
      if(ifd2e.NE.0)then
      i1=1
      i2=i1+nparm
      i3=i2+Nvar
      iend=i3+nparm*Nvar-1
      call tstcor(iend,Mxcore,6HPUTFF )
      call putff(Nz,Lbl,Lalpha,Lbeta,nparm,Nvar,Ffx,Frcnst,Core(i1),Core
     &(i2),Core(i3),iprint)
      endif
      endif
      call twrite(igrdnt,Energy,lgrdnt,1,lgrdnt,1,0)
      endif
      JUMP=0
      return
      
      end
C* :1 * 
      
