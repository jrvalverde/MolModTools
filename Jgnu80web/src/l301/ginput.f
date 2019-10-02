
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ginput"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ginput.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 59 "ginput.web"
      subroutine ginput(JUMP)
      implicit none
      double precision Atmchg,b1,b3,b4,C,C1,C2,C3,C4,Dexpb,Dexpbe,Dexpli
     &,Dgen,dtemp,dtemp1,dtemp2,enr,Exx,Pdexp,Pdexp1
      double precision Pdexp2,Ppexp,ptemp,rone,Scal1,Scal1x,Scal2,Scal2x
     &,Scal3,Scal3x,Scal4,Scal4x,Scale,Shladf,thr,X,x1,x2,x3,x4
      double precision x5,Y,Z,zero
      integer i,I2edsc,I2esf,I5d6d,ia,iabort,Ian,Iao,Iaos,Ibasis,Ibmod,I
     &bpr,ic,icent,Icharg,Icount,id,ideriv,iel,Ifbp
      integer iff,ifpol,ii,In,iobas,iolbl,Iop,Iosc,ioscal,Iout,Ipt,Ipunc
     &h,Irot,Irtcrd,irwgen,Iscal,Ititle,Jan,Jprinp,Jpseud
      integer Jpunch,Jreadp,Jsubp,JUMP,Label,LENB,Llink,MAXPRM,MAXS21,MA
     &XSH1,MAXSHL,Maxtyp,Multip,Nae,Natoms,Nbasis,Nbe,nchain,Ne,nextov
      integer Ngic,nosym,nprims,Nshell,Numd,numf
      integer Psave
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon
      integer*4 basnam
      dimension b1(36,4),b3(36,2),b4(36,2)
      dimension basnam(4)
      dimension ic(15),id(4)
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
      common/scalsp/Scal1(35),Scal1x(35),Scal2(35),Scal2x(35),Scal3(35),
     &Scal3x(35),Scal4(35),Scal4x(35),Iscal(35),Icount
      common/iao/Iao(4)
      common/io/In,Iout,Ipunch
      common/scale/Scale(MAXSHL)
      common/numd/Numd
      common/iaos/Iaos(MAXSHL)
      common/polexp/Ppexp,Pdexp,Pdexp1,Pdexp2
      common/ops301/Ibasis,Ngic,Ipt,I5d6d,Iosc,Ibmod,Ibpr,Llink,I2edsc,I
     &rot,Jpunch,I2esf
      common/libeb/Dexpli,Dexpbe,Dexpb
      common/label/Label(1000),Ititle(100),Irtcrd(100)
      common/psave/Psave
      common/ifbp/Ifbp
      common/potpar/Jpseud,Jreadp,Jprinp,Jsubp
      common/gen/Dgen(47)
      data id/5,6,5,6/
      data thr/1.D-8/
      data b1/1.24D0,1.69D0,2.69D0,3.68D0,4.68D0,5.67D0,6.67D0,7.66D0,8.
     &65D0,9.64D0,10.61D0,11.59D0,12.56D0,13.53D0,14.50D0,15.47D0,16.43D
     &0,17.40D0,18*0.0D0,.00D0,.00D0,.80D0,1.15D0,1.50D0,1.72D0,1.95D0,2
     &.25D0,2.55D0,2.88D0,3.48D0,3.90D0,4.36D0,4.83D0,5.31D0,5.79D0,6.26
     &D0,6.74D0,24*0.0D0,.00D0,.00D0,.00D0,.00D0,1.75D0,1.70D0,1.70D0,1.
     &75D0,1.90D0,2.05D0,2.10D0,2.33D0,54*0.0D0/
      data b3/1.2D0,17*1.0D0,18*0.0D0,1.15D0,17*1.0D0,18*0.0D0/
      data b4/1.20D0,1.00D0,1.03D0,1.03D0,1.03D0,1.00D0,.99D0,.99D0,1.00
     &D0,1.00D0,.00D0,.00D0,.00D0,.00D0,.98D0,.98D0,1.00D0,.00D0,.00D0,.
     &00D0,.00D0,.00D0,.00D0,.00D0,.00D0,.00D0,.00D0,.00D0,.00D0,.00D0,.
     &00D0,.00D0,.00D0,.00D0,.00D0,.00D0,1.15D0,1.00D0,1.12D0,1.12D0,1.1
     &2D0,1.04D0,.98D0,.98D0,1.00D0,1.00D0,.00D0,.00D0,.00D0,.00D0,1.02D
     &0,1.01D0,1.01D0,.00D0,.00D0,.00D0,.00D0,.00D0,.00D0,.00D0,.00D0,.0
     &0D0,.00D0,.00D0,.00D0,.00D0,.00D0,.00D0,.00D0,.00D0,.00D0,.00D0/
      data rone/1.0D0/
      data basnam/'BASI','S   ','    ','    '/
      data zero/0.0D0/
      data iolbl/502/
      data ioscal/505/
      data iobas/506/
      data irwgen/501/
      
      
      
99001 format(1H1)
99002 format(15I2,5D10.4)
99003 format(4D20.10)
99004 format(36H REPLACEMENT POLARIZATION EXPONENTS:/1x,4D19.10)
99005 format(1x,i3,' BASIS FUNCTIONS',5x,i4,' PRIMITIVE GAUSSIANS'/1x,i3
     &,' ALPHA ELECTRONS',5x,i4,' BETA ELECTRONS'/1x,3x,' NUCLEAR REPULS
     &ION ENERGY ',f15.10,' HARTREES')
99006 format(1x,'REPLACEMENT SCALE-FACTORS: '/1x,15I2,5F10.4)
99007 format(1x,' *** ERROR TERMINATION: PROGRAM CANNOT ',/,1x,'  PRESEN
     &TLY DO DERIVATIVES OF EFFECTIVE POTENTIAL',/,1x,'  INTEGRALS BEYON
     &D S P AND D BASIS FUNCTIONS ***')
99008 format(' DUMP /LABEL/'/20(5x,25A4/))
      
      call aclear(LENB,Exx)
      
      call drum
      call ilsw(2,21,Psave)
      Ibasis=Iop(5)
      Ngic=Iop(6)
      Ipt=Iop(7)
      I5d6d=Iop(8)
      iff=Iop(9)
      numf=7
      if(iff.EQ.2)numf=10
      Iosc=0
      Ibmod=Iop(10)
      Jpseud=Iop(16)
      Jreadp=Iop(17)
      Jprinp=Iop(18)
      Jsubp=Iop(19)
      I2esf=Iop(23)
      Ibpr=Iop(24)
      iabort=Iop(26)
      if(iabort.EQ.1.AND.Ibpr.EQ.0)Ibpr=1
      Llink=Iop(25)
      I2edsc=Iop(27)
      Irot=Iop(29)
      Jpunch=Iop(32)
      I5d6d=I5d6d-1
      if(I5d6d.LT.0)then
      I5d6d=0
      if(Ibasis.EQ.1.AND.Ipt.GT.0)I5d6d=1
      endif
      if(iff.EQ.0)call ilsw(1,16,0)
      if(iff.EQ.1)call ilsw(1,16,0)
      if(iff.EQ.2)call ilsw(1,16,1)
      
      
      call ilsw(1,2,I5d6d)
      call ilsw(1,3,Ibasis)
      if(Ibasis.EQ.7)call ilsw(1,3,2)
      ifpol=0
      if(Ipt.EQ.1)ifpol=6
      if(Ipt.EQ.2)ifpol=7
      call ilsw(1,4,ifpol)
      
      
      
      ii=Iosc
      if(Iosc.EQ.0)ii=2
      if(Iosc.EQ.2)ii=0
      do 100 i=1,80
      Shellc(i)=ii
100   continue
      call basprt
      Numd=id(I5d6d+1)
      Ppexp=1.1D0
      Pdexp=0.8D0
      Pdexp1=0.09E0
      Pdexp2=0.39E0
      if(Ibmod.GE.2.AND.Ibasis.NE.7)then
      
      
      read(In,99003)ptemp,dtemp,dtemp1,dtemp2
      write(Iout,99004)ptemp,dtemp,dtemp1,dtemp2
      if(ptemp.GE.thr)Ppexp=ptemp
      if(dtemp.GE.thr)Pdexp=dtemp
      if(dtemp1.GE.thr)Pdexp1=dtemp1
      if(dtemp2.GE.thr)Pdexp2=dtemp2
      endif
      Dexpli=0.2D0
      Dexpbe=0.4D0
      Dexpb=0.6D0
      if(Irot.NE.0)then
      call rotcor(Natoms,C)
      write(Iout,99001)
      endif
      if(Ibasis.NE.7)then
      
      if(Ibasis.NE.4)then
      if(Ibasis.NE.5)then
      if(Ibasis.GT.0)then
      if(Ibasis.NE.2)then
      if(Ibasis.EQ.6)goto 120
      if(Ibasis.EQ.3)goto 120
      
      do 102 i=1,Natoms
      ia=Ian(i)
      if(ia.LE.2)then
      Scal1(i)=b4(ia,1)
      Scal1x(i)=b4(ia,2)
      else
      
      Scal1(i)=rone
      if(ia.LE.10)then
      Scal2(i)=b4(ia,1)
      Scal2x(i)=b4(ia,2)
      else
      
      Scal2(i)=rone
      if(ia.LE.18)then
      Scal3(i)=b4(ia,1)
      Scal3x(i)=b4(ia,2)
      else
      
      Scal3(i)=rone
      Scal4(i)=b4(ia,1)
      Scal4x(i)=b4(ia,2)
      endif
      endif
      endif
102   continue
      goto 150
      endif
      endif
      do 110 i=1,Natoms
      ia=Ian(i)
      Scal1(i)=b1(ia,1)
      Scal2(i)=b1(ia,2)
      Scal3(i)=b1(ia,3)
      Scal4(i)=b1(ia,4)
110   continue
      goto 150
      else
      call n21g(Iop,Natoms,Ian,C,Nbasis)
      goto 200
      endif
      
      
120   do 140 i=1,Natoms
      ia=Ian(i)
      if(ia.LE.2)then
      Scal1(i)=b3(ia,1)
      Scal1x(i)=b3(ia,2)
      else
      
      Scal1(i)=zero
      if(ia.LE.10)then
      Scal2(i)=b3(ia,1)
      Scal2x(i)=b3(ia,2)
      else
      
      Scal2(i)=zero
      if(ia.LE.18)then
      Scal3(i)=b3(ia,1)
      Scal3x(i)=b3(ia,2)
      else
      
      Scal3(i)=zero
      Scal4(i)=b3(ia,1)
      Scal4x(i)=b3(ia,2)
      endif
      endif
      endif
140   continue
      else
      call fc6311(Iop,Natoms,Ian,C,Nbasis)
      goto 200
      endif
150   if(Ibmod.EQ.3.OR.Ibmod.EQ.1)then
160   read(In,99002)(ic(i),i=1,15),x1,x2,x3,x4,x5
      do 180 i=1,15
      if(ic(i).EQ.-0)ic(i)=0
180   continue
      write(Iout,99006)(ic(i),i=1,15),x1,x2,x3,x4,x5
      if(ic(1).NE.0)then
      do 190 i=1,15
      if(ic(i).EQ.0)goto 160
      icent=ic(i)
      Scal1(icent)=x1
      if(Iop(5).LE.0)then
      Scal2(icent)=x2
      Scal3(icent)=x3
      Scal4(icent)=x4
      
      elseif(Ian(icent).LE.2)then
      Scal1x(icent)=x2
      
      elseif(Ian(icent).LE.10)then
      Scal2(icent)=x2
      Scal2x(icent)=x3
      
      elseif(Ian(icent).LE.18)then
      Scal2(icent)=x2
      Scal3(icent)=x3
      Scal3x(icent)=x4
      else
      
      Scal2(icent)=x2
      Scal3(icent)=x3
      Scal4(icent)=x4
      Scal4x(icent)=x5
      endif
190   continue
      endif
      endif
      call twrite(ioscal,Scal1,298,1,298,1,0)
      if(Ibasis.LE.0)then
      
      call sto(Iop(5),Natoms,Ian,C,Nbasis,Ngic,Ipt)
      elseif(Ibasis.NE.2)then
      if(Ibasis.EQ.1)then
      
      call leeao(Natoms,Ian,C,Nbasis)
      else
      
      
      
      
      
      call losbas(Ngic,Ipt,Numd,numf,Natoms,Ian,C,Nbasis)
      endif
      else
      call sto(Iop(5),Natoms,Ian,C,Nbasis,Ngic,Ipt)
      endif
      else
      call gbasis(Iop,C,Ian,Natoms,Nbasis)
      endif
      
200   if(Ibpr.EQ.10)call prgbas
      
      call bprint(Ibpr,nprims)
      Ne=0
      do 300 i=1,Natoms
      Ne=Ne+Ian(i)
300   continue
      Ne=Ne-Icharg
      Nae=(Ne+Multip-1)/2
      Nbe=(Ne-Multip+1)/2
      call renorm
      Maxtyp=Shellt(1)
      do 400 i=2,Nshell
      Maxtyp=max0(Maxtyp,Shellt(i))
400   continue
      call twrite(iobas,Exx,LENB,1,LENB,1,0)
      call twrite(iolbl,Label,600,1,600,1,0)
      if(Iop(34).NE.0)write(Iout,99008)Ititle,Label,iel
      if(Iop(34).GE.2)call bdump(2)
      
      
      
      if(Jpunch.NE.0)call binwt(Exx,2962,basnam,Nbasis)
      
      
      if(Jpseud.NE.0)call pinput(Natoms,Ian,C,Nae,Nbe,Ne,Atmchg)
      if(Jpseud.NE.0)then
      call ilsw(2,23,ideriv)
      if(ideriv.NE.0)then
      if(Maxtyp.GE.3)then
      write(6,99007)
      call lnk1e
      endif
      endif
      endif
      call repuls(Natoms,Atmchg,C,enr)
      Dgen(41)=enr
      call twrite(irwgen,Dgen(1),47,1,47,1,0)
      write(Iout,99005)Nbasis,nprims,Nae,Nbe,enr
      
      call bldmap(X,Shellt,Shellc,Nshell,I5d6d)
      call ilsw(2,26,nosym)
      if(nosym.EQ.0)call bassym(Natoms,Iop,Nbasis,Nshell,X,Y,Z)
      if(Iop(50).EQ.0)call set2e(Iop)
      JUMP=0
      if(iabort.NE.1)return
      nchain=nextov(JUMP)
      write(Iout,99009)
      
99009 format(' *** gnu80 STOPS EXECUTING, THIS WAS A TEST JOB')
      
      stop 301
      
      end
C* :1 * 
      
