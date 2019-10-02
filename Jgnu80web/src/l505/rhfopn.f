
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rhfopn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rhfopn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "rhfopn.web"
      subroutine rhfopn(JUMP)
      implicit none
      double precision acurcy,Atmchg,C,Cc,Coeff,D,Da,Dd,De,Dgen,energy,F
     &,Fa,Fb,Ff,Filabc,finac,t,ten,tenrgy
      double precision vir,zero
      integer i,Ian,Ibf,Icharg,Icnvg,Icyc,icyc1,icyc64,icyc8,Id505,Idum,
     &Idump,Iext,Ifcnvg,Ifill,Igen,Iguess,ihalt,Ij,In
      integer Iop,Iout,Ipch,Iprint,Ipunch,Irstrt,Irwc1,Irwc2,Irwc3,Irwc4
     &,Irwca,Irwcb,Irwev,Irwfa,Irwfb,Irwgen,Irwh,irwibf,Irwpa,Irwpb
      integer Irwpt,Irws,Irwt,Isym2e,isymm,itemp,j,jcycle,JUMP,k,LENB,li
     &m,LNEQ,MAXBAS,maxcyc,Maxnbf,MAXNTT,MAXPRM,MAXS21,MAXSH1
      integer MAXSHL,MEMLEN,Multip,Nae,Natoms,Nbasis,Nbe,Ne,neq,neqbas,n
     &sq,nsymop,Ntt,Nttmax
      
      
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      
      parameter(MAXBAS=150,MEMLEN=50000,MAXNTT=((MAXBAS*(MAXBAS+1))/2),L
     &NEQ=(4*MAXBAS+4*MAXSHL))
      
      integer Psave
      dimension neqbas(MAXBAS,8)
      dimension Da(1),D(1),Fa(1),Fb(1),Idum(1)
      dimension Igen(1)
      dimension irwibf(2)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/memry/Dd(70,70),De(70),F(70,70),Ff(70),Coeff(70,70),Cc(70),
     &Id505,Ifill,Filabc(35089)
      common/max505/Maxnbf,Nttmax
      common/gen/Dgen(47)
      common/ops505/Ipch,Iprint,Idump,Iguess,Icnvg,Icyc,Irstrt,Iext,Ifcn
     &vg
      common/irw505/Irwgen,Irws,Irwt,Irwh,Irwev,Irwca,Irwcb,Irwpa,Irwpb,
     &Irwpt,Irwfa,Irwfb,Irwc1,Irwc2,Irwc3,Irwc4
      common/ibf/Ibf(30)
      common/psave/Psave
      common/io/In,Iout,Ipunch
      common/jnkphf/Ntt,Ij(71)
      equivalence(Da(1),Dd(1,1)),(D(1),Dd(36,36))
      equivalence(Fa(1),F(1,1)),(Fb(1),F(36,36))
      equivalence(Idum(1),Coeff(1,1))
      equivalence(Igen(1),Dgen(1))
      equivalence(Isym2e,Ibf(30))
      data irwibf/508,15/,isymm/551/,neq/565/
      data ten/10.0D00/,finac/0.5D-06/,zero/0.0D00/
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(' ','SCF DONE:  E(RHF) = ',g19.12,' A.U. AFTER ',i4,' CYCLE
     &S'/1x,'           CONVG  = ',d13.4,12x,' -V/T = ',f7.4)
99002 format('  NTT = ',i6,' NE = ',i6,' NAE = ',i6,' NBE = ',i6)
99003 format(45H RESTRICTED HARTREE FOCK SCF FOR OPEN SHELLS.)
99004 format(1H )
99005 format(1H ,41HREQUESTED CONVERGENCE ON DENSITY MATRIX =,d11.4,8H W
     &ITHIN ,i3,8H CYCLES.)
99006 format(1x,4HITER,2x,17HELECTRONIC-ENERGY,7x,11HCONVERGENCE,4x,13HE
     &XTRAPOLATION/1x,4(1H-),2x,17(1H-),7x,11(1H-),4x,13(1H-))
99007 format(43H0CONVERGENCE FAILURE ... ERROR TERMINATION.)
99008 format('+',20x,g19.12)
      call drum
      
      ihalt=iabs(Iop(13)-1)
      Ipch=Iop(32)
      Iprint=Iop(33)
      Idump=Iop(34)
      if(Idump.GT.2)Iprint=4
      Iguess=Iop(5)
      Icnvg=Iop(6)
      Icyc=Iop(7)
      Iext=Iop(15)
      icyc64=Iop(22)
      icyc8=Iop(23)
      icyc1=Iop(24)
      Ifcnvg=Iop(16)
      Ntt=(Nbasis*(Nbasis+1))/2
      call ilsw(2,21,Psave)
      
      call ilsw(1,1,0)
      call ilsw(1,22,1)
      
      lim=3*(Nbasis**2+Nbasis)
      do 100 i=1,lim
      Da(i)=zero
100   continue
      
      call twrite(Irwfa,Da(1),Nttmax,1,Ntt,1,0)
      call twrite(Irwfb,Da(1),Nttmax,1,Ntt,1,0)
      
      call twrite(Irwc1,Da(1),2*Nttmax,1,2*Ntt,1,0)
      call twrite(Irwc2,Da(1),2*Nttmax,1,2*Ntt,1,0)
      call twrite(Irwc3,Da(1),2*Nttmax,1,2*Ntt,1,0)
      call twrite(Irwc4,Da(1),2*Nttmax,1,2*Ntt,1,0)
      
      write(Iout,99003)
      call phfchk(Nbasis)
      
      do 200 i=1,71
      Ij(i)=(i*(i-1))/2
200   continue
      
      call tread(Irwpa,Da,Nttmax,1,Ntt,1,0)
      call tread(Irwpb,D,Nttmax,1,Ntt,1,0)
      
      acurcy=finac
      if(Icnvg.NE.0)acurcy=ten**(-2*Icnvg)
      
      maxcyc=20
      if(Icyc.LT.1)then
      elseif(Icyc.EQ.1)then
      maxcyc=1
      else
      
      maxcyc=2**(Icyc-1)
      if(Icyc.EQ.7)maxcyc=64*icyc64+8*icyc8+icyc1
      endif
      
      call tread(irwibf(1),Ibf(1),irwibf(2),1,irwibf(2),1,0)
      
      if(Isym2e.NE.0)then
      call tread(isymm,Idum(1),1,1,1,1,0)
      nsymop=Idum(1)
      call tread(neq,Idum(1),LNEQ,1,LNEQ,1,0)
      k=8*MAXSHL
      do 250 j=1,8
      do 220 i=1,MAXBAS
      k=k+1
      neqbas(i,j)=Idum(k)
220   continue
250   continue
      endif
      
      if(Iprint.GE.3)write(Iout,99002)Ntt,Ne,Nae,Nbe
      write(Iout,99005)acurcy,maxcyc
      if(Psave.EQ.0)write(Iout,99006)
      
      call cycphf(jcycle,energy,acurcy,maxcyc,Nae,Nbe,Nbasis,nsymop,neqb
     &as)
      
      call tread(Irwpa,Da,Ntt,1,Ntt,1,0)
      call tread(Irwpb,Fa,Ntt,1,Ntt,1,0)
      call aadd(Ntt,Da,Fa,Da)
      call twrite(Irwpt,Da,Ntt,1,Ntt,1,0)
      
      call tread(Irwgen,Igen(1),47,1,47,1,0)
      tenrgy=energy+Dgen(41)
      
      call virial(Nbasis,tenrgy,Da,Fa,Irwpt,Irwt,vir,t)
      
      write(Iout,99004)
      write(Iout,99008)(tenrgy,i=1,4)
      write(Iout,99001)tenrgy,jcycle,acurcy,vir
      
      Dgen(32)=tenrgy
      Dgen(40)=vir
      Dgen(42)=t
      Dgen(43)=tenrgy
      
      call twrite(Irwgen,Igen(1),47,1,47,1,0)
      
      if(Iprint.NE.0)call phfprt(Nbasis)
      
      nsq=Nbasis*Nbasis
      if(Ipch.LT.1)then
      elseif(Ipch.EQ.1)then
      call tread(Irwev,Da,Nttmax,1,2*Nbasis,1,0)
      call binwt(Da,2*Nbasis,1HE)
      call tread(Irwca,Da,nsq,1,nsq,1,0)
      call binwt(Da,2*nsq,1HC)
      else
      
      if(Ipch.LT.2)goto 300
      if(Ipch.NE.2)then
      
      call tread(Irwev,Da,Nttmax,1,2*Nbasis,1,0)
      call binwt(Da,2*Nbasis,1HE)
      call tread(Irwca,Da,nsq,1,nsq,1,0)
      call binwt(Da,2*nsq,1HC)
      endif
      call tread(Irwpa,Da,Nttmax,1,Ntt,1,0)
      call binwt(Da,2*Ntt,1HP)
      call tread(Irwpb,Da,Nttmax,1,Ntt,1,0)
      call binwt(Da,2*Ntt,1HP)
      endif
      
      
300   call ilsw(2,5,itemp)
      if(ihalt*itemp.NE.0)then
      write(Iout,99007)
      call lnk1e
      endif
      
      JUMP=0
      return
      
      end
C* :1 * 
      
