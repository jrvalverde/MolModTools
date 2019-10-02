
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 first"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "first.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 37 "first.web"
      subroutine first(NSYMOP,NEQBAS,JCYCLE)
      implicit none
      double precision A,a0,a1,a2,a3,Acurcy,Atmchg,B,beta,Big,C,cpulft,C
     &rit,Dd,delta,Deriv,Dfl,Dsr,dtime,Dumscr
      double precision E,ea,efph,eh,ehsav,elen,elm,Energy,ensav,esav,eti
     &me,Fillab,Four,gabs,gfloat,gmax1,gnorm,gsav,gsign,gsqrt
      integer i,i0,i1,i1i,i1r,i2,ia,Ian,Icharg,icls,icon,iconv,Icount,ic
     &y,idm2,idmet,Idscr,idump,ierr,Ieval
      integer Ievals,iexp,Ifill,Igeno,iguess,ii,im,In,Ioc,Ioc0,Iocs,Iod,
     &Iof1p,Iof1t,Iofa,Ione,Iop,Ioq,iord2,Ios
      integer Iouab,Ious,Iout,Iov,ipart,Iperm,ipfm,ipliml,iplimu,iprint,
     &Ipspin,Iptot,Ipunch,ir,iss,isym,itauf,Itcnt,j,jcnt
      integer JCYCLE,Jmat,keycon,Ksm,Kspin,Ksw,Lehf,Ligen,Lilsw,Lipcw,lm
     &,lm1,lmm,lmp,Lrep,Lrmsd,Ls2,Ltau,Lten,Lvir
      integer m1,m2,MAXBAS,Maxcyc,maxint,maxord,maxpt,md2,Mdim,mdm,Mdsq,
     &mexit,minint,minprt,minpt,mint,mord,mord1,Mshifs,msp
      integer mtd,Mtt,mtt1,Multip,Nae,Natoms,nb2,Nbasis,Nbe,Ne,NEQBAS,Ne
     &sk,Nest,Nest1,ng,np,np1,Nse,Nsep,Nsk
      integer NSYMOP,Ntt
      double precision One,Onept5,Pt5,ptime,q,Rms,rmsav,rmthrd,rmthrs,sc
     &alt,scalt0,scaltd,smald,Small,tau,tau0,tau1,tau2,Three,time0
      double precision Two,X,xi,Xl,xlam,xlambd,Y,ymin,Zero
      parameter(MAXBAS=150)
      dimension NEQBAS(MAXBAS,8)
      real cputim
      logical conju,minver,nomin,methc,Key,Cmp,Rhf,cd,Class,Extrap
      logical Skpsym
      dimension Dd(70),Iperm(70),elen(2),xlambd(10),elm(10),Nsk(2)
      dimension etime(3)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/con503/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/maxdm/Mdim,Mtt,Ntt,Mdsq,Mshifs
      common/memry/A(4970),B(4970),Fillab(40060)
      common/io/In,Iout,Ipunch
      common/rwf503/Igeno,Ieval,Ios,Ione,Iofa(4),Iod(4),Ioc(4),Iocs(4),I
     &oc0(4),Ioq(4),Ious(4),Jmat(4),Iof1p(4),Ievals,Iouab(4),Iptot(2),Ip
     &spin(2)
      common/cut/Energy,Deriv,Acurcy,Rms,Maxcyc,Icount,Class,Extrap
      common/scr/X(10,10),Y(10),Xl(10),Dfl(20),Dsr(140),E(140),Key,Itcnt
     &,Crit,Skpsym,Idscr,Dumscr(5209),Ifill
      common/lge503/Lilsw,Lipcw,Ltau,Lehf,Lvir,Lrep,Lten,Ls2,Ligen,Lrmsd
      equivalence(Nsk(1),Nae)
      equivalence(Dd(1),B(4901))
      equivalence(Iperm(1),X(1,1))
      equivalence(Iof1t,Iof1p(1)),(Ioc(1),Iov)
      data rmthrd/1.D-3/
      data tau2,mdm,mord1,maxord,maxpt/.3D0,10,9,9,10/
      data scaltd/2.1D0/
      data smald/1.D-18/
      data dtime/120./
      
      
      
      
      
      
      
      
99001 format(2I1,g18.5,2G20.5)
99002 format(40I2)
99003 format(g16.10)
99004 format(1x,72(1H*),/,1x,'CYC',1x,'PT',5x,'ELECTRONIC ENERGY',3x,'CO
     &NVERGENCE',1x,'PTS/DEG',2x,'LAMBDA',5x,'(DE/DL)',/,1x,72('*'))
99005 format(56(1H*)/1x,5HCYCLE,5x,17HELECTRONIC ENERGY,5x,11HCONVERGENC
     &E,2x,7HEXTRAP./56(1H*))
99006 format(' STEEPEST DESCENT')
99007 format(1x,i4,4x,d22.15)
99008 format(1x,i3,'(',i2,')',1x,d22.15)
99009 format(' ',61x,d11.4)
99010 format(1x,19(1H*)/' *ITERATION ABORTED*'/1x,19(1H*))
99011 format(' TAU(0)=',f7.4,6x,'MAX. DEGREE OF SEARCH POLYNOMIAL=',i2)
99012 format(' M.O. COEFFICIENTS ',a6/1x,17(1H*))
99013 format(' F(1),T'/1x,6(1H*))
99014 format(1x,i4,5x,17H(NON-VARIATIONAL))
99015 format(1x)
99016 format(' CONJUGATE DIRECTIONS')
99017 format(' ',42x,2I3,1x,d11.4)
99018 format(' ',25x,'METHOD B')
99019 format(' ',25x,'METHOD WITH GENERAL Q')
99020 format(' MAX-TIME EXIT')
99021 format(' ',49x,d11.4)
99022 format(' ALTERNATE TRIAL WAVEFUNCTION (TAU POSITIVE)'/1x,43('*'))
99023 format(' TAU NEGATIVE'/1x,12(1H*))
99024 format(' *** ENERGY RISES ***')
99025 format(' REPEAT THE PREVIOUS SEARCH WITH TAU =',d26.12)
99026 format(' CLASSICAL SCF')
99027 format(' ')
      idmet=Iop(8)
      icls=Iop(9)
      iexp=Iop(11)
      iord2=Iop(14)
      itauf=Iop(15)
      ipfm=Iop(19)
      mexit=Iop(20)
      iprint=Iop(33)
      idump=Iop(34)
      
      call ilsw(2,21,minprt)
      idm2=idmet/2
      call ilsw(2,7,isym)
      call ilsw(2,1,icon)
      icon=icon+1
      call ilsw(2,8,iguess)
      
      ipart=1
      if(Cmp)ipart=2
      mtd=Mtt+Mtt
      mtt1=Mtt+1
      md2=Ksm*Mdim
      nb2=Ksm*Nbasis
      gsav=One
      iss=0
      iconv=0
      ensav=Zero
      
      mord=0
      minpt=0
      tau1=Zero
      scalt0=Zero
      q=Zero
      if(mod(idmet,2).NE.0)read(In,99001)mord,minpt,tau1,scalt0,q
      if(mord.EQ.0)mord=mord1
      mord=min0(mord,maxord)
      mord=max0(mord,2)
      minpt=min0(minpt,mord)
      minpt=max0(minpt,2)
      tau0=tau2
      if(iguess.NE.0)then
      call tread(Igeno,A,Ligen,1,Ligen,1,0)
      if(A(Ltau).GT.Small)tau0=A(Ltau)
      endif
      if(tau1.GT.Zero)tau0=tau1
      scalt=scaltd
      if(scalt0.GT.Zero)scalt=scalt0
      
      msp=minpt*mord
      minint=minpt-1
      maxint=maxpt
      
      ipliml=1000
      iplimu=0
      if(ipfm.NE.0)read(In,99002)ipliml,iplimu
      if(iplimu.EQ.99)iplimu=1000
      
      rmthrs=rmthrd
      if(icls.EQ.2)read(In,99003)rmthrs
      
      Class=.FALSE.
      if(idm2.EQ.1)Class=.TRUE.
      conju=.FALSE.
      methc=.FALSE.
      if(.NOT.(Class))then
      if(idm2.EQ.2)conju=.TRUE.
      if(gabs(q).GT.Zero)methc=.TRUE.
      if(.NOT.conju)write(Iout,99006)
      if(conju)write(Iout,99016)
      if(.NOT.methc)write(Iout,99018)
      if(methc)write(Iout,99019)
      write(Iout,99011)tau0,mord
      endif
      if(Class)write(Iout,99026)
      if(isym.NE.0)write(Iout,99023)
      if(.NOT.Class.AND.minprt.EQ.0)write(Iout,99004)
      if(Class.AND.minprt.EQ.0)write(Iout,99005)
      
100   if(iss.NE.0)then
      do 150 Kspin=1,Ksm
      call tioc(Nbasis,2,Iocs,B,4,1,idump)
      call tioc(Nbasis,2,Ioc,A,4,1,idump)
      call tioc(Nbasis,1,Ioc,B,4,1,idump)
      call tioc(Nbasis,1,Iocs,A,4,1,idump)
150   continue
      endif
      
      
      
      
      JCYCLE=-1
      Skpsym=.TRUE.
      nomin=.TRUE.
      minver=.TRUE.
      mint=minint
      do 200 i=1,3
      etime(i)=0.0D00
200   continue
      
      
300   JCYCLE=JCYCLE+1
      ierr=0
      if((JCYCLE.EQ.0).OR.(Iop(21).EQ.2))call ofix(Nbasis,ierr,idump)
      if(ierr.NE.0.AND.Iop(21).EQ.0)call lnk1e
      m1=1
      do 400 Kspin=1,Ksm
      Nse=Nsk(Kspin)
      call tioc(Nbasis,2,Ioc,B,4,1,idump)
      call pmat(Nbasis,B,A)
      call tioc(Nbasis,1,Iod,A(m1),2,1,idump)
      m1=m1+Mshifs
400   continue
      if(JCYCLE.EQ.0)call conuso(Nbasis,iexp,keycon,1,idump)
      if(JCYCLE.NE.0)call conuso(Nbasis,iexp,keycon,2,idump)
      if(keycon.EQ.0)goto 1900
      if(JCYCLE.NE.0)then
      ptime=0.0D00
      do 450 i=1,3
      ptime=gmax1(ptime,etime(i))
450   continue
      ptime=ptime+dtime
      if(mexit.EQ.0)ptime=0.
      if(cpulft(i).LE.ptime)then
      write(Iout,99020)
      goto 1800
      endif
      endif
      
      time0=cputim(i)
      if(JCYCLE.GT.Maxcyc)goto 1800
      
      if(.NOT.(Class))then
      call bessrt(Nbasis,1,tau,A,B,Mdim,idump)
      
      do 500 i=1,maxpt
      xlambd(i)=Zero
500   continue
      endif
      esav=Zero
      jcnt=0
      lm=1
      
      
600   eh=Zero
      efph=Zero
      
      m1=1
      do 700 Kspin=1,Ksm
      if(lm.EQ.1)then
      
      call tioc(Nbasis,2,Iod,A(m1),2,1,idump)
      else
      Nse=Nsk(Kspin)
      call tioc(Nbasis,2,Ioc,B,4,1,idump)
      call pmat(Nbasis,B,A)
      call tioc(Nbasis,1,Iod,A(m1),2,1,idump)
      endif
      m1=m1+Mshifs
700   continue
      
      m1=1
      do 800 Kspin=1,Ksm
      call tread(Ione,B(m1),Mtt,1,Ntt,1,0)
      if(Cmp)then
      m2=m1+Mtt-1
      do 720 i=1,Ntt
      B(m2+i)=Zero
720   continue
      endif
      
      a0=A(m1)
      call matmul(Nbasis,A(m1),B(m1),0,2,2)
      eh=eh+A(m1)
      A(m1)=a0
      m1=m1+Mshifs
800   continue
      
      call formf(Nbasis,Ione,Iod,NSYMOP,NEQBAS)
      
      m1=1
      do 900 Kspin=1,Ksm
      call tioc(Nbasis,1,Iofa,B(m1),2,1,idump)
      call tioc(Nbasis,2,Iod,A(m1),2,1,idump)
      call matmul(Nbasis,A(m1),B(m1),0,0,2)
      efph=efph+A(m1)
      m1=m1+Mshifs
900   continue
      Energy=Pt5*(eh+efph)
      if(minprt.EQ.0)then
      if(Extrap)write(Iout,99014)JCYCLE
      if(.NOT.(Extrap.AND.iprint.LT.3))then
      if(Class)write(Iout,99007)JCYCLE,Energy
      if(.NOT.Class)write(Iout,99008)JCYCLE,lm,Energy
      endif
      endif
      lmm=mod(lm-1,maxpt)+1
      elm(lmm)=Energy
      
      
      
      
      if(minver)then
      minver=.FALSE.
      
      
      if(.NOT.(Class.OR.JCYCLE.LE.1))then
      if(ensav.LE.Energy-Small)then
      write(Iout,99024)
      icls=0
      Energy=ensav
      eh=ehsav
      elm(1)=Energy
      mint=maxint
      tau=tau/gfloat(mint)
      tau=gsign(tau,-Deriv)
      JCYCLE=JCYCLE-1
      write(Iout,99025)tau
      write(Iout,99008)JCYCLE,lm,Energy
      goto 1100
      endif
      endif
      
      ensav=Energy
      ehsav=eh
      rmsav=Rms
      if(.NOT.(.NOT.(Rms.LT.rmthrs.AND.icls.NE.0.AND.JCYCLE.GE.1).OR.Cla
     &ss))then
      Class=.TRUE.
      if(minprt.EQ.0)write(Iout,99005)
      write(Iout,99015)
      endif
      
      mint=mint-1
      mint=max0(mint,minint)
      tau=tau0
      if(JCYCLE.GT.0.AND.itauf.EQ.0)tau=scalt*xlam/gfloat(mint)
      
      do 950 Kspin=1,Ksm
      call tioc(Nbasis,2,Ioc,A,4,1,idump)
      if(iprint.GE.3)then
      write(Iout,99012)Ksw(Kspin)
      call cmat(A,Mdim,Mdim,Nbasis,Nbasis,Cmp)
      endif
      call tioc(Nbasis,1,Ioc0,A,4,1,idump)
950   continue
      
      
      do 1000 Kspin=1,Ksm
      Nest=Nesk(Kspin)
      Nse=Nsk(Kspin)
      Nest1=Nest+1
      Nsep=Nse+1
      
      call tioc(Nbasis,2,Iofa,A,3,1,idump)
      call tioc(Nbasis,2,Ioc0,B,4,1,idump)
      call matmul(Nbasis,A,B,0,0,0)
      call matmul(Nbasis,A,B,1,0,0)
      call asgsym(Nbasis,A,B,Mdim)
      call tioc(Nbasis,1,Iof1t,A,3,1,idump)
      if(.NOT.(Class))then
      
      call sls(1,A,Mdim,Nbasis)
      if(JCYCLE.LE.iplimu.AND.JCYCLE.GE.ipliml)then
      write(Iout,99013)
      call csymm(A,Mdim,Nbasis,Cmp)
      write(Iout,99027)
      endif
      
      call diablo(Nbasis,E(Nest1),Dd,X)
      call tioc(Nbasis,1,Ioq,B,4,1,idump)
      
      call tioc(Nbasis,2,Iof1t,A,3,1,idump)
      call matmul(Nbasis,A,B,0,0,0)
      call matmul(Nbasis,A,B,1,0,0)
      call tioc(Nbasis,1,Iof1p,A,3,1,idump)
      endif
1000  continue
      Skpsym=.FALSE.
      if(Class)then
      
      
      do 1020 Kspin=1,Ksm
      Nest1=Nesk(Kspin)+1
      call tioc(Nbasis,2,Iof1t,A,2,1,idump)
      call diagd(A,B,E(Nest1),Nbasis,Dd,X,Mdim,Cmp)
      call tioc(Nbasis,2,Ioc0,A,4,1,idump)
      call matmul(Nbasis,A,B,0,0,0)
      call tioc(Nbasis,1,Ioc,A,4,1,idump)
1020  continue
      minver=.TRUE.
      call twrite(Ieval,E,nb2,1,nb2,1,0)
      goto 300
      else
      
      call twrite(Ieval,E,nb2,1,nb2,1,0)
      
      
      delta=Zero
      gnorm=Zero
      a0=One
      
      do 1040 Kspin=1,Ksm
      Nest=Nesk(Kspin)
      Nse=Nsk(Kspin)
      Nsep=Nse+1
      call tioc(Nbasis,2,Iof1p,A,2,1,idump)
      do 1030 ia=Nsep,Nbasis
      i1r=ia*(ia-1)/2
      i1i=i1r+Mtt
      ea=E(ia+Nest)
      do 1025 ii=1,Nse
      a1=ea-E(ii+Nest)
      if(methc)a0=a1**q
      if(Cmp)gnorm=gnorm+(a0*A(i1i+ii))**2
      gnorm=gnorm+(a0*A(i1r+ii))**2
      delta=delta+a0
1025  continue
1030  continue
1040  continue
      
      delta=delta/gfloat(Nae*(Nbasis-Nae)+Nbe*(Nbasis-Nbe))
      if(Rhf)then
      gnorm=gnorm+gnorm
      delta=delta+delta
      endif
      gnorm=gsqrt(gnorm)/delta
      
      
      Deriv=Zero
      eh=Zero
      beta=gnorm/gsav
      gsav=gnorm
      cd=conju.AND..NOT.nomin
      a0=One
      
      do 1060 Kspin=1,Ksm
      Nest=Nesk(Kspin)
      Nse=Nsk(Kspin)
      Nsep=Nse+1
      call tioc(Nbasis,2,Iof1p,A,2,1,idump)
      if(cd)call tioc(Nbasis,2,Jmat,A(Mshifs+1),2,1,idump)
      i0=0
      do 1050 icy=1,ipart
      do 1045 ia=Nsep,Nbasis
      ea=E(ia+Nest)
      i1=i0+ia*(ia-1)/2
      i2=i1+Mshifs
      do 1042 ii=1,Nse
      ir=i1+ii
      a1=ea-E(ii+Nest)
      if(methc)a0=a1**q/delta
      a2=A(ir)
      a3=a2*a0
      if(cd)a3=a3+beta*A(i2+ii)
      Deriv=Deriv-a2*a3/a1
      A(ir)=a3
      eh=eh+a3**2
1042  continue
1045  continue
      i0=Mtt
1050  continue
      call tioc(Nbasis,1,Jmat,A,2,1,idump)
1060  continue
      Deriv=Deriv+Deriv
      if(Rhf)then
      Deriv=Deriv+Deriv
      eh=eh+eh
      endif
      if(cd)then
      
      beta=gnorm/gsqrt(eh)
      Deriv=Deriv*beta
      do 1070 Kspin=1,Ksm
      Nse=Nsk(Kspin)
      Nsep=Nse+1
      call tioc(Nbasis,2,Jmat,A,2,1,idump)
      do 1065 ia=Nsep,Nbasis
      i1r=ia*(ia-1)/2
      i1i=i1r+Mtt
      do 1062 ii=1,Nse
      ir=i1r+ii
      A(ir)=A(ir)*beta
      if(Cmp)then
      im=i1i+ii
      A(im)=A(im)*beta
      endif
1062  continue
1065  continue
      call tioc(Nbasis,1,Jmat,A,2,1,idump)
1070  continue
      endif
      if(minprt.EQ.0)write(Iout,99009)Deriv
      if(gabs(Deriv).LT.smald)goto 1900
      
      if(tau*Deriv.GT.Zero)tau=tau0
      tau=gsign(tau,-Deriv)
      endif
      endif
      
      
      
      
1100  lm=lm+1
      if(esav.LT.Energy.AND.lm.GT.minpt)goto 1600
      esav=Energy
      if(lm.GT.msp)goto 1600
      
      
      lmm=mod(lm-1,maxpt)+1
      lmp=mod(lm-2,maxpt)+1
      Itcnt=0
1200  Itcnt=Itcnt+1
      xlam=xlambd(lmp)+tau
      xlambd(lmm)=xlam
      Crit=Big
      
1300  do 1400 Kspin=1,Ksm
      Nest=Nesk(Kspin)
      Nse=Nsk(Kspin)
      Nest1=Nest+1
      Nsep=Nse+1
      
      call tioc(Nbasis,2,Jmat,A,2,1,idump)
      do 1350 ia=Nsep,Nbasis
      i1r=ia*(ia-1)/2
      i1i=i1r+Mtt
      do 1320 ii=1,Nse
      ir=i1r+ii
      A(ir)=A(ir)*xlam
      if(Cmp)then
      im=i1i+ii
      A(im)=A(im)*xlam
      endif
1320  continue
1350  continue
      
      call diagd(A,B,E(Nest1),Nbasis,Dd,X,Mdim,Cmp)
      call tioc(Nbasis,1,Iov,B,4,1,idump)
      if(iord2.LE.1)call bessrt(Nbasis,iord2+2,tau,A,B,Mdim,idump)
      if(Key)goto 1200
1400  continue
      if(.NOT.minver.AND.minprt.EQ.0)write(Iout,99021)xlam
      
      
      do 1500 Kspin=1,Ksm
      Nest=Nesk(Kspin)
      Nse=Nsk(Kspin)
      
      if(iord2.GT.1)then
      
      call tioc(Nbasis,2,Iov,B,4,1,idump)
      else
      call tioc(Nbasis,2,Iov,A,4,1,idump)
      call bessrt(Nbasis,4,tau,A,B,Mdim,idump)
      endif
      call tioc(Nbasis,2,Ioq,A,4,1,idump)
      call matmul(Nbasis,A,B,0,0,0)
      
      call tioc(Nbasis,2,Ioc0,B,4,1,idump)
      call matmul(Nbasis,B,A,0,0,0)
      call tioc(Nbasis,1,Ioc,B,4,1,idump)
1500  continue
      
      i=mod(JCYCLE,2)+1
      etime(i)=cputim(i)-time0
      if(.NOT.(minver))goto 600
      goto 300
      
      
1600  if(lm.GT.maxpt)then
      
      lm1=lm-1
      np=maxpt
      ng=np-1
      mint=lm-maxpt
      m1=mint-1
      do 1650 i=mint,lm1
      i1=mod(i-1,maxpt)+1
      i2=i-m1
      xi=xlambd(i1)
      Xl(i2)=xi
      Y(i2)=elm(i1)
      a0=One
      X(i2,1)=One
      do 1620 j=2,maxpt
      a0=a0*xi
      X(i2,j)=a0
1620  continue
1650  continue
      else
      np=lm-1
      np1=lm
      ng=np
      do 1700 i=1,np
      xi=xlambd(i)
      Xl(i)=xi
      Y(i)=elm(i)
      a0=One
      X(i,1)=One
      do 1660 j=2,np1
      a0=a0*xi
      X(i,j)=a0
1660  continue
1700  continue
      Y(np1)=Deriv
      X(np1,1)=Zero
      X(np1,2)=One
      if(np1.GE.3)then
      do 1720 i=3,np1
      X(np1,i)=Zero
1720  continue
      endif
      endif
      
      call locmin(ng,np,X,Y,Xl,nomin,xlam,ymin,iprint,mdm)
      if(minprt.EQ.0)write(Iout,99017)np,ng,xlam
      minver=.TRUE.
      Itcnt=0
      goto 1300
      
      
1800  iconv=1
      write(Iout,99010)
1900  if(isym.NE.0)then
      iss=iss+1
      elen(iss)=Energy
      if(iss.NE.2)then
      call tread(Ieval,E,nb2,1,nb2,1,0)
      call twrite(Ievals,E,nb2,1,nb2,1,0)
      write(Iout,99022)
      goto 100
      
      elseif(elen(2).GT.elen(1))then
      call tread(Ievals,E,nb2,1,nb2,1,0)
      do 1920 Kspin=1,Ksm
      call tioc(Nbasis,2,Iocs,A,4,1,idump)
      call tioc(Nbasis,2,Ioc,B,4,1,idump)
      call tioc(Nbasis,1,Iocs,B,4,1,idump)
      call tioc(Nbasis,1,Ioc,A,4,1,idump)
1920  continue
      endif
      endif
      
      
      call ilsw(1,5,iconv)
      
      call ilsw(1,7,0)
      
      call tread(Ieval,E,nb2,1,nb2,1,0)
      do 2000 Kspin=1,Ksm
      Nse=Nsk(Kspin)
      Nest=Nesk(Kspin)
      call tioc(Nbasis,2,Ioc0,B,4,1,idump)
      call aufbau(Nbasis,B,Mdim)
      call tioc(Nbasis,1,Ioc,B,4,1,idump)
2000  continue
      
      call twrite(Ieval,E,nb2,1,nb2,1,0)
      
      Acurcy=tau
      Maxcyc=JCYCLE-1
      
      return
      
      end
C* :1 * 
      
