
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 scfdm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "scfdm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 33 "scfdm.web"
      subroutine scfdm(JUMP)
      implicit none
      double precision a0,a1,Acurcy,Atmchg,Big,C,Da,Db,Degen,degend,Deri
     &v,Dmatml,Dumscr,E,Energy,Fa,Fb,Fillab,finac,Four
      double precision Fuzzy,fuzzyd,Gen,One,Onept5,Pt5,rep,Rms,Small,Spa
     &n,spand,Sthrs,sthrsd,ten,tenrgy,Three,Two,vir,Zero
      integer i,i1,Ian,Ibf,iblnk,Icharg,icons,iconsd,iconv,Icount,icuts,
     &Idum,idump,ientry,Ieval,Ievals,Igen,Igeno,il,In
      integer incy1,incy64,incy8,inhib,Ioc,Ioc0,Iocs,Iod,Iof1p,Iofa,Ione
     &,Iop,Ioq,Ios,Iouab,Ious,Iout,ipart,ipch,iprint
      integer ipsav,Ipspin,Iptot,Ipunch,iq,Irwibf,irww,istab,Isym,Isym2e
     &,Isymm,j,jcycle,Jmat,JUMP,k,Ksm,Kspin,Ksw,labdck
      integer ldns,Lehf,LENB,LENFIL,Lenibf,levals,Ligen,Lilsw,Lipcw,lmo,
     &LNEQ,Lrep,Lrmsd,Ls2,Ltau,Lten,Lvir,macc,MAXBAS,Maxcyc
      integer MAXNTT,MAXPRM,MAXS21,MAXSH1,MAXSHL,mcyc,Mdim,Mdsq,MEMLEN,m
     &inprt,minpsv,MPLUS1,mrest,Mshifs,Mtt,Multip,Nae,namscf,Natoms,nb2
      integer nb22,Nbasis,Nbe,nbsq,Ne,Neq,neqbas,Nesk,Nest,Nest1,Nse,Nse
     &p,nsymop,Ntt
      
      
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      
      parameter(MAXBAS=150,MEMLEN=50000,MAXNTT=((MAXBAS*(MAXBAS+1))/2),L
     &ENFIL=(MEMLEN-4*MAXNTT),LNEQ=(4*MAXBAS+4*MAXSHL),MPLUS1=(8*MAXSHL+
     &1))
      
      logical Cmp,Rhf,Class,Extrap,ipr
      dimension Igen(280)
      dimension labdck(4,11)
      dimension lmo(2,2),ldns(2,2)
      dimension neqbas(MAXBAS,8),Idum(10424),namscf(4)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/maxdm/Mdim,Mtt,Ntt,Mdsq,Mshifs
      common/memry/Da(2485),Db(2485),Fa(2485),Fb(2485),Fillab(40060)
      common/rwf503/Igeno,Ieval,Ios,Ione,Iofa(4),Iod(4),Ioc(4),Iocs(4),I
     &oc0(4),Ioq(4),Ious(4),Jmat(4),Iof1p(4),Ievals,Iouab(4),Iptot(2),Ip
     &spin(2)
      common/io/In,Iout,Ipunch
      common/con503/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/symmet/Isym(140)
      common/scr/Dmatml(140),Gen(140),E(140),Dumscr(5212)
      common/cut/Energy,Deriv,Acurcy,Rms,Maxcyc,Icount,Class,Extrap
      common/fuzzyf/Fuzzy,Degen,Sthrs,Span
      common/ibf/Ibf(30)
      common/locibf/Irwibf,Lenibf
      common/lge503/Lilsw,Lipcw,Ltau,Lehf,Lvir,Lrep,Lten,Ls2,Ligen,Lrmsd
      common/isy503/Isymm,Neq
      equivalence(Gen(1),Igen(1))
      equivalence(Isym2e,Ibf(30)),(Idum(1),Dumscr(1))
      data iblnk/'    '/
      data ten/1.D1/
      data finac/5.D-5/
      data fuzzyd,degend,sthrsd,spand/1.D-10,2.D-5,1.D-4,5.D-7/
      data labdck/'ALPH','A MO',' COE','FS  ','ALPH','A MO',' COE','FS,I
     &','BETA',' MO ','COEF','S   ','BETA',' MO ','COEF','S, I','ALPH','
     &A DE','NSIT','Y   ','ALPH','A DE','NSIT','Y, I','BETA',' DEN','SIT
     &Y','    ','BETA',' DEN','SITY',',  I','EIGE','NVAL','UES ','    ',
     &'SYMM','ETRY','    ','    ','COMM','ON/G','EN/ ','    '/
      data lmo/1,3,2,4/
      data ldns/5,7,6,8/
      data irww/571/
      data namscf/4HRRHF,4HRUHF,4HCRHF,4HCUHF/
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(1A1)
99002 format(2G20.14)
99003 format(i3)
99004 format(1x)
99005 format(' REAL RHF PROCEDURE FOR CLOSED SHELLS'/1x,20('*'))
99006 format(' REAL UHF PROCEDURE'/1x,18('*'))
99007 format(' COMPLEX RHF PROCEDURE FOR CLOSED SHELLS'/1x,39('*'))
99008 format(' COMPLEX UHF PROCEDURE'/1x,21('*'))
99009 format(' CYCLE #',i3,': EHF=',d21.12,'  NUCLEAR REP.=',d20.12,'  V
     &IRIAL=',d20.12/12x,'S**2=',f8.4,'  RMS ERROR P-MATRIX=',d11.4,'  T
     &AU=',f8.4)
99010 format(' THIS PROCEDURE IS NOT AVAILABLE FOR EITHER FULL OR EMPTY 
     &SPINSPA           CES. USE L501 OR L502')
99011 format(' SYMMETRY-ASSIGNMENT NOT POSSIBLE DUE TO FUZZY F(1),T')
99012 format(' M.O. COEFFICIENTS ',a6/1x,17('*'))
99013 format(' DENSITY MATRIX ',a6/1x,14('*'))
99014 format(' ','SCF DONE:  E(',a4,') = ',g19.12,' A.U. AFTER ',i4,' CY
     &CLES'/1x,'           CONVG  = ',d13.4,12x,' -V/T = ',f7.4)
99015 format(' EIGENVALUES ',a6/1x,11('*'))
99016 format(5(i7,f13.7))
99017 format(' SYMMETRY ASSIGNMENTS ',a6/1x,20(1H*))
99018 format(14(i5,':',i3))
99019 format(' SCFDM CANNOT HANDLE COMPLEX WAVE-FUNCTIONS WITH'/' TWO-EL
     &ECTRON INTEGRAL SYMMETRY TURNED ON.')
      call drum
      call dimens(Nbasis)
      
      macc=Iop(6)
      mcyc=Iop(7)
      mrest=Iop(10)
      ientry=Iop(12)
      inhib=Iop(16)
      iconsd=Iop(17)
      icuts=Iop(18)
      incy1=Iop(24)
      incy8=Iop(23)
      incy64=Iop(22)
      ipsav=Iop(31)
      ipch=Iop(32)
      iprint=Iop(33)
      idump=Iop(34)
      
      if(Nae.LE.0.OR.Nae.EQ.Nbasis.OR.Nbe.EQ.0.OR.Nbe.EQ.Nbasis)then
      write(Iout,99010)
      call lnk1e
      endif
      
      call ilsw(2,21,minprt)
      minpsv=minprt
      i1=iabs(2-ipsav)
      if(ipsav.NE.0)call ilsw(1,21,i1)
      call ilsw(2,21,minprt)
      
      if(minprt.NE.0)call ilsw(1,20,1)
      
      call ilsw(2,1,icons)
      icons=icons+1
      Cmp=.FALSE.
      if(icons.GT.2)Cmp=.TRUE.
      Rhf=.FALSE.
      if(mod(icons,2).NE.0)Rhf=.TRUE.
      
      call ilsw(2,6,istab)
      if(istab.NE.0)call lnk1
      if(inhib.NE.0)call ilsw(1,7,0)
      
      Ksm=mod(icons-1,2)+1
      Nesk(1)=0
      Nesk(2)=Nbasis
      nb2=Ksm*Nbasis
      Icount=0
      if(icons.EQ.2)then
      
      write(Iout,99006)
      elseif(icons.EQ.3)then
      
      Ksw(1)=iblnk
      write(Iout,99007)
      elseif(icons.EQ.4)then
      
      write(Iout,99008)
      else
      
      Ksw(1)=iblnk
      write(Iout,99005)
      endif
      
      call tquery(Ievals,levals)
      if(levals.EQ.0)then
      do 50 i=1,nb2
      Isym(i)=0
50    continue
      call twrite(Ievals,Isym,nb2,1,nb2,1,0)
      endif
      
      if(.NOT.(Rhf))then
      iq=2
      if(Cmp)iq=1
      do 100 i=1,4,iq
      call twrite(Iptot(i),Da,Ntt,1,Ntt,1,0)
100   continue
      endif
      
      
      call tread(Irwibf,Ibf(1),Lenibf,1,Lenibf,1,0)
      
      if(Isym2e.EQ.1)then
      if(Cmp)then
      write(Iout,99019)
      call lnk1e
      endif
      call tread(Isymm,Idum(1),1,1,1,1,0)
      nsymop=Idum(1)
      call tread(Neq,Idum(1),LNEQ,1,LNEQ,1,0)
      k=MAXSHL*8
      do 150 j=1,8
      do 120 i=1,MAXBAS
      k=k+1
      neqbas(i,j)=Idum(k)
120   continue
150   continue
      endif
      
      if(mrest.EQ.0)then
      endif
      
      Acurcy=finac
      if(macc.GT.0)Acurcy=ten**(-macc)
      Maxcyc=32
      if(mcyc.GT.0)Maxcyc=mcyc
      
      Fuzzy=fuzzyd
      Degen=degend
      Sthrs=sthrsd
      Span=spand
      if(iconsd.NE.0)read(In,99002)Fuzzy,Degen
      if(icuts.NE.0)read(In,99002)Sthrs,Span
      
      nb22=(nb2+1)/2
      call tread(Ievals,Isym,nb22,1,nb22,1,0)
      if(Maxcyc.NE.0)then
      
      if(ientry.EQ.0)call first(nsymop,neqbas,jcycle)
      endif
      
      
      if(.NOT.(Rhf))then
      iq=1
      if(Cmp)iq=2
      do 200 i=1,iq
      call tread(Iod(i),Da,Ntt,1,Ntt,1,0)
      call tread(Iod(i+2),Db,Ntt,1,Ntt,1,0)
      do 160 j=1,Ntt
      a0=Da(j)+Db(j)
      a1=Da(j)-Db(j)
      Da(j)=a0
      Db(j)=a1
160   continue
      call twrite(Iptot(i),Da,Ntt,1,Ntt,1,0)
      call twrite(Ipspin(i),Db,Ntt,1,Ntt,1,0)
200   continue
      endif
      
      if(ipch.NE.0.OR.iprint.NE.0)then
      call tread(Ieval,E,nb2,1,nb2,1,0)
      ipart=(icons-1)/2+1
      nbsq=Nbasis**2
      do 250 Kspin=1,Ksm
      Nest=Nesk(Kspin)
      call tioc(Nbasis,2,Iod,Da,2,1,idump)
      if(iprint.GT.1)then
      write(Iout,99013)Ksw(Kspin)
      call csymm(Da,Mdim,Nbasis,Cmp)
      endif
      if(ipch.NE.0.AND.ipch.NE.1)then
      il=ldns(Kspin,1)
      call binwt(Da,2*Ntt,labdck(1,il),Nbasis)
      il=ldns(Kspin,2)
      if(Cmp)call binwt(Da(Mtt+1),2*Ntt,labdck(1,il),Nbasis)
      endif
      call tioc(Nbasis,2,Ioc,Da,4,1,idump)
      if(iprint.NE.0)then
      write(Iout,99015)Ksw(Kspin)
      write(Iout,99016)(i,E(i+Nest),i=1,Nbasis)
      write(Iout,99012)Ksw(Kspin)
      call cmat(Da,Mdim,Mdim,Nbasis,Nbasis,Cmp)
      endif
      if(ipch.NE.0.AND.ipch.NE.2)then
      call comat(Da,Da,Mdim,Nbasis)
      il=lmo(Kspin,1)
      call binwt(Da,2*nbsq,labdck(1,il),Nbasis)
      if(Cmp)then
      call comat(Da(Mdsq+1),Da(Mdsq+1),Mdim,Nbasis)
      il=lmo(Kspin,2)
      call binwt(Da(Mdsq+1),2*nbsq,labdck(1,il),Nbasis)
      endif
      endif
250   continue
      
      if(ipch.NE.0)call binwt(E,2*nb2,labdck(1,9),Nbasis)
      endif
      
      if(ientry.NE.1)then
      call tread(Igeno,Gen,Ligen,1,Ligen,1,0)
      rep=Gen(Lrep)
      
      tenrgy=Energy+rep
      if(minprt.EQ.0)then
      vir=Zero
      write(Iout,99004)
      write(Iout,99014)namscf(icons),tenrgy,jcycle,Rms,vir
      endif
      
      ipr=.FALSE.
      if(iprint.GE.2)ipr=.TRUE.
      call frmw(Nbasis,Nae,Nbe,Ioc(1),Ioc(2),Ioc(3),Ieval,irww,Fa,E,Da,i
     &pr,Iout)
      
      call ilsw(1,8,1)
      
      call ilsw(1,21,minpsv)
      
      
      Igen(Lilsw)=Iop(2)
      Igen(Lipcw)=Iop(1)
      Gen(Lehf)=tenrgy
      Gen(Lten)=tenrgy
      Gen(Ltau)=Acurcy
      Gen(Lrmsd)=Rms
      
      vir=Zero
      Gen(Lvir)=vir
      
      Gen(Ls2)=Zero
      if(.NOT.Rhf)call spindm(Nae,Nbe,Nbasis,Gen(Ls2),Da,Fa,idump)
      call twrite(Igeno,Gen,Ligen,1,Ligen,1,0)
      if(ipch.NE.0)call binwt(Gen,2*Ligen,labdck(1,11),Nbasis)
      
      if(.NOT.(Isym(1).NE.0.AND.(Rhf.OR.Isym(Nbasis+1).NE.0)))then
      write(Iout,99011)
      do 260 i=1,nb2
      Isym(i)=0
260   continue
      
      elseif(iprint.NE.0)then
      do 280 Kspin=1,Ksm
      Nest=Nesk(Kspin)
      write(Iout,99017)Ksw(Kspin)
      write(Iout,99018)(i,Isym(i+Nest),i=1,Nbasis)
280   continue
      endif
      call twrite(Ievals,Isym,nb22,1,nb22,1,0)
      if(ipch.NE.0)call binwt(Isym,2*nb22,labdck(1,10),Nbasis)
      if(minprt.NE.0)write(Iout,99004)
      if(minprt.NE.0)write(Iout,99009)Maxcyc,tenrgy,rep,vir,Gen(Ls2),Rms
     &,Acurcy
      endif
      call ilsw(2,5,iconv)
      if(iconv.NE.0)call lnk1e
      
      JUMP=0
      return
      
      end
C* :1 * 
      
