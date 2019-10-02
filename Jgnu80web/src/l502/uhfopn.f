
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uhfopn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uhfopn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "uhfopn.web"
      subroutine uhfopn(JUMP)
      implicit none
      double precision acurcy,Atmchg,C,D,Da,Db,Dgen,energy,Fa,Fb,Filabc,
     &finac,s2,t,ten,tenrgy,V,vir,Vv,zero
      integer i,Ian,Ibf,Icharg,icnvg,icyc,Ida,ide,idump,iextp,Igen,igues
     &s,ihalt,Ij,In,Iop,Iout,ipch,iprint,Ipunch
      integer irwan,Irwc1,Irwc2,Irwc3,Irwca,Irwcb,Irwden,Irweig,Irwfa,Ir
     &wfb,Irwgen,Irwh,Irwibf,Irwpa,Irwpb,Irwps,Irwpt,Irws,Irwt,Irwtm
      integer Irwur,irww,iscf,Isym2e,isymm,itemp,Ititle,jcycle,JUMP,labd
     &ck,len,LENB,Lenibf,lneq,MAXBAS,maxcyc,Maxnbf,MAXNTT,MAXPRM,MAXS21
      integer MAXSH1,MAXSHL,MEMLEN,mout,MOUTD,MPLUS1,Multip,n,Nae,Natoms
     &,nb,Nbasis,nbasp,Nbe,Ne,neq,neqbas,nsymop,Ntt,ntt2
      integer Nttmax,Nwiib,nwrd
      
      
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      
      parameter(MAXBAS=150,MEMLEN=50000,MAXNTT=((MAXBAS*(MAXBAS+1))/2),M
     &OUTD=(8*MAXBAS+8*MAXSHL),MPLUS1=(8*MAXSHL+1))
      
      integer Psave
      character*6 Word
      logical ipr
      dimension Igen(1)
      dimension irwan(13)
      dimension Ititle(80)
      dimension labdck(4,5)
      dimension Ida(1)
      dimension mout(MOUTD),neqbas(MAXBAS,8)
      dimension D(75,75),Irwden(4)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/memry/Da(2850),Db(2850),Fa(2850),Fb(2850),V(75,75),Vv(75),F
     &ilabc(32900)
      common/psave/Psave
      common/word/Word(4)
      common/ind/Ntt,Ij(127)
      common/max502/Maxnbf,Nttmax
      common/io/In,Iout,Ipunch
      common/gen/Dgen(47)
      common/irw502/Irwgen,Irweig,Irwca,Irwcb,Irwpa,Irwpb,Irwpt,Irwps,Ir
     &wfa,Irwfb,Irwur,Irws,Irwh,Irwt,Irwtm,Irwc1,Irwc2,Irwc3,Irwibf,Leni
     &bf
      common/ibf/Ibf(30)
      equivalence(Igen(1),Dgen(1))
      equivalence(Nwiib,Ibf(29))
      equivalence(D(1,1),Da(1))
      equivalence(Ititle(1),Fa(1))
      equivalence(mout(MPLUS1),neqbas(1,1))
      equivalence(Ida(1),Da(1))
      equivalence(Isym2e,Ibf(30))
      equivalence(Irwpa,Irwden(1))
      data labdck/'ALPH','A MO',' COE','FS  ','BETA',' MO ','COEF','S   
     &','ALPH','A DE','NSIT','Y   ','BETA',' DEN','SITY','    ','EIGE','
     &NVAL','UES ','   '/
      data irwan/2551,2552,2553,2554,2555,2556,2557,2558,2559,2560,2561,
     &2552,2553/,irww/571/
      data finac/5.0D-5/,zero/0.0D0/,ten/10.0D0/
      data isymm/551/,neq/565/,lneq/1000/
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(' UHF OPEN SHELL SCF.'/' REQUESTED CONVERGENCE ON DENSITY M
     &ATRIX =',d11.4,8H WITHIN ,i3,8H CYCLES.)
99002 format(1x,4HITER,2x,17HELECTRONIC-ENERGY,7x,11HCONVERGENCE,4x,13HE
     &XTRAPOLATION/1x,4(1H-),2x,17(1H-),7x,11(1H-),4x,13(1H-))
99003 format(12H CORE GUESS.)
99004 format(33H DENSITY MATRIX READ FROM CARDS: ,13A6)
99005 format(' ','SCF DONE:  E(UHF) = ',g19.12,' A.U. AFTER ',i4,' CYCLE
     &S'/1x,'           CONVG  = ',d13.4,12x,' -V/T = ',f7.4/1x,'       
     &    S**2   =   ',f7.4)
99006 format('+',20x,g19.12)
99007 format(1x)
99008 format(1x,10(1H<),a6,11HEIGENVALUES,10(1H>)/(i4,d14.7,i4,d14.7,i4,
     &d14.7,i4,d14.7,i4,d14.7))
99009 format(1x,10(1H<),a6,30HMOLECULAR ORBITAL COEFFICIENTS,10(1H>))
99010 format(1x,10(1H<),a6,14HDENSITY MATRIX,10(1H>))
99011 format(1x,10(1H<),18HALPHA-BETA OVERLAP,10(1H>))
99012 format('  DENSITY MATRIX READ IN HAS WRONG BASIS.')
99013 format(38H0CONVERGENCE FAILURE---RUN TERMINATED./)
99014 format(i3)
      
      call drum
      
      call ilsw(2,1,iscf)
      if(iscf.EQ.0.OR.iscf.GT.1)goto 500
      
      ipch=Iop(32)
      iprint=Iop(33)
      idump=Iop(34)
      iguess=Iop(5)
      icnvg=Iop(6)
      icyc=Iop(7)
      iextp=Iop(11)
      ihalt=Iop(13)-1
      acurcy=finac
      
      maxcyc=32
      if(icyc.NE.0)maxcyc=icyc
      
      if(icnvg.NE.0)acurcy=ten**(-icnvg)
      call ilsw(2,21,Psave)
      if(idump.NE.0)iprint=3
      
      write(Iout,99001)acurcy,maxcyc
      if(Psave.EQ.0)write(Iout,99002)
      Ntt=(Nbasis*(Nbasis+1))/2
      nbasp=Nbasis+1
      do 100 i=1,nbasp
      Ij(i)=(i*(i-1))/2
100   continue
      len=3*(Maxnbf*Maxnbf+Maxnbf)
      do 200 i=1,len
      Da(i)=zero
200   continue
      if(Iop(12).EQ.0)then
      
      if(iguess.NE.0)then
      call twrite(Irweig,Da,2*Nbasis,1,2*Nbasis,1,0)
      call twrite(Irwca,Da,Maxnbf,Maxnbf,Nbasis,Nbasis,0)
      call twrite(Irwcb,Da,Maxnbf,Maxnbf,Nbasis,Nbasis,0)
      call twrite(Irwpa,Da,Nttmax,1,Ntt,1,0)
      call twrite(Irwpb,Da,Nttmax,1,Ntt,1,0)
      endif
      call twrite(Irwpt,Da,Nttmax,1,Ntt,1,0)
      call twrite(Irwps,Da,Nttmax,1,Ntt,1,0)
      call twrite(Irwfa,Da,Nttmax,1,Ntt,1,0)
      call twrite(Irwfb,Da,Nttmax,1,Ntt,1,0)
      call twrite(Irwur,Da,Maxnbf,Maxnbf,Nbasis,Nbasis,0)
      
      ntt2=Ntt+Ntt
      call twrite(Irwc1,Da,ntt2,1,ntt2,1,0)
      call twrite(Irwc2,Da,ntt2,1,ntt2,1,0)
      call twrite(Irwc3,Da,ntt2,1,ntt2,1,0)
      
      call tread(Irwibf,Ibf,Lenibf,1,Lenibf,1,0)
      
      if(Isym2e.EQ.1)then
      call tread(isymm,Ida(1),1,1,1,1,0)
      nsymop=Ida(1)
      call tread(neq,mout(1),lneq,1,lneq,1,0)
      endif
      call formv(Nbasis,Maxnbf,D,V,Fa,Fb)
      iguess=Iop(5)+1
      if(iguess.EQ.2)then
      call tread(Irwh,Fa,Nttmax,1,Ntt,1,0)
      call tread(Irwh,Fb,Nttmax,1,Ntt,1,0)
      do 220 i=1,Ntt
      Da(i)=zero
      Db(i)=zero
220   continue
      ide=0
      write(Iout,99003)
      elseif(iguess.EQ.3)then
      call binrd(Da,Ititle,1,1HP,nwrd,nb)
      write(Iout,99004)(Ititle(i),i=1,14)
      if(nb.NE.Nbasis)then
      write(Iout,99012)
      call lnk1e
      endif
      call binrd(Db,Ititle,1,1HP,nwrd,nb)
      write(Iout,99004)(Ititle(i),i=1,14)
      if(nb.NE.Nbasis)then
      write(Iout,99012)
      call lnk1e
      endif
      ide=1
      else
      call tread(Irwpa,Da,Nttmax,1,Ntt,1,0)
      call tread(Irwpb,Db,Nttmax,1,Ntt,1,0)
      ide=1
      endif
      call cycopn(energy,acurcy,maxcyc,ide,jcycle,iextp,nsymop,neqbas)
      
      call tread(Irwpa,Da,Ntt,1,Ntt,1,0)
      call tread(Irwpb,Db,Ntt,1,Ntt,1,0)
      do 250 i=1,Ntt
      Da(i)=Da(i)+Db(i)
250   continue
      call twrite(Irwpt,Da,Ntt,1,Ntt,1,0)
      
      do 300 i=1,Ntt
      Da(i)=Da(i)-(Db(i)+Db(i))
300   continue
      call twrite(Irwps,Da,Ntt,1,Ntt,1,0)
      
      call spin(Nbasis,Nae,Nbe,Maxnbf,Da,Fa,Vv,s2)
      
      call tread(Irwgen,Dgen,47,1,47,1,0)
      
      tenrgy=Dgen(41)+energy
      
      call virial(Nbasis,tenrgy,Da,Db,Irwpt,Irwt,vir,t)
      
      write(Iout,99007)
      write(Iout,99005)tenrgy,jcycle,acurcy,vir,s2
      
      call annil(Iop(14),Nae,Nbe,Nbasis,irwan,iprint,Da,Fa,V(1,1),V(1,2)
     &)
      Dgen(23)=acurcy
      Dgen(32)=tenrgy
      Dgen(40)=vir
      Dgen(42)=t
      Dgen(43)=tenrgy
      Dgen(44)=s2
      call twrite(Irwgen,Igen,47,1,47,1,0)
      
      
      if(iprint.GT.0)then
      call tread(Irweig,Fa,Nttmax,1,2*Nbasis,1,0)
      write(Iout,99008)Word(1),(i,Fa(i),i=1,Nbasis)
      call tread(Irwca,D,Maxnbf,Maxnbf,Nbasis,Nbasis,0)
      write(Iout,99009)Word(1)
      call matout(D,Maxnbf,Maxnbf,Nbasis,Nbasis)
      do 320 i=1,Nbasis
      Fa(i)=Fa(i+Nbasis)
320   continue
      write(Iout,99008)Word(2),(i,Fa(i),i=1,Nbasis)
      call tread(Irwcb,D,Maxnbf,Maxnbf,Nbasis,Nbasis,0)
      write(Iout,99009)Word(2)
      call matout(D,Maxnbf,Maxnbf,Nbasis,Nbasis)
      
      if(iprint.GT.1)then
      do 330 i=1,4
      call tread(Irwden(i),Da,Nttmax,1,Ntt,1,0)
      write(Iout,99010)Word(i)
      call ltoutd(Nbasis,Da,1)
330   continue
      write(Iout,99011)
      call tread(Irwur,D,Maxnbf,Maxnbf,Nbasis,Nbasis,0)
      call matout(D,Maxnbf,Maxnbf,Nbasis,Nbasis)
      endif
      endif
      endif
      
      
      if(ipch.LT.1)goto 400
      if(ipch.NE.1)then
      call tread(Irwpa,Da,Nttmax,1,Ntt,1,0)
      call binwt(Da,2*Ntt,labdck(1,3),Nbasis)
      call tread(Irwpb,Da,Nttmax,1,Ntt,1,0)
      call binwt(Da,2*Ntt,labdck(1,4),Nbasis)
      if(ipch.EQ.2)goto 400
      endif
      n=Nbasis*Nbasis
      call tread(Irwca,Da,n,1,n,1,0)
      call binwt(Da,2*n,labdck(1,1),Nbasis)
      call tread(Irwcb,Da,n,1,n,1,0)
      call binwt(Da,2*n,labdck(1,2),Nbasis)
      call tread(Irweig,Da,Ntt,1,2*Nbasis,1,0)
      call binwt(Da,4*Nbasis,labdck(1,5),Nbasis)
      
      
400   ipr=.FALSE.
      if(iprint.GE.2)ipr=.TRUE.
      call frmw(Nbasis,Nae,Nbe,Irwca,-1,Irwcb,Irweig,irww,Fa,Fb,Da,ipr,I
     &out)
      
      call ilsw(2,5,itemp)
      if(ihalt*itemp.NE.0)then
      write(Iout,99013)
      call lnk1e
      endif
      
500   JUMP=0
      
      return
      
      end
C* :1 * 
      
