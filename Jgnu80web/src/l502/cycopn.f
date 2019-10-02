
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cycopn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cycopn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 33 "cycopn.web"
      subroutine cycopn(ENERGY,ACURCY,MAXCYC,IDE,JCYCLE,IEXTP,NSYMOP,NEQ
     &BAS)
      implicit none
      double precision ACURCY,Atmchg,C,Crit,D,Da,Db,Dd,ENERGY,ent,F,Fa,F
     &b,Ff,Filabc,pt5,V,Vv,zero
      integer i,Ian,Icharg,Icount,IDE,idump,IEXTP,iflag,Ij,In,Iop,Iout,i
     &pch,iprint,Ipunch,Irwc1,Irwc2,Irwc3,Irwca,Irwcb
      integer Irweig,Irwfa,Irwfb,Irwgen,Irwh,Irwibf,Irwpa,Irwpb,Irwps,Ir
     &wpt,Irws,Irwt,Irwtm,Irwur,j,JCYCLE,Jdumx,k,key,Lenibf
      integer MAXBAS,MAXCYC,Maxnbf,Multip,n,Nae,Natoms,Nbasis,Nbe,Ne,NEQ
     &BAS,NSYMOP,Ntt,Ntt2,Nttmax
      parameter(MAXBAS=150)
      dimension NEQBAS(MAXBAS,8)
      integer Psave
      character*6 Word
      dimension Fa(1),Fb(1),Da(1),Db(1)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/memry/D(75,75),Dd(75),F(75,75),Ff(75),V(75,75),Vv(75),Filab
     &c(32900)
      common/psave/Psave
      common/word/Word(4)
      common/icount/Crit,Icount,Ntt2
      common/io/In,Iout,Ipunch
      common/ind/Ntt,Ij(127)
      common/max502/Maxnbf,Nttmax
      common/gen/Jdumx(94)
      common/irw502/Irwgen,Irweig,Irwca,Irwcb,Irwpa,Irwpb,Irwpt,Irwps,Ir
     &wfa,Irwfb,Irwur,Irws,Irwh,Irwt,Irwtm,Irwc1,Irwc2,Irwc3,Irwibf,Leni
     &bf
      equivalence(Da(1),D(1,1)),(Db(1),D(1,39))
      equivalence(Fa(1),F(1,1)),(Fb(1),F(1,39))
      data zero/0.0D0/,pt5/0.5D0/
      
      
      
      
      
      
      
      
99001 format(1x,i3)
99002 format(' ',5x,d22.15)
99003 format(1x,a6,28HM. O. COEFFICIENTS AT CYCLE ,i3,1H.)
99004 format(1x,a6,24HDENSITY MATRIX AT CYCLE ,i3,1H.)
99005 format(10(1H>),31H CONVERGENCE CRITERION NOT MET.)
99006 format(' ',7x,17H(NON-VARIATIONAL))
      
      iprint=Iop(33)
      ipch=Iop(32)
      idump=Iop(34)
      if(idump.NE.0)iprint=3
      
      
      Crit=ACURCY
      Icount=0
      Ntt2=Ntt+Ntt
      ipch=Iop(9)
      JCYCLE=1
      if(IDE.EQ.0)goto 400
100   call tread(Irwh,Fa,Nttmax,1,Ntt,1,0)
      call tread(Irwh,Fb,Nttmax,1,Ntt,1,0)
      k=1
      ENERGY=zero
      do 200 i=1,Nbasis
      do 150 j=1,i
      if(i.NE.j)then
      ENERGY=ENERGY+Fa(k)*Da(k)+Fb(k)*Db(k)
      else
      
      ENERGY=ENERGY+pt5*(Fa(k)*Da(k)+Fb(k)*Db(k))
      endif
      k=k+1
150   continue
200   continue
      ent=ENERGY+ENERGY
      if(Psave.EQ.0)write(Iout,99001)JCYCLE
      
      call fofopn(Nbasis,NSYMOP,NEQBAS)
      k=1
      do 300 i=1,Nbasis
      do 250 j=1,i
      if(i.NE.j)then
      ENERGY=ENERGY+Fa(k)*Da(k)+Fb(k)*Db(k)
      else
      
      ENERGY=ENERGY+pt5*(Fa(k)*Da(k)+Fb(k)*Db(k))
      endif
      k=k+1
250   continue
300   continue
      if(iflag.NE.0)then
      
      if(Psave.EQ.0)write(Iout,99006)
      else
      if(Psave.EQ.0)write(Iout,99002)ENERGY
      endif
400   call twrite(Irwfa,Fa,Nttmax,1,Ntt,1,0)
      call twrite(Irwfb,Fb,Nttmax,1,Ntt,1,0)
      call square(Fa,F,Maxnbf,Nbasis,0)
      call tread(Irwtm,V,Maxnbf,Maxnbf,Nbasis,Nbasis,0)
      call matpac(V,F,D,Maxnbf,Nbasis,1)
      call matpac(D,V,F,Maxnbf,Nbasis,3)
      call diag(Nbasis,Maxnbf,F,D,Ff,Dd)
      call matpac(V,D,F,Maxnbf,Nbasis,2)
      call twrite(Irwca,F,Maxnbf,Maxnbf,Nbasis,Nbasis,0)
      if(iprint.EQ.3)then
      write(Iout,99003)Word(1)
      call matout(F,Maxnbf,Maxnbf,Nbasis,Nbasis)
      endif
      do 500 i=1,Nbasis
      do 450 j=1,i
      D(i,j)=zero
      do 420 k=1,Nae
      D(i,j)=D(i,j)+F(i,k)*F(j,k)
      D(j,i)=D(i,j)
420   continue
450   continue
500   continue
      call twrite(Irwpa,D,Maxnbf,Maxnbf,Nbasis,Nbasis,1)
      if(iprint.EQ.3)then
      write(Iout,99004)Word(1)
      call matout(D,Maxnbf,Maxnbf,Nbasis,Nbasis)
      endif
      call tread(Irwfb,Fa,Nttmax,1,Ntt,1,0)
      call square(Fa,F,Maxnbf,Nbasis,0)
      call matpac(V,F,D,Maxnbf,Nbasis,1)
      call matpac(D,V,F,Maxnbf,Nbasis,3)
      call diag(Nbasis,Maxnbf,F,D,Vv,Dd)
      do 600 i=1,Nbasis
      Fa(i)=Ff(i)
      Fa(i+Nbasis)=Vv(i)
600   continue
      call twrite(Irweig,Fa,Nttmax,1,2*Nbasis,1,0)
      call matpac(V,D,F,Maxnbf,Nbasis,2)
      call twrite(Irwcb,F,Maxnbf,Maxnbf,Nbasis,Nbasis,0)
      if(iprint.EQ.3)then
      write(Iout,99003)Word(2)
      call matout(F,Maxnbf,Maxnbf,Nbasis,Nbasis)
      endif
      do 700 i=1,Nbasis
      do 650 j=1,i
      D(i,j)=zero
      if(Nbe.GT.0)then
      do 610 k=1,Nbe
      D(i,j)=D(i,j)+F(i,k)*F(j,k)
610   continue
      endif
      D(j,i)=D(i,j)
650   continue
700   continue
      call twrite(Irwpb,D,Maxnbf,Maxnbf,Nbasis,Nbasis,1)
      if(iprint.EQ.3)then
      write(Iout,99004)Word(2)
      call matout(D,Maxnbf,Maxnbf,Nbasis,Nbasis)
      endif
      call tread(Irwpa,Da,Nttmax,1,Ntt,1,0)
      call tread(Irwpb,Db,Nttmax,1,Ntt,1,0)
      
      
      if(ipch.EQ.4)then
      call binwt(Da,Ntt,1HP)
      call binwt(Db,Ntt,1HP)
      n=Nbasis*Nbasis
      call tread(Irwca,F,n,1,n,1,0)
      call binwt(F,n,1HC)
      call tread(Irwcb,F,n,1,n,1,0)
      call binwt(F,n,1HC)
      call tread(Irweig,V,2*Nbasis,1,2*Nbasis,1,0)
      call binwt(V,2*Nbasis,1HE)
      endif
      JCYCLE=JCYCLE+1
      call conopn(Nbasis,key,ACURCY,iflag,IEXTP)
      if(key.NE.0)then
      if(JCYCLE.LE.MAXCYC)goto 100
      
      
      write(Iout,99005)
      call ilsw(1,5,1)
      else
      
      call ilsw(1,5,0)
      JCYCLE=JCYCLE-1
      endif
      return
      
      end
C* :1 * 
      
