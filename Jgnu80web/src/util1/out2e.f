
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 out2e"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "out2e.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 43 "out2e.web"
      subroutine out2e(NSET,MU,NU,LAMBDA,SIGMA,GINT,DBUF,IBUF2E,DBUF2E,I
     &RET,IDCOUT,IOP,D,F)
      implicit none
      double precision aa,Atmchg,C,cutoff,DBUF2E,gabs,GINT,pt5,ten,tenm6
     &,Valint
      integer I,ia,Ian,Ibasd,Ibase,Ibf,IBUF2E,Icharg,Icon,Icount,Icut,ID
     &COUT,Idump,Ifil,ifprnt,ij,In,intape,Intcnt,Iops2e
      integer Iout,Iprint,Ipunch,Iq,Ireset,IRET,Irwibf,Irwicb,iset,Ismod
     &e,isss,Istat,iswich,Isym2e,itest,Itotal,Iux,J,Ja,jprint
      integer jsss,K,Kntt1,Kntt2,L,LAMBDA,Last,Lenibf,Limint,Mindx,Mode,
     &MU,Multip,n,Nae,Natoms,Nbasis,Nbe,Ne,next
      integer nfile,Nrpext,NSET,Ntx,NU,Nwiib,nwiib2,Nwpi
      save
      integer IOP(*)
      double precision D(*),F(*)
      integer P,Q,R,Sindx
      integer SIGMA
      integer Dbase,Dcount,Dbasd
      logical DBUF,ifirst
      dimension Ibf(30)
      dimension IBUF2E(*),DBUF2E(*),GINT(3,3),ia(2),ifprnt(7)
      dimension ij(150)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/io/In,Iout,Ipunch
      common/packed/I,J,K,L,Valint,Ja
      common/irwo2e/Irwicb,Irwibf,Lenibf
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Isym2e
      common/ops2e/Iops2e(20)
      equivalence(Ismode,Ibf(1))
      equivalence(Iprint,Iops2e(1))
      equivalence(Idump,Iops2e(2))
      equivalence(Icut,Iops2e(4))
      equivalence(P,I),(Q,J),(R,Mindx,K),(Sindx,L)
      equivalence(ia(1),aa),(Kntt1,Icount),(Kntt2,Dcount)
      data ifprnt/0,0,2,1,2,1,2/
      data nfile/0/
      data pt5/0.5D0/,ten/10.0D0/
      data tenm6/1.0D-6/
      
      
99001 format(1x,i8,' INTEGRALS PRODUCED FOR A TOTAL OF ',i9)
99002 format(15H AFTER BUFLAB' ,2I9,1x,i12)
99003 format(15H SWAP BUFFERS' ,5I9)
99004 format(27H INTEGRAL FILE'  SWAP FROM ,i2,4H TO ,i2,5H, AT ,i9,16H 
     &TOTAL RECORDS. )
99005 format(20H AT INITIALIZATION' /(1x,i6,2x,i9))
99006 format(' CONTINUATION IN OUT2E:'/(1x,i6,2x,i9))
      
      if(IOP(45).NE.0)then
      if(NSET.LT.0)call tread(Irwibf,Ibf(1),Lenibf,1,Lenibf,1,0)
      if(NSET.GT.0)then
      if(IOP(45).EQ.1)call dir2e(NSET,MU,NU,LAMBDA,SIGMA,GINT,IOP,D,F)
      if(IOP(45).EQ.2)call dirtrn
      IRET=0
      return
      else
      IRET=0
      return
      endif
      elseif(NSET.LT.0)then
      
      
      
      
      
      
      
      
      call tread(Irwibf,Ibf(1),Lenibf,1,Lenibf,1,0)
      Intcnt=0
      IRET=0
      ifirst=.TRUE.
      intape=Iux(2)
      Ntx=1
      do 50 I=1,Nbasis
      ij(I)=(I*(I-1))/2
50    continue
      Icut=IOP(27)
      Iprint=IOP(33)
      Idump=IOP(34)
      if(Idump.GE.3)Iprint=2
      cutoff=tenm6
      if(Icut.NE.0)cutoff=ten**(-Icut)
      jprint=ifprnt(Iprint+1)
      
      if(Istat.EQ.1)then
      
      Iq=1
      if(Idump.NE.0)write(Iout,99005)(I,Ibf(I),I=1,30)
      call idef(intape,IOP(36))
      elseif(Istat.EQ.2)then
      
      Iq=1
      Ibase=Ibasd(1)
      Dbase=Dbasd(1)
      nwiib2=Nwiib/2
      call tread(Irwicb,IBUF2E(Ibase),nwiib2,1,nwiib2,1,0)
      if(Idump.NE.0)write(Iout,99006)(I,Ibf(I),I=1,30)
      intape=Iux(Ntx+1)
      if(Ifil.GT.0)call ipos(intape,Ifil+1)
      else
      
      IRET=1
      return
      endif
      return
      elseif(NSET.EQ.0)then
      
      
      if(DBUF.AND.(.NOT.ifirst))call iwait(intape)
      if(Last.NE.IDCOUT)then
      
      Istat=2
      nwiib2=Nwiib/2
      call twrite(Irwicb,IBUF2E(Ibase),nwiib2,1,nwiib2,1,0)
      else
      
      Istat=3
      call labint(IBUF2E(Ibase),1)
      call iwrite(intape,Iq,IBUF2E)
      Ifil=Ifil+1
      call iwait(intape)
      call iwind(intape)
      endif
      else
      
      
      
      
      do 150 iset=1,NSET
      if(Mode.EQ.1)then
      elseif(Mode.EQ.3.OR.Mode.EQ.5.OR.Mode.EQ.6)then
      if((gabs(GINT(1,iset))+gabs(GINT(2,iset))).GT.cutoff)goto 60
      goto 150
      elseif(Mode.EQ.4)then
      if((gabs(GINT(1,iset))+gabs(GINT(2,iset))+gabs(GINT(3,iset))).GT.c
     &utoff)goto 60
      goto 150
      endif
      if(gabs(GINT(1,iset)).LE.cutoff)goto 150
      
60    Intcnt=Intcnt+1
      
      I=MU
      if(iset.EQ.2)then
      
      J=SIGMA
      K=NU
      L=LAMBDA
      elseif(iset.EQ.3)then
      
      J=LAMBDA
      K=NU
      L=SIGMA
      else
      
      J=NU
      K=LAMBDA
      L=SIGMA
      endif
      
      
      iswich=0
      if(I.LT.J)then
      n=I
      I=J
      J=n
      iswich=iswich+1
      endif
      if(K.LT.L)then
      n=K
      K=L
      L=n
      iswich=iswich+1
      endif
      if(I.LT.K)then
      elseif(I.EQ.K)then
      
      if(J.GE.L)goto 80
      else
      goto 80
      endif
      n=I
      I=K
      K=n
      n=J
      J=L
      L=n
      
80    if(Ismode.LE.0)then
      
      if(I.NE.J)then
      
      if(I.NE.K)then
      
      if(J.NE.K)then
      
      if(K.EQ.L)then
      Sindx=5
      
      elseif(J.NE.L)then
      
      
      
      
      call pack4
      aa=GINT(1,iset)
      IBUF2E(Kntt1+Ibase)=Ja
      IBUF2E(Kntt1+1+Ibase)=ia(1)
      IBUF2E(Kntt1+2+Ibase)=ia(2)
      Kntt1=Kntt1+3
      goto 100
      else
      Sindx=2
      endif
      elseif(K.NE.L)then
      
      R=L
      Sindx=1
      else
      Mindx=1
      Sindx=6
      endif
      elseif(J.NE.L)then
      
      R=L
      Sindx=0
      else
      Sindx=3
      endif
      elseif(I.NE.K)then
      
      if(K.NE.L)then
      Q=K
      R=L
      Sindx=4
      else
      
      Q=K
      Mindx=2
      Sindx=6
      endif
      elseif(I.NE.L)then
      
      Q=L
      Mindx=3
      Sindx=6
      else
      Sindx=7
      endif
      
      call pack4
      aa=GINT(1,iset)
      IBUF2E(Kntt2+Ibase)=Ja
      IBUF2E(Kntt2+1+Ibase)=ia(1)
      IBUF2E(Kntt2+2+Ibase)=ia(2)
      Kntt2=Kntt2-3
      else
      
      isss=I
      jsss=J
      if(Ismode.LT.4)then
      
      I=ij(I)+J
      J=ij(K)+L
      call pack2
      else
      call pack4
      I=ij(I)+J
      J=ij(K)+L
      endif
      if(Ismode.EQ.2.OR.Ismode.EQ.4)then
      
      if(I.EQ.J)then
      GINT(1,iset)=pt5*GINT(1,iset)
      GINT(2,iset)=pt5*GINT(2,iset)
      endif
      IBUF2E(Icount+Ibase)=Ja
      DBUF2E(Dcount+Dbase)=GINT(1,iset)
      DBUF2E(Dcount+1+Dbase)=GINT(2,iset)
      Dcount=Dcount+2
      elseif(Ismode.EQ.3.OR.Ismode.EQ.5)then
      
      if(I.EQ.J)then
      GINT(1,iset)=pt5*GINT(1,iset)
      GINT(2,iset)=pt5*GINT(2,iset)
      GINT(3,iset)=pt5*GINT(3,iset)
      endif
      IBUF2E(Icount+Ibase)=Ja
      DBUF2E(Dcount+Dbase)=GINT(1,iset)
      DBUF2E(Dcount+1+Dbase)=GINT(2,iset)
      itest=mod(iswich,2)
      if(itest.NE.0)GINT(3,iset)=-GINT(3,iset)
      DBUF2E(Dcount+2+Dbase)=GINT(3,iset)
      Dcount=Dcount+3
      else
      
      if(I.EQ.J)GINT(1,iset)=pt5*GINT(1,iset)
      IBUF2E(Icount+Ibase)=Ja
      DBUF2E(Dcount+Dbase)=GINT(1,iset)
      Dcount=Dcount+1
      endif
      
      Icount=Icount+1
      if(Icount.GT.Limint)goto 120
      goto 150
      endif
      
100   if(Kntt1.LE.Kntt2)goto 150
      
120   call labint(IBUF2E(Ibase),0)
      if(Idump.NE.0)write(Iout,99002)Iq,Ibase,IBUF2E(Ibase)
      Kntt1=Ireset(1)
      Kntt2=Ireset(2)
      if(DBUF.AND.(.NOT.ifirst))call iwait(intape)
      call iwrite(intape,Iq,IBUF2E)
      Ifil=Ifil+1
      if(.NOT.DBUF)call iwait(intape)
      if(Ifil.NE.(nfile+Icon*Ntx))then
      
      ifirst=.FALSE.
      else
      Ntx=Ntx+1
      next=Iux(Ntx+1)
      write(Iout,99004)intape,next,Ifil
      if(intape.NE.Iux(1))call ifile(intape)
      call iwind(intape)
      call iwind(next)
      ifirst=.TRUE.
      intape=next
      endif
      if(DBUF)then
      Iq=iabs(Iq-2)+1
      Ibase=Ibasd(Iq)
      Dbase=Dbasd(Iq)
      if(Idump.NE.0)write(Iout,99003)Iq,Icount,Ibase,Dcount,Dbase
      endif
150   continue
      return
      endif
      Itotal=Itotal+Intcnt
      call twrite(Irwibf,Ibf(1),Lenibf,1,Lenibf,1,0)
      if(Intcnt.GT.0)write(Iout,99001)Intcnt,Itotal
      
      
      if(jprint.GT.0.AND.Istat.EQ.3)call dmpint(jprint,IBUF2E,DBUF2E)
      
      return
      
      end
C* :1 * 
      
