
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fofclo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fofclo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 33 "fofclo.web"
      subroutine fofclo(NBASIS,JCYCLE,D,F,IBUF2E,DBUF2E,IPRINT,NSYMOP,NE
     &QBAS)
      implicit none
      double precision Aa,D,DBUF2E,F,pt5,secint,terint,Valint,zero
      integer I,Ia,Ibasd,Ibase,IBUF2E,Icon,Icount,Ifil,iflst,Ij,In,intap
     &e,Intcnt,Iop,Iout,IPRINT,Ipunch,Iq,iqbufr,iqproc
      integer Ireset,Irwc,Irwc1,Irwc2,Irwc3,Irweig,Irwf,Irwgen,Irwh,Irwi
     &bf,Irwlc,Irwle,Irwlp,Irwp,Irwpt,Irws,Irwt,Irwtm,Irww,Ismode
      integer Istat,Isym2e,Itotal,Iux,J,Ja,JCYCLE,jq,js,jt,K,Kntt1,Kntt2
     &,L,Last,Length,Limint,lq,m,MAXBAS
      integer Mindx,Mode,named,namef,NBASIS,NEQBAS,next,nfile,Nrpext,Nsq
     &,NSYMOP,Ntt,Ntx,Nwiib,Nwpi
      parameter(MAXBAS=150)
      integer P,Q,R,Sindx
      integer Dbase,Dbasd,Dcount
      logical fast1
      dimension D(*),F(*),IBUF2E(*),DBUF2E(*)
      dimension Ia(2)
      dimension NEQBAS(MAXBAS,8)
      common/ia/Ij(257)
      common/ntt/Ntt,Length,Nsq
      common/io/In,Iout,Ipunch
      common/packed/I,J,K,L,Valint,Ja
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Isym2e
      common/irw501/Irwgen,Irws,Irwh,Irwt,Irweig,Irwc,Irwp,Irwpt,Irwf,Ir
     &wc1,Irwc2,Irwc3,Irwtm,Irwibf(2),Irwle,Irwlc,Irwlp,Irww
      common/iop/Iop(50)
      equivalence(P,I),(Q,J),(R,K),(Sindx,L),(Mindx,K)
      equivalence(Valint,Ia(1),Aa)
      equivalence(Kntt1,Icount),(Kntt2,Dcount)
      data pt5/0.5D0/,namef/4H-F- /,named/4H-D- /,zero/0.0D0/
      data nfile/0/
      
      
      
      
      
      
      
      
      
      
99001 format(25H1BAD INTEGRAL COUNT:  IS ,i9,5x,12H  SHOULD BE ,i9)
99002 format(13H CONTENTS OF ,a4,23HAT THE START OF FOFCLO:)
99003 format(13H CONTENTS OF ,a4,32HAFTER RAFFENETTI INITIALIZATION:)
99004 format(13H CONTENTS OF ,a4,21HAT THE END OF FOFCLO:)
99005 format(1H )
      
      
      fast1=.FALSE.
      do 100 I=1,Ntt
      F(I)=zero
100   continue
      if(IPRINT.GE.3)then
      write(Iout,99002)namef
      call ltoutd(NBASIS,F,1)
      write(Iout,99002)named
      call ltoutd(NBASIS,D,1)
      write(Iout,99005)
      endif
      
      
      if(Ismode.GT.0)then
      K=0
      do 150 I=1,NBASIS
      do 120 J=1,I
      K=K+1
      D(K)=D(K)+D(K)
120   continue
      D(K)=pt5*D(K)
150   continue
      
      if(IPRINT.GE.3)then
      write(Iout,99003)named
      call ltoutd(NBASIS,D,1)
      write(Iout,99003)namef
      call ltoutd(NBASIS,F,1)
      endif
      endif
      
      iqbufr=1
      iqproc=2
      Intcnt=0
      intape=Iux(2)
      Ntx=1
      call iread(intape,iqbufr,DBUF2E)
      Ifil=1
200   call iwait(intape)
      iqbufr=iabs(iqbufr-2)+1
      iqproc=iabs(iqproc-2)+1
      Ibase=Ibasd(iqproc)
      Dbase=Dbasd(iqproc)
      call labscf(IBUF2E(Ibase),iflst)
      if(iflst.EQ.0)then
      if(Ifil.EQ.(nfile+Ntx*Icon))then
      call iwind(intape)
      Ntx=Ntx+1
      next=Iux(Ntx+1)
      call iwind(next)
      intape=next
      endif
      call iread(intape,iqbufr,DBUF2E)
      Ifil=Ifil+1
      endif
      
      
      if(Mode.EQ.2)then
      
      if(Kntt1.GT.0)then
      
      if(.NOT.fast1)then
      
      do 210 m=1,Kntt1
      Ja=IBUF2E(m+Ibase)
      call unpck2
      F(I)=F(I)+D(J)*DBUF2E(m+Dbase)
      F(J)=F(J)+D(I)*DBUF2E(m+Dbase)
210   continue
      Intcnt=Intcnt+Kntt1
      else
      call raff1c(F,D,Kntt1,IBUF2E,Ibase,Dbase,Intcnt)
      endif
      endif
      elseif(Mode.EQ.3)then
      
      Dcount=1
      if(Kntt1.GT.0)then
      do 220 m=1,Kntt1
      Ja=IBUF2E(m+Ibase)
      call unpck2
      F(I)=F(I)+D(J)*DBUF2E(Dcount+Dbase)
      F(J)=F(J)+D(I)*DBUF2E(Dcount+Dbase)
      Dcount=Dcount+2
220   continue
      Intcnt=Intcnt+Kntt1
      endif
      elseif(Mode.EQ.4)then
      
      Dcount=1
      if(Kntt1.GT.0)then
      do 240 m=1,Kntt1
      Ja=IBUF2E(m+Ibase)
      call unpck2
      F(I)=F(I)+D(J)*DBUF2E(Dcount+Dbase)
      F(J)=F(J)+D(I)*DBUF2E(Dcount+Dbase)
      Dcount=Dcount+3
240   continue
      Intcnt=Intcnt+Kntt1
      endif
      else
      
      
      if(Kntt1.GT.0)then
      jq=Ireset(1)+Ibase
      lq=jq+(Kntt1-1)*Nwpi
      do 260 m=jq,lq,Nwpi
      Ja=IBUF2E(m)
      Ia(1)=IBUF2E(m+1)
      Ia(2)=IBUF2E(m+2)
      call unpck4
      secint=-pt5*Valint
      js=Ij(I)+J
      jt=Ij(K)+L
      terint=Valint+Valint
      F(js)=F(js)+D(jt)*terint
      F(jt)=F(jt)+D(js)*terint
      js=Ij(I)+K
      if(J.LE.L)then
      
      jt=Ij(L)+J
      else
      jt=Ij(J)+L
      endif
      F(js)=F(js)+D(jt)*secint
      F(jt)=F(jt)+D(js)*secint
      js=Ij(I)+L
      if(J.LE.K)then
      
      jt=Ij(K)+J
      else
      jt=Ij(J)+K
      endif
      F(js)=F(js)+D(jt)*secint
      F(jt)=F(jt)+D(js)*secint
260   continue
      Intcnt=Intcnt+Kntt1
      endif
      
      if(Kntt2.GT.0)then
      lq=Ireset(2)+Ibase
      jq=lq-(Kntt2-1)*Nwpi
      do 280 m=jq,lq,Nwpi
      Ja=IBUF2E(m)
      Ia(1)=IBUF2E(m+1)
      Ia(2)=IBUF2E(m+2)
      call unpck4
      secint=-pt5*Valint
      Sindx=Sindx+1
      if(Sindx.EQ.2)then
      js=Ij(P)+Q
      jt=Ij(Q)+R
      terint=Valint-secint
      F(js)=F(js)+D(jt)*terint
      F(jt)=F(jt)+D(js)*terint
      js=Ij(Q+1)
      jt=Ij(P)+R
      F(js)=F(js)-D(jt)*Valint
      F(jt)=F(jt)+D(js)*secint
      elseif(Sindx.EQ.3)then
      js=Ij(P)+Q
      jt=Ij(R)+Q
      terint=Valint-secint
      F(js)=F(js)+D(jt)*terint
      F(jt)=F(jt)+D(js)*terint
      js=Ij(Q+1)
      jt=Ij(P)+R
      F(js)=F(js)-D(jt)*Valint
      F(jt)=F(jt)+D(js)*secint
      elseif(Sindx.EQ.4)then
      js=Ij(P)+Q
      terint=Valint-secint
      F(js)=F(js)+D(js)*terint
      js=Ij(P+1)
      jt=Ij(Q+1)
      F(js)=F(js)+D(jt)*secint
      F(jt)=F(jt)+D(js)*secint
      elseif(Sindx.EQ.5)then
      js=Ij(P+1)
      jt=Ij(Q)+R
      terint=Valint+Valint
      F(js)=F(js)+D(jt)*terint
      F(jt)=F(jt)+D(js)*Valint
      js=Ij(P)+Q
      jt=Ij(P)+R
      F(js)=F(js)+D(jt)*secint
      F(jt)=F(jt)+D(js)*secint
      elseif(Sindx.EQ.6)then
      js=Ij(P)+Q
      jt=Ij(R+1)
      terint=Valint+Valint
      F(js)=F(js)+D(jt)*Valint
      F(jt)=F(jt)+D(js)*terint
      js=Ij(P)+R
      if(Q.LE.R)then
      
      jt=Ij(R)+Q
      else
      jt=Ij(Q)+R
      endif
      F(js)=F(js)+D(jt)*secint
      F(jt)=F(jt)+D(js)*secint
      elseif(Sindx.EQ.7)then
      if(Mindx.EQ.2)then
      
      js=Ij(P+1)
      jt=Ij(Q+1)
      F(js)=F(js)+D(jt)*Valint
      F(jt)=F(jt)+D(js)*Valint
      js=Ij(P)+Q
      F(js)=F(js)+D(js)*secint
      elseif(Mindx.EQ.3)then
      
      js=Ij(P+1)
      jt=Ij(P)+Q
      F(js)=F(js)+D(jt)*Valint
      F(jt)=F(jt)-D(js)*secint
      else
      
      js=Ij(P)+Q
      jt=Ij(Q+1)
      F(js)=F(js)-D(jt)*secint
      F(jt)=F(jt)+D(js)*Valint
      endif
      elseif(Sindx.EQ.8)then
      js=Ij(P+1)
      F(js)=F(js)-D(js)*secint
      else
      js=Ij(P)+Q
      jt=Ij(P)+R
      terint=Valint-secint
      F(js)=F(js)+D(jt)*terint
      F(jt)=F(jt)+D(js)*terint
      js=Ij(P+1)
      jt=Ij(Q)+R
      F(js)=F(js)-D(jt)*Valint
      F(jt)=F(jt)+D(js)*secint
      endif
280   continue
      Intcnt=Intcnt+Kntt2
      endif
      endif
      
      
      
      if(iflst.LE.0)goto 200
      call iwind(intape)
      
      if(Intcnt.NE.Itotal)then
      write(Iout,99001)Intcnt,Itotal
      call lnk1e
      endif
      
      if(Isym2e.EQ.1)then
      call fsymm(NBASIS,F,NSYMOP,NEQBAS,Ij,D)
      do 300 I=1,Ntt
      F(I)=D(I)
300   continue
      endif
      
      call tread(Irwh,D(1),Ntt,1,Ntt,1,0)
      do 400 I=1,Ntt
      F(I)=F(I)+D(I)
400   continue
      
      call tread(Irwlp,D(1),Ntt,1,Ntt,1,0)
      
      if(IPRINT.GE.3)then
      write(Iout,99004)named
      call ltoutd(NBASIS,D,1)
      write(Iout,99004)namef
      call ltoutd(NBASIS,F,1)
      write(Iout,99005)
      endif
      
      return
      
      end
C* :1 * 
      
