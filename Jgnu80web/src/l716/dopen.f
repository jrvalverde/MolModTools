
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dopen"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dopen.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 31 "dopen.web"
      subroutine dopen(NATOMS,NBASIS,FXYZ,DA,DB,DBUF2E,IBUF2E,IJ,IDUMP)
      implicit none
      double precision DA,DB,DBUF2E,dijkl,four,fx,FXYZ,fy,fz,pt5,two,Val
     &int,x,zero
      integer I,iatx,iaty,iatz,Ibasd,Ibase,Ibfpad,IBUF2E,Icon,Icount,Idr
     &v1,IDUMP,Ifil,iflst,ii,iia,IJ,In,inst,intape
      integer Intcnt,Iout,Ipunch,Iq,iqbufr,iqproc,Ireset,Irwibf,Irwpa,Ir
     &wpb,Irwpt,Irww,Ismode,Istat,Itotal,Iux,J,Ja,jatx,jaty
      integer jatz,K,katx,katy,katz,Kntt1,Kntt2,L,Last,latx,laty,latz,Le
     &nibf,Limint,m,maxm,mij,mik,mil,Mindx
      integer mjk,mjl,mkl,Mode,mpp,mpq,mpr,mqq,mqr,mrq,mrr,nat3,NATOMS,N
     &BASIS,nbsp,next,nfile,Nrpext,nset,nstt
      integer ntt,Ntx,Nwiib,nwiib2,Nwpi
      integer P,Q,R,Sindx
      integer Dbase,Dbasd,Dcount
      dimension FXYZ(*),DA(*),DB(*),DBUF2E(*),IBUF2E(*),IJ(*)
      dimension iia(18),x(9),fx(3),fy(3),fz(3)
      common/io/In,Iout,Ipunch
      common/packed/I,J,K,L,Valint,Ja
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Ibfpad
      common/irw716/Irww,Irwpt,Idrv1,Irwpa,Irwpb
      common/irwibf/Irwibf,Lenibf
      equivalence(P,I),(Q,J),(R,K),(Sindx,L),(Mindx,K)
      equivalence(iia(1),x(1))
      equivalence(Kntt1,Icount),(Kntt2,Dcount)
      data zero,pt5,two,four/0.D0,0.5D0,2.D0,4.D0/
      data nfile/0/
      
      
      
      
      
      
      
      
      
      
99001 format(2x,6F12.7)
99002 format(25H1BAD INTEGRAL COUNT:  IS ,i9,5x,12H  SHOULD BE ,i9)
99003 format(1x,7(2x,i20))
99004 format(' DUMP OF TWO ELECTRON INTEGRAL DERIVATIVE CONTRIBUTION ','
     &TO THE FORCES')
99005 format(2x,3F10.7)
      
      ntt=NBASIS*(NBASIS+1)/2
      
      call tread(Irwpa,DA,ntt,1,ntt,1,0)
      call tread(Irwpb,DB,ntt,1,ntt,1,0)
      
      call tread(Irwibf,Ismode,Lenibf,1,Lenibf,1,0)
      Nwpi=0
      nset=0
      nat3=3*NATOMS
      nbsp=NBASIS+1
      do 100 ii=1,nbsp
      IJ(ii)=ii*(ii-1)/2
100   continue
      iflst=0
      iqbufr=1
      iqproc=2
      Intcnt=0
      intape=Iux(2)
      Ntx=1
      nwiib2=300
      call iread(intape,iqbufr,DBUF2E)
      Ifil=1
200   if(iflst.GT.0)then
      
      
      
      call iwind(intape)
      if(Intcnt.NE.Itotal)then
      write(Iout,99002)Intcnt,Itotal
      call lnk1e
      endif
      else
      call iwait(intape)
      iqbufr=iabs(iqbufr-2)+1
      iqproc=iabs(iqproc-2)+1
      Ibase=Ibasd(iqproc)
      if(IBUF2E(Ibase).LT.0)iflst=1
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
      
      
      m=Ireset(1)+Ibase-Nwpi
      if(IDUMP.GT.2)write(Iout,99003)(IBUF2E(Ibase-1+K),K=1,nwiib2)
      Kntt1=iabs(IBUF2E(Ibase))
      maxm=Ibase+Kntt1-1
250   m=m+Nwpi
      if(m.GT.maxm)goto 200
      Intcnt=Intcnt+1
      Ja=IBUF2E(m)
      if(Ja.LT.0)then
      
      
      Ja=-Ja
      call unpck4
      Sindx=Sindx+1
      if(Sindx.EQ.2)then
      mpq=IJ(P)+Q
      mqr=IJ(Q)+R
      mqq=IJ(Q+1)
      mpr=IJ(P)+R
      dijkl=four*((DA(mpq)+DB(mpq))*(DA(mqr)+DB(mqr)))-two*(DA(mpq)*DA(m
     &qr)+DB(mpq)*DB(mqr)+DA(mpr)*DA(mqq)+DB(mpr)*DB(mqq))
      elseif(Sindx.EQ.3)then
      mpq=IJ(P)+Q
      mrq=IJ(R)+Q
      mqq=IJ(Q+1)
      mpr=IJ(P)+R
      dijkl=four*((DA(mpq)+DB(mpq))*(DA(mrq)+DB(mrq)))-two*(DA(mpr)*DA(m
     &qq)+DB(mpr)*DB(mqq)+DA(mpq)*DA(mrq)+DB(mpq)*DB(mrq))
      elseif(Sindx.EQ.4)then
      mpq=IJ(P)+Q
      mpp=IJ(P+1)
      mqq=IJ(Q+1)
      dijkl=two*((DA(mpq)+DB(mpq))*(DA(mpq)+DB(mpq)))-(DA(mpp)*DA(mqq)+D
     &B(mpp)*DB(mqq)+DA(mpq)*DA(mpq)+DB(mpq)*DB(mpq))
      elseif(Sindx.EQ.5)then
      mpp=IJ(P+1)
      mqr=IJ(Q)+R
      mpq=IJ(P)+Q
      mpr=IJ(P)+R
      dijkl=two*((DA(mpp)+DB(mpp))*(DA(mqr)+DB(mqr)))-two*(DA(mpq)*DA(mp
     &r)+DB(mpq)*DB(mpr))
      elseif(Sindx.EQ.6)then
      mpq=IJ(P)+Q
      mrr=IJ(R+1)
      mpr=IJ(P)+R
      if(Q.LE.R)then
      
      mqr=IJ(R)+Q
      else
      mqr=IJ(Q)+R
      endif
      dijkl=two*((DA(mpq)+DB(mpq))*(DA(mrr)+DB(mrr)))-two*(DA(mpr)*DA(mq
     &r)+DB(mpr)*DB(mqr))
      elseif(Sindx.EQ.7)then
      if(Mindx.EQ.2)then
      
      mpp=IJ(P+1)
      mqq=IJ(Q+1)
      mpq=IJ(P)+Q
      dijkl=((DA(mpp)+DB(mpp))*(DA(mqq)+DB(mqq)))-(DA(mpq)*DA(mpq)+DB(mp
     &q)*DB(mpq))
      elseif(Mindx.EQ.3)then
      
      mpp=IJ(P+1)
      mpq=IJ(P)+Q
      dijkl=two*((DA(mpp)+DB(mpp))*(DA(mpq)+DB(mpq)))-two*(DA(mpp)*DA(mp
     &q)+DB(mpp)*DB(mpq))
      else
      
      mpq=IJ(P)+Q
      mqq=IJ(Q+1)
      dijkl=two*((DA(mpq)+DB(mpq))*(DA(mqq)+DB(mqq)))-two*(DA(mpq)*DA(mq
     &q)+DB(mpq)*DB(mqq))
      endif
      elseif(Sindx.EQ.8)then
      mpp=IJ(P+1)
      dijkl=pt5*((DA(mpp)+DB(mpp))**2)-pt5*(DA(mpp)*DA(mpp)+DB(mpp)*DB(m
     &pp))
      else
      mpq=IJ(P)+Q
      mpr=IJ(P)+R
      mpp=IJ(P+1)
      mqr=IJ(Q)+R
      dijkl=four*((DA(mpq)+DB(mpq))*(DA(mpr)+DB(mpr)))-two*(DA(mpp)*DA(m
     &qr)+DB(mpp)*DB(mqr)+DA(mpr)*DA(mpq)+DB(mpr)*DB(mpq))
      endif
      elseif(Ja.EQ.0)then
      
      
      if(nset.NE.0)then
      if(IDUMP.GE.2)write(Iout,99001)fx,fy,fz
      FXYZ(latx)=FXYZ(latx)-fx(1)-fx(2)-fx(3)
      FXYZ(laty)=FXYZ(laty)-fy(1)-fy(2)-fy(3)
      FXYZ(latz)=FXYZ(latz)-fz(1)-fz(2)-fz(3)
      if(nset.EQ.1)goto 260
      if(nset.NE.2)then
      
      FXYZ(katx)=FXYZ(katx)+fx(3)
      FXYZ(katy)=FXYZ(katy)+fy(3)
      FXYZ(katz)=FXYZ(katz)+fz(3)
      endif
      FXYZ(jatx)=FXYZ(jatx)+fx(2)
      FXYZ(jaty)=FXYZ(jaty)+fy(2)
      FXYZ(jatz)=FXYZ(jatz)+fz(2)
260   FXYZ(iatx)=FXYZ(iatx)+fx(1)
      FXYZ(iaty)=FXYZ(iaty)+fy(1)
      FXYZ(iatz)=FXYZ(iatz)+fz(1)
      endif
      Ja=IBUF2E(m+1)
      call unpck4
      nset=3
      if(K.EQ.0)nset=2
      if(J.EQ.0)nset=1
      nstt=6*nset
      Nwpi=6*nset+1
      m=m+2-Nwpi
      if(nset.EQ.1)goto 280
      if(nset.NE.2)then
      
      katx=3*K-2
      katy=katx+1
      katz=katy+1
      endif
      jatx=3*J-2
      jaty=jatx+1
      jatz=jaty+1
280   iatx=3*I-2
      iaty=iatx+1
      iatz=iaty+1
      latx=3*L-2
      laty=latx+1
      latz=laty+1
      do 300 ii=1,3
      fx(ii)=zero
      fy(ii)=zero
      fz(ii)=zero
300   continue
      goto 250
      else
      
      
      call unpck4
      mij=IJ(I)+J
      mkl=IJ(K)+L
      mik=IJ(I)+K
      if(J.LE.L)then
      
      mjl=IJ(L)+J
      else
      mjl=IJ(J)+L
      endif
      mil=IJ(I)+L
      if(J.LE.K)then
      
      mjk=IJ(K)+J
      else
      mjk=IJ(J)+K
      endif
      dijkl=four*((DA(mij)+DB(mij))*(DA(mkl)+DB(mkl)))-two*(DA(mik)*DA(m
     &jl)+DB(mik)*DB(mjl)+DA(mil)*DA(mjk)+DB(mil)*DB(mjk))
      endif
      
      
      do 350 ii=1,nstt
      iia(ii)=IBUF2E(m+ii)
350   continue
      inst=-2
      do 400 ii=1,nset
      inst=inst+3
      fx(ii)=fx(ii)+x(inst)*dijkl
      fy(ii)=fy(ii)+x(inst+1)*dijkl
      fz(ii)=fz(ii)+x(inst+2)*dijkl
400   continue
      goto 250
      endif
      
      if(IDUMP.GE.2)write(Iout,99004)
      if(IDUMP.GE.2)write(Iout,99005)(FXYZ(ii),ii=1,nat3)
      
      return
      
      end
C* :1 * 
      
