
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fofopn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fofopn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "fofopn.web"
      subroutine fofopn(NBASIS,NSYMOP,NEQBAS)
      implicit none
      double precision Da,Db,Dbuf2e,Fa,Fb,Filabc,pspin,pspin2,pt25,ptotl
     &,ptotl2,secint,temp,terint,val1,val2,Valint,zero
      integer I,Ia,Ibasd,Ibase,Ibuf2e,Icon,Ifil,iflst,Ij,In,intape,Intcn
     &t,Iout,Ipunch,Iq,iqbufr,iqproc,Ireset,Irwc1,Irwc2
      integer Irwc3,Irwca,Irwcb,Irweig,Irwfa,Irwfb,Irwgen,Irwh,Irwibf,Ir
     &wpa,Irwpb,Irwps,Irwpt,Irws,Irwt,Irwtm,Irwur,Ismode,Istat,Isym2e
      integer Itotal,Iux,J,Ja,jq,js,jt,K,Kntt1,Kntt2,L,Last,Lenibf,Limin
     &t,lq,m,MAXBAS,Maxnbf,Maxntt,Mindx
      integer Mode,NBASIS,NEQBAS,next,nfile,Nrpext,NSYMOP,Ntt,Ntx,Nwiib,
     &Nwpi
      parameter(MAXBAS=150)
      dimension NEQBAS(MAXBAS,8)
      integer P,Q,R,Sindx
      integer Psave
      integer Dbase,Dbasd,dcount
      dimension Ia(2)
      dimension Ibuf2e(1)
      common/memry/Da(2850),Db(2850),Fa(2850),Fb(2850),Dbuf2e(4760),Fila
     &bc(33840)
      common/ind/Ntt,Ij(127)
      common/packed/I,J,K,L,Valint,Ja
      common/io/In,Iout,Ipunch
      common/psave/Psave
      common/max502/Maxnbf,Maxntt
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Isym2e
      common/irw502/Irwgen,Irweig,Irwca,Irwcb,Irwpa,Irwpb,Irwpt,Irwps,Ir
     &wfa,Irwfb,Irwur,Irws,Irwh,Irwt,Irwtm,Irwc1,Irwc2,Irwc3,Irwibf,Leni
     &bf
      equivalence(Ia(1),Valint)
      equivalence(P,I),(Q,J),(R,Mindx,K),(Sindx,L)
      equivalence(Ibuf2e(1),Dbuf2e(1))
      equivalence(pspin2,pspin),(ptotl2,ptotl)
      data pt25/0.25D0/
      data zero/0.0D0/
      data nfile/0/
      
      
      
      
      
      
99001 format(' BAD INTEGRAL COUNT:  IS ',i9,5x,' SHOULD BE ',i9)
      
      
      
      do 100 I=1,Ntt
      Fa(I)=zero
      Fb(I)=zero
100   continue
      
      if(Ismode.GT.0)then
      K=0
      do 150 I=1,NBASIS
      do 120 J=1,I
      K=K+1
      pspin=Da(K)-Db(K)
      ptotl=Da(K)+Db(K)
      Da(K)=ptotl+ptotl
      Db(K)=pspin+pspin
120   continue
      Da(K)=ptotl
      Db(K)=pspin
150   continue
      endif
      
      
      iqbufr=1
      iqproc=2
      Intcnt=0
      intape=Iux(2)
      Ntx=1
      call iread(intape,iqbufr,Dbuf2e)
      Ifil=1
200   call iwait(intape)
      iqbufr=iabs(iqbufr-2)+1
      iqproc=iabs(iqproc-2)+1
      Ibase=Ibasd(iqproc)
      Dbase=Dbasd(iqproc)
      call labscf(Ibuf2e(Ibase),iflst)
      if(iflst.EQ.0)then
      if(Ifil.EQ.(nfile+Ntx*Icon))then
      call iwind(intape)
      Ntx=Ntx+1
      next=Iux(Ntx+1)
      call iwind(next)
      intape=next
      endif
      call iread(intape,iqbufr,Dbuf2e)
      Ifil=Ifil+1
      endif
      
      
      if(Mode.EQ.2)then
      
      
      call lnk1e
      elseif(Mode.EQ.3)then
      elseif(Mode.NE.4)then
      
      
      if(Kntt1.GT.0)then
      jq=Ireset(1)+Ibase
      lq=jq+(Kntt1-1)*Nwpi
      do 220 m=jq,lq,Nwpi
      Ja=Ibuf2e(m)
      Ia(1)=Ibuf2e(m+1)
      Ia(2)=Ibuf2e(m+2)
      call unpck4
      js=Ij(I)+J
      jt=Ij(K)+L
      terint=Valint+Valint
      secint=(Da(jt)+Db(jt))*terint
      Fa(js)=Fa(js)+secint
      Fb(js)=Fb(js)+secint
      secint=(Da(js)+Db(js))*terint
      Fa(jt)=Fa(jt)+secint
      Fb(jt)=Fb(jt)+secint
      js=Ij(I)+K
      if(J.LE.L)then
      
      jt=Ij(L)+J
      else
      jt=Ij(J)+L
      endif
      Fa(js)=Fa(js)-Da(jt)*Valint
      Fb(js)=Fb(js)-Db(jt)*Valint
      Fa(jt)=Fa(jt)-Da(js)*Valint
      Fb(jt)=Fb(jt)-Db(js)*Valint
      js=Ij(I)+L
      if(J.LE.K)then
      
      jt=Ij(K)+J
      else
      jt=Ij(J)+K
      endif
      Fa(js)=Fa(js)-Da(jt)*Valint
      Fb(js)=Fb(js)-Db(jt)*Valint
      Fa(jt)=Fa(jt)-Da(js)*Valint
      Fb(jt)=Fb(jt)-Db(js)*Valint
220   continue
      Intcnt=Intcnt+Kntt1
      endif
      
      if(Kntt2.GT.0)then
      lq=Ireset(2)+Ibase
      jq=lq-(Kntt2-1)*Nwpi
      do 240 m=jq,lq,Nwpi
      Ja=Ibuf2e(m)
      Ia(1)=Ibuf2e(m+1)
      Ia(2)=Ibuf2e(m+2)
      call unpck4
      Sindx=Sindx+1
      if(Sindx.EQ.2)then
      js=Ij(P)+Q
      jt=Ij(Q)+R
      terint=Valint+Valint
      secint=(Da(jt)+Db(jt))*terint
      Fa(js)=Fa(js)+secint
      Fb(js)=Fb(js)+secint
      secint=(Da(js)+Db(js))*terint
      Fa(jt)=Fa(jt)+secint
      Fb(jt)=Fb(jt)+secint
      Fa(js)=Fa(js)-Da(jt)*Valint
      Fb(js)=Fb(js)-Db(jt)*Valint
      Fa(jt)=Fa(jt)-Da(js)*Valint
      Fb(jt)=Fb(jt)-Db(js)*Valint
      js=Ij(P)+R
      jt=Ij(Q+1)
      Fa(js)=Fa(js)-Da(jt)*Valint
      Fb(js)=Fb(js)-Db(jt)*Valint
      Fa(jt)=Fa(jt)-Da(js)*terint
      Fb(jt)=Fb(jt)-Db(js)*terint
      elseif(Sindx.EQ.3)then
      js=Ij(P)+Q
      jt=Ij(R)+Q
      terint=Valint+Valint
      secint=(Da(jt)+Db(jt))*terint
      Fa(js)=Fa(js)+secint
      Fb(js)=Fb(js)+secint
      secint=(Da(js)+Db(js))*terint
      Fa(jt)=Fa(jt)+secint
      Fb(jt)=Fb(jt)+secint
      Fa(js)=Fa(js)-Da(jt)*Valint
      Fb(js)=Fb(js)-Db(jt)*Valint
      Fa(jt)=Fa(jt)-Da(js)*Valint
      Fb(jt)=Fb(jt)-Db(js)*Valint
      js=Ij(P)+R
      jt=Ij(Q+1)
      Fa(js)=Fa(js)-Da(jt)*Valint
      Fb(js)=Fb(js)-Db(jt)*Valint
      Fa(jt)=Fa(jt)-Da(js)*terint
      Fb(jt)=Fb(jt)-Db(js)*terint
      elseif(Sindx.EQ.4)then
      js=Ij(P)+Q
      secint=Da(js)+Db(js)
      Fa(js)=Fa(js)+(secint+Db(js))*Valint
      Fb(js)=Fb(js)+(secint+Da(js))*Valint
      js=Ij(P+1)
      jt=Ij(Q+1)
      Fa(js)=Fa(js)-Da(jt)*Valint
      Fb(js)=Fb(js)-Db(jt)*Valint
      Fa(jt)=Fa(jt)-Da(js)*Valint
      Fb(jt)=Fb(jt)-Db(js)*Valint
      elseif(Sindx.EQ.5)then
      js=Ij(P+1)
      jt=Ij(Q)+R
      secint=(Da(jt)+Db(jt))*Valint
      secint=secint+secint
      Fa(js)=Fa(js)+secint
      Fb(js)=Fb(js)+secint
      secint=(Da(js)+Db(js))*Valint
      Fa(jt)=Fa(jt)+secint
      Fb(jt)=Fb(jt)+secint
      js=Ij(P)+Q
      jt=Ij(P)+R
      Fa(js)=Fa(js)-Da(jt)*Valint
      Fb(js)=Fb(js)-Db(jt)*Valint
      Fa(jt)=Fa(jt)-Da(js)*Valint
      Fb(jt)=Fb(jt)-Db(js)*Valint
      elseif(Sindx.EQ.6)then
      js=Ij(P)+Q
      jt=Ij(R+1)
      secint=(Da(jt)+Db(jt))*Valint
      Fa(js)=Fa(js)+secint
      Fb(js)=Fb(js)+secint
      secint=(Da(js)+Db(js))*Valint
      secint=secint+secint
      Fa(jt)=Fa(jt)+secint
      Fb(jt)=Fb(jt)+secint
      js=Ij(P)+R
      if(Q.LE.R)then
      
      jt=Ij(R)+Q
      else
      jt=Ij(Q)+R
      endif
      Fa(js)=Fa(js)-Da(jt)*Valint
      Fb(js)=Fb(js)-Db(jt)*Valint
      Fa(jt)=Fa(jt)-Da(js)*Valint
      Fb(jt)=Fb(jt)-Db(js)*Valint
      elseif(Sindx.EQ.7)then
      if(Mindx.EQ.2)then
      
      js=Ij(P+1)
      jt=Ij(Q+1)
      secint=(Da(jt)+Db(jt))*Valint
      Fa(js)=Fa(js)+secint
      Fb(js)=Fb(js)+secint
      secint=(Da(js)+Db(js))*Valint
      Fa(jt)=Fa(jt)+secint
      Fb(jt)=Fb(jt)+secint
      js=Ij(P)+Q
      Fa(js)=Fa(js)-Da(js)*Valint
      Fb(js)=Fb(js)-Db(js)*Valint
      elseif(Mindx.EQ.3)then
      
      js=Ij(P+1)
      jt=Ij(P)+Q
      secint=Valint+Valint
      Fa(js)=Fa(js)+Db(jt)*secint
      Fb(js)=Fb(js)+Da(jt)*secint
      Fa(jt)=Fa(jt)+Db(js)*Valint
      Fb(jt)=Fb(jt)+Da(js)*Valint
      else
      
      js=Ij(P)+Q
      jt=Ij(Q+1)
      Fa(js)=Fa(js)+Db(jt)*Valint
      Fb(js)=Fb(js)+Da(jt)*Valint
      secint=Valint+Valint
      Fa(jt)=Fa(jt)+Db(js)*secint
      Fb(jt)=Fb(jt)+Da(js)*secint
      endif
      elseif(Sindx.EQ.8)then
      js=Ij(P+1)
      Fa(js)=Fa(js)+Db(js)*Valint
      Fb(js)=Fb(js)+Da(js)*Valint
      else
      js=Ij(P)+Q
      jt=Ij(P)+R
      terint=Valint+Valint
      secint=(Da(jt)+Db(jt))*terint
      Fa(js)=Fa(js)+secint
      Fb(js)=Fb(js)+secint
      secint=(Da(js)+Db(js))*terint
      Fa(jt)=Fa(jt)+secint
      Fb(jt)=Fb(jt)+secint
      Fa(jt)=Fa(jt)-Da(js)*Valint
      Fb(jt)=Fb(jt)-Db(js)*Valint
      Fa(js)=Fa(js)-Da(jt)*Valint
      Fb(js)=Fb(js)-Db(jt)*Valint
      js=Ij(P+1)
      jt=Ij(Q)+R
      Fa(js)=Fa(js)-Da(jt)*terint
      Fb(js)=Fb(js)-Db(jt)*terint
      Fa(jt)=Fa(jt)-Da(js)*Valint
      Fb(jt)=Fb(jt)-Db(js)*Valint
      endif
240   continue
      Intcnt=Intcnt+Kntt2
      endif
      goto 300
      endif
      
      dcount=1
      if(Kntt1.GT.0)then
      do 250 m=1,Kntt1
      Ja=Ibuf2e(m+Ibase)
      call unpck2
      val1=Dbuf2e(dcount+Dbase)
      val2=Dbuf2e(dcount+1+Dbase)
      dcount=dcount+Ismode
      Fa(I)=Fa(I)+Da(J)*val1
      Fa(J)=Fa(J)+Da(I)*val1
      Fb(I)=Fb(I)+Db(J)*val2
      Fb(J)=Fb(J)+Db(I)*val2
250   continue
      Intcnt=Intcnt+Kntt1
      endif
      
      
300   if(iflst.LE.0)goto 200
      call iwind(intape)
      
      if(Intcnt.NE.Itotal)then
      write(Iout,99001)Intcnt,Itotal
      call lnk1e
      endif
      
      
      if(Isym2e.EQ.1)then
      call fsymm(NBASIS,Fa,NSYMOP,NEQBAS,Ij,Da)
      call fsymm(NBASIS,Fb,NSYMOP,NEQBAS,Ij,Db)
      do 350 I=1,Ntt
      Fa(I)=Da(I)
      Fb(I)=Db(I)
350   continue
      endif
      
      call tread(Irwh,Da,Ntt,1,Ntt,1,0)
      if(Ismode.EQ.0)then
      
      do 400 I=1,Ntt
      Fa(I)=Fa(I)+Da(I)
      Fb(I)=Fb(I)+Da(I)
400   continue
      else
      do 450 I=1,Ntt
      temp=pt25*Fb(I)
      Fb(I)=Da(I)+Fa(I)+temp
      Fa(I)=Da(I)+Fa(I)-temp
450   continue
      endif
      
      call tread(Irwpa,Da,Ntt,1,Ntt,1,0)
      call tread(Irwpb,Db,Ntt,1,Ntt,1,0)
      
      return
      
      end
C* :1 * 
      
