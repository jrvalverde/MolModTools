
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fofphf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fofphf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "fofphf.web"
      subroutine fofphf(JCYCLE,INSIDE,NBASIS,NSYMOP,NEQBAS,IRWH)
      implicit none
      double precision D,D1,Dbuf2e,F,F1,fact,Filabc,Fill505,pspin,pt25,p
     &t5,ptotl,temp,terint,val1,val2,Valint
      integer I,Ia,Ibasd,Ibase,Ibuf2e,Icon,Id505,Ifil,Ifill,iflst,Ij,In,
     &INSIDE,intape,Intcnt,Iout,Ipunch,Iq,iqbufr,iqproc
      integer Ireset,IRWH,irwpa,irwpb,Ismode,Istat,Isym2e,Itotal,Iux,J,J
     &a,JCYCLE,jq,js,jt,K,Kntt1,Kntt2,L,Last
      integer Limint,lq,m,MAXBAS,Maxnbf,Maxntt,Mindx,Mode,NBASIS,NEQBAS,
     &nfile,Nrpext,NSYMOP,Ntt,Ntx,Nwiib,Nwpi
      parameter(MAXBAS=150)
      integer P,Q,R,Sindx
      integer Psave
      integer Dbase,Dbasd,dcount
      dimension Ibuf2e(1),NEQBAS(MAXBAS,8)
      dimension Ia(2)
      common/memry/D1(2485),D(2485),F(2485),F1(2485),Dbuf2e(4760),Fill50
     &5(210),Id505,Ifill,Filabc(35089)
      common/jnkphf/Ntt,Ij(71)
      common/max505/Maxnbf,Maxntt
      common/io/In,Iout,Ipunch
      common/packed/I,J,K,L,Valint,Ja
      common/psave/Psave
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Isym2e
      equivalence(Ibuf2e(1),Dbuf2e(1))
      equivalence(P,I),(Q,J),(R,K),(Sindx,L),(Mindx,K)
      equivalence(Ia(1),Valint)
      data pt5/0.5D0/,pt25/0.25D0/
      data nfile/0/
      data irwpa/528/,irwpb/530/
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(1H+,22x,i9)
99002 format(25H1BAD INTEGRAL COUNT:  IS ,i9,5x,12H  SHOULD BE ,i9)
      
      
      if(Ismode.GT.0)then
      
      if(INSIDE.EQ.2)then
      
      fact=-pt25
      elseif(INSIDE.EQ.3)then
      
      fact=+pt25
      else
      
      K=0
      do 20 I=1,NBASIS
      do 10 J=1,I
      K=K+1
      ptotl=D1(K)+D(K)
      D1(K)=ptotl+ptotl
10    continue
      D1(K)=D1(K)*pt5
20    continue
      goto 300
      endif
      K=0
      do 50 I=1,NBASIS
      do 40 J=1,I
      K=K+1
      ptotl=D1(K)+D(K)
      pspin=fact*(D1(K)-D(K))
      D1(K)=ptotl+ptotl
      D(K)=pspin+pspin
40    continue
      D1(K)=D1(K)*pt5
      D(K)=D(K)*pt5
50    continue
      
      elseif(INSIDE.EQ.2)then
      
      do 100 I=1,Ntt
      D(I)=D1(I)+D(I)
100   continue
      elseif(INSIDE.EQ.3)then
      
      do 150 I=1,Ntt
      temp=D1(I)
      D1(I)=D(I)
      D(I)=D1(I)+temp
150   continue
      else
      
      do 200 I=1,Ntt
      D(I)=D1(I)+D(I)
      D1(I)=pt5*D(I)
200   continue
      endif
      
      
300   call aclear(Ntt,F(1))
      
      
      
      iqbufr=1
      iqproc=2
      Intcnt=0
      intape=Iux(2)
      Ntx=1
      call iread(intape,iqbufr,Dbuf2e)
      Ifil=1
400   call iwait(intape)
      iqbufr=iabs(iqbufr-2)+1
      iqproc=iabs(iqproc-2)+1
      Ibase=Ibasd(iqproc)
      Dbase=Dbasd(iqproc)
      call labscf(Ibuf2e(Ibase),iflst)
      if(iflst.EQ.0)then
      if(Ifil.EQ.(nfile+Ntx*Icon))then
      call iwind(intape)
      Ntx=Ntx+1
      intape=Iux(Ntx+1)
      call iwind(intape)
      endif
      call iread(intape,iqbufr,Dbuf2e)
      Ifil=Ifil+1
      endif
      
      if(Mode.EQ.1)then
      elseif(Mode.EQ.3)then
      goto 600
      elseif(Mode.EQ.4)then
      goto 600
      else
      
      call lnk1e
      endif
      
      if(Kntt1.GT.0)then
      jq=Ireset(1)+Ibase
      lq=jq+(Kntt1-1)*Nwpi
      do 450 m=jq,lq,Nwpi
      Ja=Ibuf2e(m)
      Ia(1)=Ibuf2e(m+1)
      Ia(2)=Ibuf2e(m+2)
      call unpck4
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
      F(js)=F(js)-D1(jt)*Valint
      F(jt)=F(jt)-D1(js)*Valint
      js=Ij(I)+L
      if(J.LE.K)then
      
      jt=Ij(K)+J
      else
      jt=Ij(J)+K
      endif
      F(js)=F(js)-D1(jt)*Valint
      F(jt)=F(jt)-D1(js)*Valint
450   continue
      Intcnt=Intcnt+Kntt1
      endif
      
      if(Kntt2.GT.0)then
      lq=Ireset(2)+Ibase
      jq=lq-(Kntt2-1)*Nwpi
      do 500 m=jq,lq,Nwpi
      Ja=Ibuf2e(m)
      Ia(1)=Ibuf2e(m+1)
      Ia(2)=Ibuf2e(m+2)
      call unpck4
      Sindx=Sindx+1
      if(Sindx.EQ.2)then
      js=Ij(P)+Q
      jt=Ij(Q)+R
      terint=Valint+Valint
      F(js)=F(js)+D(jt)*terint
      F(jt)=F(jt)+D(js)*terint
      F(js)=F(js)-D1(jt)*Valint
      F(jt)=F(jt)-D1(js)*Valint
      js=Ij(P)+R
      jt=Ij(Q+1)
      F(js)=F(js)-D1(jt)*Valint
      F(jt)=F(jt)-D1(js)*terint
      elseif(Sindx.EQ.3)then
      js=Ij(P)+Q
      jt=Ij(R)+Q
      terint=Valint+Valint
      F(js)=F(js)+D(jt)*terint
      F(jt)=F(jt)+D(js)*terint
      F(js)=F(js)-D1(jt)*Valint
      F(jt)=F(jt)-D1(js)*Valint
      js=Ij(P)+R
      jt=Ij(Q+1)
      F(js)=F(js)-D1(jt)*Valint
      F(jt)=F(jt)-D1(js)*terint
      elseif(Sindx.EQ.4)then
      js=Ij(P)+Q
      F(js)=F(js)+(D(js)+D(js)-D1(js))*Valint
      js=Ij(P+1)
      jt=Ij(Q+1)
      F(js)=F(js)-D1(jt)*Valint
      F(jt)=F(jt)-D1(js)*Valint
      elseif(Sindx.EQ.5)then
      js=Ij(P+1)
      jt=Ij(Q)+R
      F(js)=F(js)+(D(jt)+D(jt))*Valint
      F(jt)=F(jt)+D(js)*Valint
      js=Ij(P)+Q
      jt=Ij(P)+R
      F(js)=F(js)-D1(jt)*Valint
      F(jt)=F(jt)-D1(js)*Valint
      elseif(Sindx.EQ.6)then
      js=Ij(P)+Q
      jt=Ij(R+1)
      F(js)=F(js)+D(jt)*Valint
      F(jt)=F(jt)+(D(js)+D(js))*Valint
      js=Ij(P)+R
      if(Q.LE.R)then
      
      jt=Ij(R)+Q
      else
      jt=Ij(Q)+R
      endif
      F(js)=F(js)-D1(jt)*Valint
      F(jt)=F(jt)-D1(js)*Valint
      elseif(Sindx.EQ.7)then
      if(Mindx.EQ.2)then
      
      js=Ij(P+1)
      jt=Ij(Q+1)
      F(js)=F(js)+D(jt)*Valint
      F(jt)=F(jt)+D(js)*Valint
      js=Ij(P)+Q
      F(js)=F(js)-D1(js)*Valint
      elseif(Mindx.EQ.3)then
      
      js=Ij(P+1)
      jt=Ij(P)+Q
      F(js)=F(js)+(D(jt)-D1(jt))*(Valint+Valint)
      F(jt)=F(jt)+(D(js)-D1(js))*Valint
      else
      
      js=Ij(P)+Q
      jt=Ij(Q+1)
      F(js)=F(js)+(D(jt)-D1(jt))*Valint
      F(jt)=F(jt)+(D(js)-D1(js))*(Valint+Valint)
      endif
      elseif(Sindx.EQ.8)then
      js=Ij(P+1)
      F(js)=F(js)+(D(js)-D1(js))*Valint
      else
      js=Ij(P)+Q
      jt=Ij(P)+R
      terint=Valint+Valint
      F(js)=F(js)+D(jt)*terint
      F(jt)=F(jt)+D(js)*terint
      F(jt)=F(jt)-D1(js)*Valint
      F(js)=F(js)-D1(jt)*Valint
      js=Ij(P+1)
      jt=Ij(Q)+R
      F(js)=F(js)-D1(jt)*terint
      F(jt)=F(jt)-D1(js)*Valint
      endif
500   continue
      Intcnt=Intcnt+Kntt2
      endif
      goto 700
      
      
600   dcount=1
      if(Kntt1.GT.0)then
      if(INSIDE.LE.1)then
      do 620 m=1,Kntt1
      Ja=Ibuf2e(m+Ibase)
      call unpck2
      F(I)=F(I)+D1(J)*Dbuf2e(dcount+Dbase)
      F(J)=F(J)+D1(I)*Dbuf2e(dcount+Dbase)
      dcount=dcount+Ismode
620   continue
      Intcnt=Intcnt+Kntt1
      else
      
      do 640 m=1,Kntt1
      Ja=Ibuf2e(m+Ibase)
      call unpck2
      val1=Dbuf2e(dcount+Dbase)
      val2=Dbuf2e(dcount+1+Dbase)
      F(I)=F(I)+D1(J)*val1+D(J)*val2
      F(J)=F(J)+D1(I)*val1+D(I)*val2
      dcount=dcount+Ismode
640   continue
      Intcnt=Intcnt+Kntt1
      endif
      endif
      
      
      
700   if(iflst.LE.0)goto 400
      call iwind(intape)
      if(Intcnt.NE.Itotal)then
      write(Iout,99002)Intcnt,Itotal
      call lnk1e
      endif
      
      if(Isym2e.NE.0)then
      call fsymm(NBASIS,F,NSYMOP,NEQBAS,Ij,D)
      call amove(Ntt,D,F)
      endif
      
      call tread(IRWH,D(1),Ntt,1,Ntt,1,0)
      call aadd(Ntt,D,F,F)
      
      call tread(irwpa,D1(1),Ntt,1,Ntt,1,0)
      call tread(irwpb,D(1),Ntt,1,Ntt,1,0)
      return
      
      end
C* :1 * 
      
