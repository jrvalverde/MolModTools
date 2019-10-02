
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 wtilda"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "wtilda.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "wtilda.web"
      subroutine wtilda
      implicit none
      double precision a0,a1,Cmo,Cut,E,V,V1,Valint,X
      integer I,Iao,Ibasd,Ibase,Ibfpad,Icon,Icount,Ifil,iflst,Iia,ij,In,
     &intape,Intc,Intcnt,Intt,Iout,Ipunch,Iq,iqbufr
      integer iqproc,Ireset,Ismode,Istat,Itotal,Iux,J,Ja,jji,jjk,jjl,jki
     &,jli,jlj,jq,K,Kntt1,Kntt2,L,Last
      integer Limint,Lpair,lq,Ls,m,Mdv,Mindx,Mode,nfile,Nr,Nrpext,Ntx,Nw
     &iib,Nwpi
      integer P,Q,R,Sindx
      integer Dbase,Dbasd,Dcount
      dimension Iao(25250)
      dimension Iia(2)
      common/v/V(10000),V1(10000),Mdv
      common/moc/Cmo(12625),E(175)
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Ibfpad
      common/packed/I,J,K,L,Valint,Ja
      common/wtild/Cut,Ls(81),Lpair,Nr,Intt,Intc
      common/io/In,Iout,Ipunch
      equivalence(Iao(1),Cmo(1))
      equivalence(P,I),(Q,J),(R,K),(Sindx,L),(Mindx,K)
      equivalence(Valint,Iia(1)),(Valint,X)
      equivalence(Icount,Kntt1),(Dcount,Kntt2)
      data nfile/0/
      
      
      
      
      
      
      
      
      
      
      
      
      call track('WTILDA')
      
      
      Ntx=1
      intape=Iux(2)
      iqbufr=1
      iqproc=2
      call iwind(intape)
      call iread(intape,iqbufr,Cmo)
      Ifil=1
100   call iwait(intape)
      
      iqbufr=iabs(iqbufr-2)+1
      iqproc=iabs(iqproc-2)+1
      Ibase=Ibasd(iqproc)
      Dbase=Dbasd(iqproc)
      call labscf(Iao(Ibase),iflst)
      if(iflst.EQ.0)then
      if(Ifil.EQ.(nfile+Ntx*Icon))then
      call iwind(intape)
      
      Ntx=Ntx+1
      intape=Iux(Ntx+1)
      call iwind(intape)
      endif
      call iread(intape,iqbufr,Cmo)
      
      Ifil=Ifil+1
      endif
      
      if(Mode.NE.1)call lnk1e
      
      if(Kntt1.GT.0)then
      jq=Ireset(1)+Ibase
      lq=jq+(Kntt1-1)*Nwpi
      
      do 150 m=jq,lq,Nwpi
      Ja=Iao(m)
      Iia(1)=Iao(m+1)
      Iia(2)=Iao(m+2)
      if(dabs(X).GE.Cut)then
      Intc=Intc+1
      call unpck4
      jki=Ls(K)+I
      jli=Ls(L)+I
      if(J.GE.L)then
      
      jjl=Ls(L)+J
      a0=-X
      else
      jjl=Ls(J)+L
      a0=X
      endif
      if(J.GT.K)then
      
      jjk=Ls(K)+J
      a1=-X
      else
      jjk=Ls(J)+K
      a1=X
      endif
      do 110 ij=1,Nr
      V1(jki)=V1(jki)-a0*V(jjl)
      V1(jli)=V1(jli)-a1*V(jjk)
      V1(jjk)=V1(jjk)-a1*V(jli)
      V1(jjl)=V1(jjl)-a0*V(jki)
      jki=jki+Lpair
      jli=jli+Lpair
      jjl=jjl+Lpair
      jjk=jjk+Lpair
110   continue
      endif
150   continue
      
      Intt=Intt+Kntt1
      endif
      
      if(Kntt2.GT.0)then
      lq=Ireset(2)+Ibase
      jq=lq-(Kntt2-1)*Nwpi
      
      do 200 m=jq,lq,Nwpi
      Ja=Iao(m)
      Iia(1)=Iao(m+1)
      Iia(2)=Iao(m+2)
      if(dabs(X).GE.Cut)then
      Intc=Intc+1
      call unpck4
      Sindx=Sindx+1
      
      if(Sindx.EQ.1)goto 180
      if(Sindx.EQ.2)then
      
      jji=Ls(Q)+P
      jlj=Ls(R)+Q
      do 155 ij=1,Nr
      V1(jji)=V1(jji)+X*V(jlj)
      V1(jlj)=V1(jlj)+X*V(jji)
      jji=jji+Lpair
      jlj=jlj+Lpair
155   continue
      elseif(Sindx.EQ.3)then
      
      jji=Ls(Q)+P
      jjk=Ls(Q)+R
      do 160 ij=1,Nr
      V1(jji)=V1(jji)-X*V(jjk)
      V1(jjk)=V1(jjk)-X*V(jji)
      jji=jji+Lpair
      jjk=jjk+Lpair
160   continue
      elseif(Sindx.EQ.4)then
      
      jji=Ls(Q)+P
      do 165 ij=1,Nr
      V1(jji)=V1(jji)-X*V(jji)
      jji=jji+Lpair
165   continue
      elseif(Sindx.EQ.5)then
      
      jki=Ls(Q)+P
      jli=Ls(R)+P
      do 170 ij=1,Nr
      V1(jki)=V1(jki)+X*V(jli)
      V1(jli)=V1(jli)+X*V(jki)
      jki=jki+Lpair
      jli=jli+Lpair
170   continue
      elseif(Sindx.EQ.6)then
      
      jki=Ls(R)+P
      if(Q.GT.R)then
      
      jjk=Ls(R)+Q
      X=-X
      else
      jjk=Ls(Q)+R
      endif
      do 175 ij=1,Nr
      V1(jki)=V1(jki)-X*V(jjk)
      V1(jjk)=V1(jjk)-X*V(jki)
      jki=jki+Lpair
      jjk=jjk+Lpair
175   continue
      elseif(Sindx.NE.8)then
      
      if(Mindx.EQ.1.OR.Mindx.EQ.3)then
      elseif(Mindx.EQ.2)then
      
      jki=Ls(Q)+P
      do 176 ij=1,Nr
      V1(jki)=V1(jki)+X*V(jki)
      jki=jki+Lpair
176   continue
      else
      goto 180
      endif
      endif
      goto 200
      
180   jli=Ls(R)+P
      jji=Ls(Q)+P
      do 190 ij=1,Nr
      V1(jji)=V1(jji)-X*V(jli)
      V1(jli)=V1(jli)-X*V(jji)
      jji=jji+Lpair
      jli=jli+Lpair
190   continue
      endif
      
200   continue
      Intt=Intt+Kntt2
      endif
      if(iflst.LE.0)goto 100
      call iwind(intape)
      
      return
      
      end
C* :1 * 
      
