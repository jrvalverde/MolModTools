
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 wtildb"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "wtildb.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "wtildb.web"
      subroutine wtildb
      implicit none
      double precision a0,Cmo,Cut,E,V,V1,Valint,X
      integer I,Iao,Ibasd,Ibase,Ibfpad,Icon,Icount,Ifil,iflst,Iia,ij,In,
     &intape,Intc,Intcnt,Intt,Iout,Ipunch,Iq,iqbufr
      integer iqproc,Ireset,Ismode,Istat,Itotal,Iux,J,Ja,ji,jii,jij,jik,
     &jil,jj,jji,jjj,jjk,jjl,jk,jki
      integer jkj,jl,jli,jlj,jq,K,Kntt1,Kntt2,L,Last,Limint,Lpair,lq,Ls,
     &m,Mdv,Mindx,Mode,nfile,Nr
      integer Nrpext,Ntx,Nwiib,Nwpi
      
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
      
      call track('WTILDB')
      
      
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
      ji=Ls(I)
      jj=Ls(J)
      jk=Ls(K)
      jl=Ls(L)
      jik=ji+K
      jjl=jj+L
      jil=ji+L
      jjk=jj+K
      jki=jk+I
      jlj=jl+J
      jli=jl+I
      jkj=jk+J
      
      do 110 ij=1,Nr
      V1(jik)=V1(jik)+X*V(jjl)
      V1(jjk)=V1(jjk)+X*V(jil)
      V1(jil)=V1(jil)+X*V(jjk)
      V1(jjl)=V1(jjl)+X*V(jik)
      V1(jki)=V1(jki)+X*V(jlj)
      V1(jli)=V1(jli)+X*V(jkj)
      V1(jkj)=V1(jkj)+X*V(jli)
      V1(jlj)=V1(jlj)+X*V(jki)
      jik=jik+Lpair
      jjl=jjl+Lpair
      jil=jil+Lpair
      jjk=jjk+Lpair
      jki=jki+Lpair
      jlj=jlj+Lpair
      jli=jli+Lpair
      jkj=jkj+Lpair
110   continue
      endif
      
150   continue
      
      Intt=Intt+Kntt1
      endif
      if(Kntt2.GT.0)then
      lq=Ireset(2)+Ibase
      jq=lq-(Kntt2-1)*Nwpi
      
      do 250 m=jq,lq,Nwpi
      Ja=Iao(m)
      Iia(1)=Iao(m+1)
      Iia(2)=Iao(m+2)
      if(dabs(X).GE.Cut)then
      Intc=Intc+1
      call unpck4
      Sindx=Sindx+1
      if(Sindx.EQ.1)goto 200
      if(Sindx.EQ.2)then
      
      ji=Ls(P)
      jj=Ls(Q)
      jl=Ls(R)
      jij=ji+Q
      jjl=jj+R
      jil=ji+R
      jjj=jj+Q
      jji=jj+P
      jlj=jl+Q
      jli=jl+P
      do 155 ij=1,Nr
      V1(jij)=V1(jij)+X*V(jjl)
      V1(jjj)=V1(jjj)+X*(V(jil)+V(jli))
      a0=X*V(jjj)
      V1(jil)=V1(jil)+a0
      V1(jjl)=V1(jjl)+X*V(jij)
      V1(jji)=V1(jji)+X*V(jlj)
      V1(jli)=V1(jli)+a0
      V1(jlj)=V1(jlj)+X*V(jji)
      jij=jij+Lpair
      jjl=jjl+Lpair
      jil=jil+Lpair
      jjj=jjj+Lpair
      jji=jji+Lpair
      jlj=jlj+Lpair
      jli=jli+Lpair
155   continue
      elseif(Sindx.EQ.3)then
      
      ji=Ls(P)
      jj=Ls(Q)
      jk=Ls(R)
      jik=ji+R
      jjj=jj+Q
      jij=ji+Q
      jjk=jj+R
      jki=jk+P
      jji=jj+P
      jkj=jk+Q
      do 160 ij=1,Nr
      a0=X*V(jjj)
      V1(jik)=V1(jik)+a0
      V1(jjk)=V1(jjk)+X*V(jij)
      V1(jij)=V1(jij)+X*V(jjk)
      V1(jjj)=V1(jjj)+X*(V(jik)+V(jki))
      V1(jki)=V1(jki)+a0
      V1(jji)=V1(jji)+X*V(jkj)
      V1(jkj)=V1(jkj)+X*V(jji)
      jik=jik+Lpair
      jjj=jjj+Lpair
      jij=jij+Lpair
      jjk=jjk+Lpair
      jki=jki+Lpair
      jji=jji+Lpair
      jkj=jkj+Lpair
160   continue
      elseif(Sindx.EQ.4)then
      
      ji=Ls(P)
      jj=Ls(Q)
      jii=ji+P
      jjj=jj+Q
      jij=ji+Q
      jji=jj+P
      do 165 ij=1,Nr
      V1(jii)=V1(jii)+X*V(jjj)
      V1(jji)=V1(jji)+X*V(jij)
      V1(jij)=V1(jij)+X*V(jji)
      V1(jjj)=V1(jjj)+X*V(jii)
      jii=jii+Lpair
      jjj=jjj+Lpair
      jij=jij+Lpair
      jji=jji+Lpair
165   continue
      elseif(Sindx.EQ.5)then
      
      ji=Ls(P)
      jik=ji+Q
      jil=ji+R
      jki=Ls(Q)+P
      jli=Ls(R)+P
      do 170 ij=1,Nr
      V1(jik)=V1(jik)+X*V(jil)
      V1(jil)=V1(jil)+X*V(jik)
      V1(jki)=V1(jki)+X*V(jli)
      V1(jli)=V1(jli)+X*V(jki)
      jik=jik+Lpair
      jil=jil+Lpair
      jki=jki+Lpair
      jli=jli+Lpair
170   continue
      elseif(Sindx.EQ.6)then
      
      jk=Ls(R)
      jik=Ls(P)+R
      jjk=Ls(Q)+R
      jki=jk+P
      jkj=jk+Q
      do 175 ij=1,Nr
      V1(jik)=V1(jik)+X*V(jjk)
      V1(jjk)=V1(jjk)+X*V(jik)
      V1(jki)=V1(jki)+X*V(jkj)
      V1(jkj)=V1(jkj)+X*V(jki)
      jik=jik+Lpair
      jjk=jjk+Lpair
      jki=jki+Lpair
      jkj=jkj+Lpair
175   continue
      elseif(Sindx.EQ.8)then
      
      jii=Ls(P)+P
      do 180 ij=1,Nr
      V1(jii)=V1(jii)+X*V(jii)
      jii=jii+Lpair
180   continue
      
      elseif(Mindx.EQ.1)then
      
      jj=Ls(Q)
      jjj=jj+Q
      jij=Ls(P)+Q
      jji=jj+P
      do 185 ij=1,Nr
      a0=X*V(jjj)
      V1(jij)=V1(jij)+a0
      V1(jjj)=V1(jjj)+X*(V(jij)+V(jji))
      V1(jji)=V1(jji)+a0
      jjj=jjj+Lpair
      jij=jij+Lpair
      jji=jji+Lpair
185   continue
      elseif(Mindx.EQ.2)then
      
      jik=Ls(P)+Q
      jki=Ls(Q)+P
      do 190 ij=1,Nr
      V1(jik)=V1(jik)+X*V(jik)
      V1(jki)=V1(jki)+X*V(jki)
      jik=jik+Lpair
      jki=jki+Lpair
190   continue
      elseif(Mindx.EQ.3)then
      
      ji=Ls(P)
      jii=ji+P
      jil=ji+Q
      jli=Ls(Q)+P
      do 195 ij=1,Nr
      V1(jii)=V1(jii)+X*(V(jil)+V(jli))
      a0=X*V(jii)
      V1(jil)=V1(jil)+a0
      V1(jli)=V1(jli)+a0
      jii=jii+Lpair
      jil=jil+Lpair
      jli=jli+Lpair
195   continue
      else
      goto 200
      endif
      goto 250
      
200   ji=Ls(P)
      jj=Ls(Q)
      jl=Ls(R)
      jii=ji+P
      jjl=jj+R
      jlj=jl+Q
      jil=ji+R
      jji=jj+P
      jli=jl+P
      jij=ji+Q
      do 210 ij=1,Nr
      V1(jii)=V1(jii)+X*(V(jjl)+V(jlj))
      V1(jji)=V1(jji)+X*V(jil)
      V1(jil)=V1(jil)+X*V(jji)
      a0=X*V(jii)
      V1(jjl)=V1(jjl)+a0
      V1(jli)=V1(jli)+X*V(jij)
      V1(jij)=V1(jij)+X*V(jli)
      V1(jlj)=V1(jlj)+a0
      jii=jii+Lpair
      jjl=jjl+Lpair
      jlj=jlj+Lpair
      jil=jil+Lpair
      jji=jji+Lpair
      jli=jli+Lpair
      jij=jij+Lpair
210   continue
      endif
      
250   continue
      Intt=Intt+Kntt2
      endif
      if(iflst.LE.0)goto 100
      call iwind(intape)
      
      return
      
      end
C* :1 * 
      
