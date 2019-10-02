
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 trcl80"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "trcl80.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "trcl80.web"
      subroutine trcl80
      implicit none
      double precision a0,Atmchg,C,Cmo,gfloat,S,T,tenp8,V1,Valint,X,zero
      integer I,ia,iadk,iads,Ian,ib,Ibasd,Ibase,Ibfpad,ibprev,ibuck,ic,I
     &charg,Icon,icont,iconv,Icount,id,Idummy,Idump
      integer Ieval,Ifil,iflag,iflst,Iia,Ij,imdv,IMEMLN,In,indij,indkl,i
     &ndmj,indmk,indml,indmoi,indmoj,indmok,indx,Inforb,intape
      integer Intcnt,Ioab,Iop,iopcl,Iout,Iprint,Ipunch,Iq,iqbufr,iqproc,
     &Ireset,Irwibf,iscr,Ismode,Ispect,Istat,Itotal,Iux,ix,J
      integer Ja,jc,jdif,jq,jump,K,kk,kl,Kntt1,Kntt2,L,Last,leng,lengs,L
     &enibf,Limint,ll,Lnforb,Loab,lq
      integer Lspect,m,m1,MAXBAS,Maxbuc,Maxws,mdim,mdim1,mdv,mdv2,MEMLEN
     &,Mindx,Minws,mjen,mjst,mken,mkhigh,mklow,mkst,mlen
      integer mlst,Mode,moi,moj,moja,mok,moklk,mol,mola,Morews,mstart,Mu
     &ltip,MXBAS1,Nae,Natoms,nb1,Nbasis,Nbe,nbkdat,nbuck
      integer ncount,Ne,nfile,nint,Nj,nmok,nnu,Noa,Noa2,Noa3,Noaob,noap,
     &Noava,Noavb,Nob,Nob2,Nob3,Nobva,Nobvb,Novaa
      integer Novab,Novbb,npass,Nrorb,Nrpext,nspace,NSQMAX,ntt,nttt,Ntx,
     &nucore,nuen,nust,Nva,Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      integer Nwiib,Nwpi
      parameter(MAXBAS=150,MEMLEN=50000,IMEMLN=(2*MEMLEN),MXBAS1=(MAXBAS
     &+1),NSQMAX=(MAXBAS*MAXBAS))
      logical iopt,mokocc
      integer P,Q,R,Sindx
      integer Dbase,Dbasd,Dcount
      integer V
      integer Regws
      dimension X(4760),ix(11264)
      dimension Iia(2)
      dimension lengs(18),nbkdat(6)
      dimension V1(MEMLEN)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/memry/V(IMEMLN)
      common/st802/S(NSQMAX),T(MAXBAS),Ij(MXBAS1),Nj(MXBAS1)
      common/cmo802/Cmo(NSQMAX)
      common/packed/I,J,K,L,Valint,Ja
      common/comorb/Inforb,Lnforb
      common/io/In,Iout,Ipunch
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Ibfpad
      common/locibf/Irwibf,Lenibf
      common/mem802/Minws,Regws,Maxws,Morews
      common/dump/Idump,Idummy
      common/print/Iprint
      equivalence(X(1),ix(1)),(S(1),X(1))
      equivalence(P,I),(Q,J),(R,K),(Sindx,L),(Mindx,K)
      equivalence(Valint,Iia(1))
      equivalence(Icount,Kntt1),(Dcount,Kntt2)
      equivalence(V(1),V1(1))
      data nfile/0/
      data iscr/2001/
      data zero,tenp8/0.D0,1.D8/
      mdim=MAXBAS
      mdv=MEMLEN
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(i6)
99002 format(1x,i7,10x,4I4,2x,d15.8)
99003 format(1x,i7,' A.O.-INTEGRALS PROCESSED')
99004 format(/' DIMENSION EXCEEDS LIMIT'/)
99005 format(' ',30x,i7,' M.O.-INTEGRALS CREATED')
99006 format(' RHF INTEGRAL TRANSFORMATION:')
99007 format(' PROCESSED THE INTEGRAL FILE ',i2,' TIMES FOR EACH MOI')
99008 format(/' (AB/CD) TRANSFORMED INTEGRALS ARE NOT AVAILABLE'/)
      
      
      nspace=Maxws
      if(Nbasis.GT.50)nspace=Morews
      
      iopcl=Iop(5)-1
      nbuck=Iop(6)+1
      icont=Iop(7)
      imdv=Iop(8)
      Iprint=Iop(33)
      Idump=Iop(34)
      
      iopt=.FALSE.
      if(Iprint.GE.2)iopt=.TRUE.
      
      if(iopcl.LT.0)call ilsw(2,1,iopcl)
      if(iopcl.NE.0)goto 1200
      
      iconv=0
      if(icont.EQ.0)call ilsw(2,5,iconv)
      if(iconv.NE.0)call lnk1e
      
      write(Iout,99006)
      if(Nbasis.GT.mdim)then
      write(Iout,99004)
      call lnk1e
      endif
      
      if(imdv.NE.0)read(In,99001)mdv
      mdv2=mdv/2
      
      if(nbuck.GT.5)write(Iout,99008)
      if(nbuck.GT.5)call lnk1e
      
      call tread(Inforb,Ispect,Lnforb,1,Lnforb,1,0)
      
      call tread(Ispect,Cmo,Lspect,1,Lspect,1,0)
      
      nint=0
      
      mdim1=mdim+1
      nb1=Nbasis+1
      ntt=(Nbasis*(Nbasis+1))/2
      nttt=Nbasis*ntt
      noap=Noa+1
      
      call tread(Irwibf,Ismode,Lenibf,1,Lenibf,1,0)
      do 100 I=1,18
      lengs(I)=0
100   continue
      lengs(1)=Noa2*Nva2
      lengs(4)=Noa2*(Noa2+1)/2
      lengs(5)=Noava*(Noava+1)/2
      lengs(11)=Noava*Noa2
      lengs(15)=Noava*Nva2
      if(nbuck.EQ.2)then
      elseif(nbuck.EQ.3)then
      goto 200
      elseif(nbuck.EQ.4)then
      lengs(15)=0
      goto 300
      elseif(nbuck.EQ.5)then
      goto 300
      else
      
      lengs(1)=0
      endif
      lengs(4)=0
200   lengs(11)=0
      lengs(15)=0
      
300   do 400 I=1,18
      if(lengs(I).GT.0)call defbuc((I+50),lengs(I))
400   continue
      call defbuc(iscr,nttt)
      
      do 500 I=1,nb1
      Ij(I)=(I*(I-1))/2
      Nj(I)=ntt*(I-1)
500   continue
      nnu=mdv/ntt
      nmok=mdv2/ntt
      if(nnu.LE.0)call lnk1e
      
      
      
      do 1100 moi=1,Noa
      
      
      iflag=1
      
      mkst=moi
      if(nbuck.EQ.1)mkst=noap
      
      mken=Nrorb
      indmoi=(moi-1)*Nbasis
      indmok=(mkst-1)*Nbasis
      if(mken.LT.mkst)goto 1100
      
      nust=0
      nuen=0
      npass=0
550   nust=nuen+1
      nuen=nust+nnu-1
      nuen=min0(nuen,Nbasis)
      npass=npass+1
      nucore=(nuen-nust+1)*ntt
      do 600 J=1,nucore
      V(J)=0
600   continue
      
      do 650 J=1,nb1
      Nj(J)=-1
650   continue
      do 700 J=nust,nuen
      Nj(J)=(J-nust)*ntt
700   continue
      
      do 750 I=1,Lspect
      Cmo(I)=Cmo(I)*tenp8
750   continue
      
      iqbufr=1
      iqproc=2
      Intcnt=0
      Ntx=1
      intape=Iux(2)
      call iwind(intape)
      call iread(intape,iqbufr,X)
      Ifil=1
800   call iwait(intape)
      iqbufr=iabs(iqbufr-2)+1
      iqproc=iabs(iqproc-2)+1
      Ibase=Ibasd(iqproc)
      Dbase=Dbasd(iqproc)
      call labscf(ix(Ibase),iflst)
      if(iflst.EQ.0)then
      if(Ifil.EQ.(nfile+Ntx*Icon))then
      call iwind(intape)
      Ntx=Ntx+1
      intape=Iux(Ntx+1)
      call iwind(intape)
      endif
      call iread(intape,iqbufr,X)
      Ifil=Ifil+1
      endif
      
      
      if(Mode.NE.1)call lnk1e
      if(Kntt1.GT.0)then
      jq=Ireset(1)+Ibase
      lq=jq+(Kntt1-1)*Nwpi
      do 820 m=jq,lq,Nwpi
      Ja=ix(m)
      Iia(1)=ix(m+1)
      Iia(2)=ix(m+2)
      call unpck4
      indij=Ij(I)+J
      indkl=Ij(K)+L
      if(Nj(J).GE.0)then
      ia=Nj(J)+indkl
      V(ia)=V(ia)+Cmo(I+indmoi)*Valint
      endif
      if(Nj(I).GE.0)then
      ib=Nj(I)+indkl
      V(ib)=V(ib)+Cmo(J+indmoi)*Valint
      endif
      if(Nj(L).GE.0)then
      ic=Nj(L)+indij
      V(ic)=V(ic)+Cmo(K+indmoi)*Valint
      endif
      if(Nj(K).GE.0)then
      id=Nj(K)+indij
      V(id)=V(id)+Cmo(L+indmoi)*Valint
      endif
820   continue
      Intcnt=Intcnt+Kntt1
      endif
      
      if(Kntt2.GT.0)then
      lq=Ireset(2)+Ibase
      jq=lq-(Kntt2-1)*Nwpi
      do 840 m=jq,lq,Nwpi
      Ja=ix(m)
      Iia(1)=ix(m+1)
      Iia(2)=ix(m+2)
      call unpck4
      Sindx=Sindx+1
      if(Sindx.EQ.1)goto 830
      if(Sindx.EQ.2)then
      
      indij=Ij(P)+Q
      indkl=Ij(Q)+R
      if(Nj(Q).GE.0)then
      ia=Nj(Q)+indkl
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      id=Nj(Q)+indij
      V(id)=V(id)+Cmo(R+indmoi)*Valint
      endif
      if(Nj(P).GE.0)then
      ib=Nj(P)+indkl
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      endif
      if(Nj(R).GE.0)then
      ic=Nj(R)+indij
      V(ic)=V(ic)+Cmo(Q+indmoi)*Valint
      endif
      elseif(Sindx.EQ.3)then
      
      indij=Ij(P)+Q
      indkl=Ij(R)+Q
      if(Nj(Q).GE.0)then
      ia=Nj(Q)+indkl
      ic=Nj(Q)+indij
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      V(ic)=V(ic)+Cmo(R+indmoi)*Valint
      endif
      if(Nj(P).GE.0)then
      ib=Nj(P)+indkl
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      endif
      if(Nj(R).GE.0)then
      id=Nj(R)+indij
      V(id)=V(id)+Cmo(Q+indmoi)*Valint
      endif
      elseif(Sindx.EQ.4)then
      
      indij=Ij(P)+Q
      if(Nj(Q).GE.0)then
      ia=Nj(Q)+indij
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      endif
      if(Nj(P).GE.0)then
      ib=Nj(P)+indij
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      endif
      elseif(Sindx.EQ.5)then
      
      if(Nj(P).GE.0)then
      ia=Nj(P)+Ij(Q)+R
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      endif
      if(Nj(R).GE.0)then
      ib=Nj(R)+Ij(P+1)
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      endif
      if(Nj(Q).GE.0)then
      ic=Nj(Q)+Ij(P+1)
      V(ic)=V(ic)+Cmo(R+indmoi)*Valint
      endif
      elseif(Sindx.EQ.6)then
      
      if(Nj(Q).GE.0)then
      ia=Nj(Q)+Ij(R+1)
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      endif
      if(Nj(P).GE.0)then
      ib=Nj(P)+Ij(R+1)
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      endif
      if(Nj(R).GE.0)then
      ic=Nj(R)+Ij(P)+Q
      V(ic)=V(ic)+Cmo(R+indmoi)*Valint
      endif
      elseif(Sindx.EQ.8)then
      
      if(Nj(P).GE.0)then
      ia=Nj(P)+Ij(P+1)
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      endif
      
      elseif(Mindx.EQ.1)then
      
      if(Nj(Q).GE.0)then
      ia=Nj(Q)+Ij(Q+1)
      ic=Nj(Q)+Ij(P)+Q
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      V(ic)=V(ic)+Cmo(Q+indmoi)*Valint
      endif
      if(Nj(P).GE.0)then
      ib=Nj(P)+Ij(Q+1)
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      endif
      elseif(Mindx.EQ.2)then
      
      if(Nj(P).GE.0)then
      ia=Nj(P)+Ij(Q+1)
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      endif
      if(Nj(Q).GE.0)then
      ib=Nj(Q)+Ij(P+1)
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      endif
      elseif(Mindx.EQ.3)then
      
      if(Nj(P).GE.0)then
      ia=Nj(P)+Ij(P)+Q
      ic=Nj(P)+Ij(P+1)
      V(ic)=V(ic)+Cmo(Q+indmoi)*Valint
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      endif
      if(Nj(Q).GE.0)then
      ib=Nj(Q)+Ij(P+1)
      V(ib)=V(ib)+Cmo(P+indmoi)*Valint
      endif
      else
      goto 830
      endif
      goto 840
      
830   indij=Ij(P)+Q
      indkl=Ij(P)+R
      if(Nj(Q).GE.0)then
      ia=Nj(Q)+indkl
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      endif
      if(Nj(P).GE.0)then
      ib=Nj(P)+indkl
      id=Nj(P)+indij
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      V(id)=V(id)+Cmo(R+indmoi)*Valint
      endif
      if(Nj(R).GE.0)then
      ic=Nj(R)+indij
      V(ic)=V(ic)+Cmo(P+indmoi)*Valint
      endif
      
840   continue
      Intcnt=Intcnt+Kntt2
      endif
      if(iflst.LE.0)goto 800
      call iwind(intape)
      
      if(moi.LE.1)then
      if(Iprint.GT.0)write(Iout,99003)Intcnt
      endif
      if(Intcnt.NE.Itotal)call lnk1e
      
      do 850 I=1,Lspect
      Cmo(I)=Cmo(I)/tenp8
850   continue
      
      
      call fileio(2,-iscr,0,0,0)
      call fileio(1,-iscr,0,0,0)
      indmk=indmok
      do 900 mok=mkst,mken
      if(iflag.EQ.2)then
      
      leng=ntt
      call fileio(2,iscr,leng,S,0)
      else
      
      do 860 I=1,ntt
      S(I)=zero
860   continue
      endif
      do 880 kl=1,ntt
      a0=zero
      jc=kl
      do 870 J=nust,nuen
      a0=a0+Cmo(indmk+J)*gfloat(V(jc))
      jc=jc+ntt
870   continue
      S(kl)=S(kl)+a0
880   continue
      leng=ntt
      call fileio(1,iscr,leng,S,0)
      indmk=indmk+Nbasis
900   continue
      iflag=2
      if(nuen.LT.Nbasis)goto 550
      if(Iprint.GT.0.AND.moi.EQ.1)write(Iout,99007)npass
      
      
      mola=0
      iadk=0
      mstart=1
      mjst=moi
      indmok=Nbasis*(mkst-1)
      indmoj=Nbasis*(mjst-1)
      
      indx=0
      ncount=0
      call fileio(2,-iscr,0,0,0)
      mklow=mkst-1
      mkhigh=mkst-1
      do 1000 mok=mkst,mken
      if(mok.LT.mklow.OR.mok.GT.mkhigh)then
      mklow=mkhigh+1
      mkhigh=mklow+nmok-1
      mkhigh=min0(mkhigh,mken)
      if(ncount.NE.0)then
      leng=mola
      call fileio(1,(ibuck+50),leng,V1,0)
      mola=0
      iadk=0
      endif
      leng=(mkhigh-mklow+1)*ntt
      call fileio(2,iscr,leng,V1,0)
      ncount=1
      endif
      if(mok.GT.Noa)then
      
      mokocc=.FALSE.
      ibuck=5
      mjen=Noa
      if(nbuck.EQ.5)mjen=Nrorb
      mlst=noap
      mlen=Nrorb
      else
      mokocc=.TRUE.
      if(nbuck.EQ.2)mjst=noap
      if(nbuck.GT.3)mjst=1
      mjen=Nrorb
      endif
      jdif=mjen-mjst+1
      indmoj=Nbasis*(mjst-1)
      if(jdif.GT.0)then
      
      
      iads=0
      do 920 L=1,Nbasis
      
      do 905 K=1,Nbasis
      ll=min0(K,L)
      kk=max0(K,L)
      moklk=Ij(kk)+ll+iadk
      T(K)=V1(moklk)
905   continue
      indmj=indmoj
      do 910 moj=mjst,mjen
      iads=iads+1
      a0=zero
      do 906 K=1,Nbasis
      a0=a0+Cmo(indmj+K)*T(K)
906   continue
      S(iads)=a0/tenp8
      indmj=indmj+Nbasis
910   continue
920   continue
      
      do 950 moj=mjst,mjen
      moja=moj-mjst+1
      if(mok.LE.Noa.OR.moj.LE.Noa)then
      
      mlst=noap
      if(mok.LE.Noa)then
      mlst=moj
      if(moj.GT.Noa)then
      
      ibuck=1
      mlen=Nrorb
      else
      if(moi.GT.moj)then
      
      ibuck=11
      mlst=noap
      else
      ibuck=4
      endif
      mlen=Noa
      if(nbuck.GT.3)mlen=Nrorb
      endif
      endif
      if(moj.EQ.moi)mlst=mok
      else
      mlst=moj
      mlen=Nrorb
      ibuck=15
      endif
      indml=(mlst-1)*Nbasis
      if(mola.EQ.0)ibprev=ibuck
      if((ibprev.NE.ibuck).AND.(mola.NE.0))goto 930
      goto 935
      
925   ibuck=11
      mlst=noap
930   leng=mola
      call fileio(1,(ibprev+50),leng,V1,0)
      ibprev=ibuck
      mola=0
      
      
935   do 940 mol=mlst,mlen
      mola=mola+1
      m1=moja
      a0=zero
      do 936 L=1,Nbasis
      a0=a0+Cmo(indml+L)*S(m1)
      m1=m1+jdif
936   continue
      indml=indml+Nbasis
      nint=nint+1
      V1(mola)=a0
      if(iopt)write(Iout,99002)ibuck,moi,mok,moj,mol,a0
      if((mok.LE.Noa).AND.(moj.LE.Noa))then
      if((mol.EQ.Noa).AND.(nbuck.GT.3))goto 925
      endif
940   continue
950   continue
      mjst=moi
      endif
      iadk=iadk+ntt
1000  continue
      leng=mola
      if(leng.NE.0)call fileio(1,(ibuck+50),leng,V1,0)
      
1100  continue
      write(Iout,99005)nint
      
1200  if(Idump.GT.1)call fdump
      jump=0
      
      return
      
      end
C* :1 * 
      
