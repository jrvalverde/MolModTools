
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 trclos"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "trclos.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "trclos.web"
      subroutine trclos(JUMP)
      implicit none
      double precision a0,Atmchg,C,Cmo,S,T,V,Valint,X,zero
      integer I,ia,iadk,iads,Ian,ib,Ibasd,Ibase,Ibfpad,ibprev,ibuck,ic,I
     &charg,Icon,icont,iconv,Icount,id,Idummy,Idump
      integer Ieval,iext,Ifil,iflst,Iia,Ij,IMEMLN,In,indij,indkl,indmj,i
     &ndmk,indml,indmoi,indmoj,indmok,indx,Inforb,intape,Intcnt
      integer Ioab,Iop,iopcl,Iout,Iprint,Ipunch,Iq,iqbufr,iqproc,Ireset,
     &Irwibf,Ismode,Ispect,Istat,Itotal,Iux,ix,J,Ja,jc
      integer jdif,jq,JUMP,K,kk,kl,Kntt1,Kntt2,L,Last,leng,lengs,Lenibf,
     &Limint,ll,Lnforb,Loab,lq,Lspect,m
      integer m1,MAXBAS,Maxbuc,Maxws,MDIM,mdim1,MEMLEN,Mindx,Minws,mjen,
     &mjst,mken,mkst,mlen,mlst,Mode,moi,moj,moja,mok
      integer moklk,mol,mola,Morews,mstart,Multip,MXBAS1,Nae,Natoms,Nbas
     &is,Nbe,nbkdat,nbuck,Ne,nfile,nint,Nj,Noa,Noa2,Noa3
      integer Noaob,noap,Noava,Noavb,Nob,Nob2,Nob3,Nobva,Nobvb,Novaa,Nov
     &ab,Novbb,Nrorb,Nrpext,nspace,NSQMAX,ntt,nttt,Ntx,Nva
      integer Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3,Nwiib,Nwpi
      parameter(MDIM=36)
      parameter(MAXBAS=150,MEMLEN=50000,IMEMLN=(2*MEMLEN),MXBAS1=(MAXBAS
     &+1),NSQMAX=(MAXBAS*MAXBAS))
      logical iopt,mokocc
      integer P,Q,R,Sindx
      integer Dbase,Dbasd,Dcount
      integer Regws
      dimension X(4760),ix(11264)
      dimension Iia(2)
      dimension lengs(18),nbkdat(6)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/memry/V(50000)
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
      data nfile/0/
      data zero/0.D0/
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(1x,i7,10x,4I4,d15.8)
99002 format(1x,i7,' A.O.-INTEGRALS PROCESSED')
99003 format(/' DIMENSION EXCEEDS LIMIT'/)
99004 format(' ',30x,i7,' M.O.-INTEGRALS CREATED')
99005 format(' RHF INTEGRAL TRANSFORMATION:')
99006 format(/' (AB/CD) TRANSFORMED INTEGRALS ARE NOT AVAILABLE'/)
      
      
      
      iext=Iop(9)
      if((iext.EQ.0).AND.(Nbasis.LE.MDIM))then
      
      
      nspace=Regws
      if(Nbasis.LE.25)nspace=Minws
      if(Nbasis.GT.31)nspace=Maxws
      
      iopcl=Iop(5)-1
      nbuck=Iop(6)+1
      icont=Iop(7)
      Iprint=Iop(33)
      Idump=Iop(34)
      
      iopt=.FALSE.
      if(Iprint.GE.2)iopt=.TRUE.
      
      if(iopcl.LT.0)call ilsw(2,1,iopcl)
      if(iopcl.NE.0)goto 500
      
      iconv=0
      if(icont.EQ.0)call ilsw(2,5,iconv)
      if(iconv.NE.0)call lnk1e
      
      write(Iout,99005)
      if(Nbasis.GT.MDIM)then
      write(Iout,99003)
      call lnk1e
      endif
      
      if(nbuck.GT.5)write(Iout,99006)
      if(nbuck.GT.5)call lnk1e
      
      call tread(Inforb,Ispect,Lnforb,1,Lnforb,1,0)
      
      call tread(Ispect,Cmo,Lspect,1,Lspect,1,0)
      
      nint=0
      
      mdim1=MDIM+1
      ntt=(Nbasis*(Nbasis+1))/2
      nttt=Nbasis*ntt
      noap=Noa+1
      
      call tread(Irwibf,Ismode,Lenibf,1,Lenibf,1,0)
      do 50 I=1,18
      lengs(I)=0
50    continue
      lengs(1)=Noa2*Nva2
      lengs(4)=Noa2*(Noa2+1)/2
      lengs(5)=Noava*(Noava+1)/2
      lengs(11)=Noava*Noa2
      lengs(15)=Noava*Nva2
      if(nbuck.EQ.2)then
      elseif(nbuck.EQ.3)then
      goto 100
      elseif(nbuck.EQ.4)then
      lengs(15)=0
      goto 150
      elseif(nbuck.EQ.5)then
      goto 150
      else
      
      lengs(1)=0
      endif
      lengs(4)=0
100   lengs(11)=0
      lengs(15)=0
      
150   do 200 I=1,18
      if(lengs(I).GT.0)call defbuc((I+50),lengs(I))
200   continue
      
      do 250 I=1,mdim1
      Ij(I)=(I*(I-1))/2
      Nj(I)=ntt*(I-1)
250   continue
      
      
      
      do 400 moi=1,Noa
      
      
      mkst=moi
      if(nbuck.EQ.1)mkst=noap
      
      mken=Nrorb
      indmoi=(moi-1)*Nbasis
      indmok=(mkst-1)*Nbasis
      if(mken.LT.mkst)goto 400
      
      do 260 J=1,nttt
      V(J)=zero
260   continue
      iqbufr=1
      iqproc=2
      Intcnt=0
      
      Ntx=1
      intape=Iux(2)
      call iwind(intape)
      call iread(intape,iqbufr,X)
      Ifil=1
280   call iwait(intape)
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
      do 290 m=jq,lq,Nwpi
      Ja=ix(m)
      Iia(1)=ix(m+1)
      Iia(2)=ix(m+2)
      call unpck4
      indij=Ij(I)+J
      indkl=Ij(K)+L
      ia=Nj(J)+indkl
      ib=Nj(I)+indkl
      ic=Nj(L)+indij
      id=Nj(K)+indij
      V(ia)=V(ia)+Cmo(I+indmoi)*Valint
      V(ib)=V(ib)+Cmo(J+indmoi)*Valint
      V(ic)=V(ic)+Cmo(K+indmoi)*Valint
      V(id)=V(id)+Cmo(L+indmoi)*Valint
290   continue
      Intcnt=Intcnt+Kntt1
      endif
      
      if(Kntt2.GT.0)then
      lq=Ireset(2)+Ibase
      jq=lq-(Kntt2-1)*Nwpi
      do 300 m=jq,lq,Nwpi
      Ja=ix(m)
      Iia(1)=ix(m+1)
      Iia(2)=ix(m+2)
      call unpck4
      Sindx=Sindx+1
      if(Sindx.EQ.1)goto 295
      if(Sindx.EQ.2)then
      
      indij=Ij(P)+Q
      indkl=Ij(Q)+R
      ia=Nj(Q)+indkl
      ib=Nj(P)+indkl
      ic=Nj(R)+indij
      id=Nj(Q)+indij
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      V(ic)=V(ic)+Cmo(Q+indmoi)*Valint
      V(id)=V(id)+Cmo(R+indmoi)*Valint
      elseif(Sindx.EQ.3)then
      
      indij=Ij(P)+Q
      indkl=Ij(R)+Q
      ia=Nj(Q)+indkl
      ib=Nj(P)+indkl
      ic=Nj(Q)+indij
      id=Nj(R)+indij
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      V(ic)=V(ic)+Cmo(R+indmoi)*Valint
      V(id)=V(id)+Cmo(Q+indmoi)*Valint
      elseif(Sindx.EQ.4)then
      
      indij=Ij(P)+Q
      ia=Nj(Q)+indij
      ib=Nj(P)+indij
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      elseif(Sindx.EQ.5)then
      
      ia=Nj(P)+Ij(Q)+R
      ib=Nj(R)+Ij(P+1)
      ic=Nj(Q)+Ij(P+1)
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      V(ic)=V(ic)+Cmo(R+indmoi)*Valint
      elseif(Sindx.EQ.6)then
      
      ia=Nj(Q)+Ij(R+1)
      ib=Nj(P)+Ij(R+1)
      ic=Nj(R)+Ij(P)+Q
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      V(ic)=V(ic)+Cmo(R+indmoi)*Valint
      elseif(Sindx.EQ.8)then
      
      ia=Nj(P)+Ij(P+1)
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      
      elseif(Mindx.EQ.1)then
      
      ia=Nj(Q)+Ij(Q+1)
      ib=Nj(P)+Ij(Q+1)
      ic=Nj(Q)+Ij(P)+Q
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      V(ic)=V(ic)+Cmo(Q+indmoi)*Valint
      elseif(Mindx.EQ.2)then
      
      ia=Nj(P)+Ij(Q+1)
      ib=Nj(Q)+Ij(P+1)
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      elseif(Mindx.EQ.3)then
      
      ia=Nj(P)+Ij(P)+Q
      ib=Nj(Q)+Ij(P+1)
      ic=Nj(P)+Ij(P+1)
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      V(ib)=V(ib)+Cmo(P+indmoi)*Valint
      V(ic)=V(ic)+Cmo(Q+indmoi)*Valint
      else
      goto 295
      endif
      goto 300
      
295   indij=Ij(P)+Q
      indkl=Ij(P)+R
      ia=Nj(Q)+indkl
      ib=Nj(P)+indkl
      ic=Nj(R)+indij
      id=Nj(P)+indij
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      V(ic)=V(ic)+Cmo(P+indmoi)*Valint
      V(id)=V(id)+Cmo(R+indmoi)*Valint
      
300   continue
      Intcnt=Intcnt+Kntt2
      endif
      if(iflst.LE.0)goto 280
      call iwind(intape)
      
      if(moi.LE.1)then
      if(Iprint.GT.0)write(Iout,99002)Intcnt
      endif
      if(Intcnt.NE.Itotal)call lnk1e
      
      
      do 340 kl=1,ntt
      indmk=indmok
      do 310 mok=mkst,mken
      a0=zero
      jc=kl
      do 305 J=1,Nbasis
      a0=a0+Cmo(indmk+J)*V(jc)
      jc=jc+ntt
305   continue
      indmk=indmk+Nbasis
      T(mok)=a0
310   continue
      jc=kl
      do 320 mok=mkst,mken
      V(jc)=T(mok)
      jc=jc+ntt
320   continue
340   continue
      
      
      mola=0
      iadk=0
      mstart=1
      mjst=moi
      indmok=Nbasis*(mkst-1)
      indmoj=Nbasis*(mjst-1)
      
      indx=0
      do 380 mok=mkst,mken
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
      do 350 L=1,Nbasis
      
      do 342 K=1,Nbasis
      ll=min0(K,L)
      kk=max0(K,L)
      moklk=Ij(kk)+ll+iadk
      T(K)=V(moklk)
342   continue
      indmj=indmoj
      do 346 moj=mjst,mjen
      iads=iads+1
      a0=zero
      do 344 K=1,Nbasis
      a0=a0+Cmo(indmj+K)*T(K)
344   continue
      S(iads)=a0
      indmj=indmj+Nbasis
346   continue
350   continue
      
      do 365 moj=mjst,mjen
      moja=moj-mjst+1
      if(mok.LE.Noa.OR.moj.LE.Noa)then
      
      mlst=noap
      if(mok.GT.Noa)ibuck=5
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
      if((ibprev.NE.ibuck).AND.(mola.NE.0))goto 354
      goto 356
      
352   ibuck=11
      mlst=noap
354   leng=mola-mstart+1
      call fileio(1,(ibprev+50),leng,V(mstart),0)
      ibprev=ibuck
      mstart=mola+1
      
      
356   do 360 mol=mlst,mlen
      mola=mola+1
      m1=moja
      a0=zero
      do 358 L=1,Nbasis
      a0=a0+Cmo(indml+L)*S(m1)
      m1=m1+jdif
358   continue
      indml=indml+Nbasis
      nint=nint+1
      V(mola)=a0
      if(iopt)write(Iout,99001)ibuck,moi,mok,moj,mol,a0
      if((mok.LE.Noa).AND.(moj.LE.Noa))then
      if((mol.EQ.Noa).AND.(nbuck.GT.3))goto 352
      endif
360   continue
365   continue
      mjst=moi
      endif
      iadk=iadk+ntt
380   continue
      leng=mola-mstart+1
      if(leng.NE.0)call fileio(1,(ibuck+50),leng,V(mstart),0)
      
400   continue
      write(Iout,99004)nint
      else
      call trcl80
      return
      endif
      
500   if(Idump.GT.0)call fdump
      JUMP=0
      
      return
      
      end
C* :1 * 
      
