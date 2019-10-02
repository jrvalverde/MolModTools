
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tropen"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tropen.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "tropen.web"
      subroutine tropen
      implicit none
      double precision a0,Atmchg,C,Cmo,S,T,V,Valint,X,zero
      integer I,ia,iadk,iads,Ian,ib,ib0,ibadr,Ibasd,Ibase,Ibfpad,ibprev,
     &ibuck,ic,Icharg,Icon,icont,iconv,Icount,id
      integer Idummy,Idump,Ieval,iext,Ifil,iflst,Iia,Ij,Ijls,In,indij,in
     &dkl,indmj,indmk,indml,indmoi,indmoj,indmok,indx,Inforb
      integer intape,Intcnt,Ioab,Iop,iopcl,Iout,ip1,ipass,iprint,Ipunch,
     &Iq,iqbufr,iqproc,Iregws,Ireset,Irwibf,isl,Ismode,Ispect,isr
      integer Istat,Itotal,Iux,ix,J,Ja,jc,jdif,jq,K,kk,kl,Kntt1,Kntt2,L,
     &Last,leng,lengs,Lenibf,Limint
      integer ll,Lnforb,Loab,lq,Lspect,m,m1,Maxbuc,Maxws,mdim,mdim1,Mind
     &x,Minws,mjen,mjsave,mjst,mken,mkst,mlen,mlst
      integer Mode,moi,moj,moja,mok,moklk,mol,mola,Morews,mstart,Multip,
     &n2,Nae,Natoms,Nbasis,Nbe,nbkdat,nbuck,Ne,nfile
      integer nint,Nj,nleng,nloop,Noa,Noa2,Noa3,Noaob,noap,Noava,Noavb,N
     &ob,Nob2,Nob3,nobp,Nobva,Nobvb,Novaa,Novab,Novbb
      integer Nrorb,Nrpext,nsbeta,nsl,nspace,nsr,nsrp,ntt,nttt,Ntx,Nva,N
     &va2,Nva3,Nvavb,Nvb,Nvb2,Nvb3,Nwiib,Nwpi
      logical iopt,mokocc
      integer P,Q,R,Sindx
      integer Dbase,Dbasd,Dcount
      dimension X(4760),ix(1)
      dimension Iia(2)
      dimension lengs(18),ibadr(5,4),nbkdat(6)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/memry/V(50000)
      common/st/S(6400),T(80),Ij(81),Nj(81)
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/cmo803/Cmo(12800)
      common/packed/I,J,K,L,Valint,Ja
      common/comorb/Inforb,Lnforb
      common/rwfscr/Ijls
      common/io/In,Iout,Ipunch
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Ibfpad
      common/locibf/Irwibf,Lenibf
      common/mem803/Minws,Iregws,Maxws,Morews
      common/dump/Idump,Idummy
      equivalence(X(1),ix(1)),(S(1),X(1))
      equivalence(P,I),(Q,J),(R,K),(Sindx,L),(Mindx,K)
      equivalence(Valint,Iia(1))
      equivalence(Icount,Kntt1),(Dcount,Kntt2)
      data mdim/36/
      data nfile/0/
      data zero/0.D0/
      data ibadr/1,4,5,11,15,3,9,10,14,18,7,6,2,12,16,8,0,0,13,17/
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(1x,3I7,5x,4I4,5x,d15.8)
99002 format(1x,i7,' A.O.-INTEGRALS PROCESSED')
99003 format(/' DIMENSION EXCEEDS LIMIT'/)
99004 format(' ',30x,i7,' M.O.-INTEGRALS CREATED')
99005 format(' UHF INTEGRAL TRANSFORMATION:')
99006 format(/' (AB/CD) TRANSFORMED INTEGRALS ARE NOT AVAILABLE'/)
      
      
      
      iext=Iop(9)
      if(iext.GT.0)goto 1000
      if(Nbasis.GT.mdim)goto 1000
      
      nspace=Iregws
      if(Nbasis.LE.25)nspace=Minws
      if(Nbasis.GT.30)nspace=Maxws
      
      iopcl=Iop(5)-1
      nbuck=Iop(6)+1
      icont=Iop(7)
      iprint=Iop(33)
      Idump=Iop(34)
      
      iopt=.FALSE.
      if(iprint.GE.2)iopt=.TRUE.
      
      if(iopcl.LT.0)call ilsw(2,1,iopcl)
      if(iopcl.NE.1)goto 900
      
      iconv=0
      if(icont.EQ.0)call ilsw(2,5,iconv)
      if(iconv.NE.0)call lnk1e
      
      write(Iout,99005)
      if(Nbasis.GT.mdim)then
      write(Iout,99003)
      call lnk1e
      endif
      
      if(nbuck.GT.5)write(Iout,99006)
      if(nbuck.GT.5)call lnk1e
      
      call tread(Inforb,Ispect,Lnforb,1,Lnforb,1,0)
      
      call tread(Ispect,Cmo,Lspect,1,Lspect,1,0)
      
      nint=0
      
      mdim1=mdim+1
      ntt=(Nbasis*(Nbasis+1))/2
      nttt=Nbasis*ntt
      nsbeta=Nrorb*Nbasis
      noap=Noa+1
      nobp=Nob+1
      
      call tread(Irwibf,Ismode,Lenibf,1,Lenibf,1,0)
      
      lengs(1)=Noa2*Nva2
      lengs(2)=Novab
      lengs(3)=Nob2*Nvb2
      lengs(4)=Noa2*(Noa2+1)/2
      lengs(5)=Noava*(Noava+1)/2
      lengs(6)=Noa2*Nob2
      lengs(7)=Noa2*Nvb2
      lengs(8)=Nob2*Nva2
      lengs(9)=Nob2*(Nob2+1)/2
      lengs(10)=Nobvb*(Nobvb+1)/2
      lengs(11)=Noa2*Noava
      lengs(12)=Noa2*Nobvb
      lengs(13)=Nob2*Noava
      lengs(14)=Nob2*Nobvb
      lengs(15)=Noava*Nva2
      lengs(16)=Noava*Nvb2
      lengs(17)=Nobvb*Nva2
      lengs(18)=Nobvb*Nvb2
      if(nbuck.EQ.2)then
      elseif(nbuck.EQ.3)then
      goto 100
      elseif(nbuck.EQ.4)then
      goto 200
      elseif(nbuck.EQ.5)then
      goto 300
      else
      
      lengs(1)=0
      lengs(3)=0
      lengs(7)=0
      lengs(8)=0
      endif
      lengs(4)=0
      lengs(6)=0
      lengs(9)=0
100   lengs(11)=0
      lengs(12)=0
      lengs(13)=0
      lengs(14)=0
200   lengs(15)=0
      lengs(16)=0
      lengs(17)=0
      lengs(18)=0
      
300   do 400 I=1,18
      if(lengs(I).GT.0)call defbuc((I+50),lengs(I))
400   continue
      
      do 500 I=1,mdim1
      Ij(I)=(I*(I-1))/2
      Nj(I)=ntt*(I-1)
500   continue
      
      
      
      do 800 moi=1,Noa
      
      do 700 ipass=1,2
      
      
      nloop=0
      mkst=moi
      
      if(ipass.EQ.2)then
      
      if(moi.GT.Nob.OR.moi.LT.1)goto 800
      if(nbuck.EQ.1)mkst=nobp
      indmoi=nsbeta+(moi-1)*Nbasis
      indmok=nsbeta+(mkst-1)*Nbasis
      nleng=ntt*(nobp-moi)
      if(nbuck.EQ.5)nleng=ntt*(Nrorb-moi+1)
      else
      
      if(noap.GT.Nrorb.OR.moi.LT.1)goto 700
      if(nbuck.EQ.1)mkst=noap
      indmoi=(moi-1)*Nbasis
      indmok=(mkst-1)*Nbasis
      nleng=ntt*(Nrorb-moi+1)
      endif
      mken=Nrorb
      if(mken.LT.mkst)goto 700
      
      do 520 J=1,nttt
      V(J)=zero
520   continue
      iqbufr=1
      iqproc=2
      Intcnt=0
      
      Ntx=1
      intape=Iux(2)
      call iwind(intape)
      call iread(intape,iqbufr,X)
      Ifil=1
540   call iwait(intape)
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
      do 550 m=jq,lq,Nwpi
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
550   continue
      Intcnt=Intcnt+Kntt1
      endif
      
      if(Kntt2.GT.0)then
      lq=Ireset(2)+Ibase
      jq=lq-(Kntt2-1)*Nwpi
      do 560 m=jq,lq,Nwpi
      Ja=ix(m)
      Iia(1)=ix(m+1)
      Iia(2)=ix(m+2)
      call unpck4
      Sindx=Sindx+1
      if(Sindx.EQ.1)goto 555
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
      goto 555
      endif
      goto 560
      
555   indij=Ij(P)+Q
      indkl=Ij(P)+R
      ia=Nj(Q)+indkl
      ib=Nj(P)+indkl
      ic=Nj(R)+indij
      id=Nj(P)+indij
      V(ia)=V(ia)+Cmo(P+indmoi)*Valint
      V(ib)=V(ib)+Cmo(Q+indmoi)*Valint
      V(ic)=V(ic)+Cmo(P+indmoi)*Valint
      V(id)=V(id)+Cmo(R+indmoi)*Valint
      
560   continue
      Intcnt=Intcnt+Kntt2
      endif
      if(iflst.LE.0)goto 540
      call iwind(intape)
      
      if(moi.LE.1)then
      if(iprint.GT.0)write(Iout,99002)Intcnt
      endif
      if(Intcnt.NE.Itotal)call lnk1e
      
      
      do 600 kl=1,ntt
      indmk=indmok
      do 570 mok=mkst,mken
      a0=zero
      jc=kl
      do 565 J=1,Nbasis
      a0=a0+Cmo(indmk+J)*V(jc)
      jc=jc+ntt
565   continue
      indmk=indmk+Nbasis
      T(mok)=a0
570   continue
      jc=kl
      do 580 mok=mkst,mken
      V(jc)=T(mok)
      jc=jc+ntt
580   continue
600   continue
      
      
      call twrite(Ijls,V,nleng,1,nleng,1,0)
      
      
      
      
      
      
      
620   mola=0
      iadk=0
      mstart=1
      n2=nloop+ipass
      if(n2.EQ.2.OR.n2.EQ.4)then
      
      isl=nsbeta
      nsl=Nob
      else
      
      isl=0
      nsl=Noa
      endif
      if(n2.EQ.2.OR.n2.EQ.3)then
      
      isr=nsbeta
      nsr=Nob
      else
      
      isr=0
      nsr=Noa
      endif
      nsrp=nsr+1
      mken=Nrorb
      indmok=isl+Nbasis*(mkst-1)
      mjst=1
      if(n2.LE.2)mjst=moi
      if(n2.EQ.4.AND.nbuck.NE.5)then
      mken=Nob
      mjst=nsrp
      endif
      indmoj=isr+Nbasis*(mjst-1)
      
      mjsave=mjst
      indx=0
      do 660 mok=mkst,mken
      if(mok.GT.nsl)then
      
      mokocc=.FALSE.
      ip1=2
      mjen=nsr
      if(nbuck.EQ.5)mjen=Nrorb
      if(n2.EQ.4.AND.nbuck.EQ.5)mjst=nsrp
      else
      mokocc=.TRUE.
      ip1=1
      if(nbuck.EQ.2)mjst=nsrp
      if(nbuck.GT.3)mjst=1
      mjen=Nrorb
      endif
      jdif=mjen-mjst+1
      indmoj=isr+Nbasis*(mjst-1)
      if(jdif.GT.0)then
      
      
      iads=0
      do 630 L=1,Nbasis
      
      do 622 K=1,Nbasis
      ll=min0(K,L)
      kk=max0(K,L)
      moklk=Ij(kk)+ll+iadk
      T(K)=V(moklk)
622   continue
      indmj=indmoj
      do 626 moj=mjst,mjen
      iads=iads+1
      a0=zero
      do 624 K=1,Nbasis
      a0=a0+Cmo(indmj+K)*T(K)
624   continue
      S(iads)=a0
      indmj=indmj+Nbasis
626   continue
630   continue
      
      do 645 moj=mjst,mjen
      moja=moj-mjst+1
      if(mok.GT.nsl.AND.moj.GT.nsr)then
      mlst=moj
      mlen=Nrorb
      ib0=5
      
      elseif(ip1.EQ.2)then
      
      mlst=nsrp
      mlen=Nrorb
      ib0=3
      else
      
      mlst=moj
      if(moj.GT.nsr)then
      
      ib0=1
      mlen=Nrorb
      else
      if(((moi.GT.moj).AND.(nloop.EQ.0)).OR.(n2.EQ.4))then
      
      mlst=nsrp
      ib0=4
      else
      ib0=2
      endif
      mlen=nsr
      if(nbuck.GT.3)mlen=Nrorb
      endif
      endif
      if(nloop.EQ.0.AND.moj.EQ.moi)mlst=mok
      if(mlen.LT.mlst)goto 645
      indml=isr+(mlst-1)*Nbasis
      ibuck=ibadr(ib0,n2)
      
      if(mola.EQ.0)ibprev=ibuck
      if((ibuck.NE.ibprev).AND.(mola.NE.0))goto 634
      goto 636
      
632   ib0=4
      ibuck=ibadr(ib0,n2)
      mlst=nsrp
634   leng=mola-mstart+1
      call fileio(1,(ibprev+50),leng,V(mstart),0)
      ibprev=ibuck
      mstart=mola+1
      
      
636   do 640 mol=mlst,mlen
      mola=mola+1
      m1=moja
      a0=zero
      do 638 L=1,Nbasis
      a0=a0+Cmo(indml+L)*S(m1)
      m1=m1+jdif
638   continue
      indml=indml+Nbasis
      nint=nint+1
      V(mola)=a0
      if(iopt)write(Iout,99001)ibuck,ipass,nloop,moi,mok,moj,mol,a0
      if((mok.LE.nsl).AND.(moj.LE.nsr))then
      if((mol.EQ.nsr).AND.(nbuck.GT.3))goto 632
      endif
640   continue
645   continue
      mjst=mjsave
      endif
      iadk=iadk+ntt
660   continue
      leng=mola-mstart+1
      if(leng.NE.0)call fileio(1,(ibuck+50),leng,V(mstart),0)
      
      if(nloop.EQ.0)then
      nloop=2
      
      call tread(Ijls,V,nleng,1,nleng,1,0)
      goto 620
      endif
      
700   continue
800   continue
      write(Iout,99004)nint
      
900   if(Idump.GT.1)call fdump
      
      return
      
1000  call trop80
      return
      
      end
C* :1 * 
      
