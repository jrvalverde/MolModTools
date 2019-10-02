
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 trop80"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "trop80.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "trop80.web"
      subroutine trop80
      implicit none
      double precision a0,Atmchg,C,Cmo,gfloat,S,T,tenp8,V1,Valint,X,zero
      integer I,ia,iadk,iads,Ian,ib,ib0,ibadr,Ibasd,Ibase,Ibfpad,ibprev,
     &ibuck,ic,Icharg,Icon,icont,iconv,Icount,id
      integer Idummy,Idump,Ieval,Ifil,iflag,iflst,Iia,Ij,Ijls,imdv,In,in
     &dij,indkl,indmj,indmk,indml,indmoi,indmoj,indmok,indx
      integer Inforb,intape,Intcnt,Ioab,Iop,iopcl,Iout,ip1,ipass,Iprint,
     &Ipunch,Iq,iqbufr,iqproc,Iregws,Ireset,Irwibf,iscr,isl,Ismode
      integer Ispect,isr,Istat,Itotal,Iux,ix,J,Ja,jc,jdif,jq,K,kk,kl,Knt
     &t1,Kntt2,L,Last,leng,lengs
      integer Lenibf,Limint,ll,Lnforb,Loab,lq,Lspect,m,m1,Maxbuc,Maxws,m
     &dim,mdim1,mdv,mdv2,Mindx,Minws,mjen,mjsave,mjst
      integer mken,mkhigh,mklow,mkst,mlen,mlst,Mode,moi,moj,moja,mok,mok
     &lk,mol,mola,Morews,mstart,Multip,n2,Nae,Natoms
      integer nb1,Nbasis,Nbe,nbkdat,nbuck,ncount,Ne,nfile,nint,Nj,nleng,
     &nloop,nmok,nnu,Noa,Noa2,Noa3,Noaob,noap,Noava
      integer Noavb,Nob,Nob2,Nob3,nobp,Nobva,Nobvb,Novaa,Novab,Novbb,npa
     &ss,Nrorb,Nrpext,nsbeta,nsl,nspace,nsr,nsrp,ntt,nttt
      integer Ntx,nucore,nuen,nust,Nva,Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3,Nwi
     &ib,Nwpi
      logical iopt,mokocc
      integer P,Q,R,Sindx
      integer Dbase,Dbasd,Dcount
      integer V
      dimension X(4760),ix(11264)
      dimension Iia(2)
      dimension lengs(18),ibadr(5,4),nbkdat(6)
      dimension V1(23976)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/memry/V(100000)
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
      common/print/Iprint
      equivalence(X(1),ix(1)),(S(1),X(1))
      equivalence(P,I),(Q,J),(R,K),(Sindx,L),(Mindx,K)
      equivalence(Valint,Iia(1))
      equivalence(Icount,Kntt1),(Dcount,Kntt2)
      equivalence(V(1),V1(1))
      data mdim,mdv/80,47952/
      data nfile/0/
      data zero,tenp8/0.D0,1.D8/
      data ibadr/1,4,5,11,15,3,9,10,14,18,7,6,2,12,16,8,0,0,13,17/
      data iscr/2001/
      
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(i6)
99002 format(1x,3I7,5x,4I4,5x,d15.8)
99003 format(1x,i7,' A.O.-INTEGRALS PROCESSED')
99004 format(/' DIMENSION EXCEEDS LIMIT'/)
99005 format(' ',30x,i7,' M.O.-INTEGRALS CREATED')
99006 format(' UHF INTEGRAL TRANSFORMATION:')
99007 format(' PROCESSED THE INTEGRAL FILE ',i2,' TIMES FOR EACH MOI')
99008 format(/' (AB/CD) TRANSFORMED INTEGRALS ARE NOT AVAILABLE'/)
      
      
      nspace=Maxws
      if(Nbasis.GT.45)nspace=Morews
      if(Nbasis.LT.25)nspace=Minws
      
      iopcl=Iop(5)-1
      nbuck=Iop(6)+1
      icont=Iop(7)
      imdv=Iop(8)
      Iprint=Iop(33)
      Idump=Iop(34)
      
      iopt=.FALSE.
      if(Iprint.GE.2)iopt=.TRUE.
      
      if(iopcl.LT.0)call ilsw(2,1,iopcl)
      if(iopcl.NE.1)goto 1000
      
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
      call defbuc(iscr,nttt)
      
      do 500 I=1,nb1
      Ij(I)=(I*(I-1))/2
      Nj(I)=ntt*(I-1)
500   continue
      
      nnu=mdv/ntt
      nmok=mdv2/ntt
      if(nmok.LE.0)call lnk1e
      
      
      do 900 moi=1,Noa
      
      do 800 ipass=1,2
      nloop=0
      mkst=moi
      
      iflag=1
      
      if(ipass.EQ.2)then
      
      if(moi.GT.Nob.OR.moi.LT.1)goto 900
      if(nbuck.EQ.1)mkst=nobp
      indmoi=nsbeta+(moi-1)*Nbasis
      indmok=nsbeta+(mkst-1)*Nbasis
      nleng=ntt*(nobp-moi)
      if(nbuck.EQ.5)nleng=ntt*(Nrorb-moi+1)
      else
      
      if(noap.GT.Nrorb.OR.moi.LT.1)goto 800
      if(nbuck.EQ.1)mkst=noap
      indmoi=(moi-1)*Nbasis
      indmok=(mkst-1)*Nbasis
      nleng=ntt*(Nrorb-moi+1)
      endif
      mken=Nrorb
      if(mken.LT.mkst)goto 800
      
      nust=0
      nuen=0
      npass=0
520   nust=nuen+1
      nuen=nuen+nnu
      nuen=min0(nuen,Nbasis)
      npass=npass+1
      nucore=(nuen-nust+1)*ntt
      do 540 J=1,nucore
      V(J)=0
540   continue
      do 560 J=1,nb1
      Nj(J)=-1
560   continue
      do 580 J=nust,nuen
      Nj(J)=(J-nust)*ntt
580   continue
      
      do 600 I=1,Lspect
      Cmo(I)=Cmo(I)*tenp8
600   continue
      
      iqbufr=1
      iqproc=2
      Intcnt=0
      
      Ntx=1
      intape=Iux(2)
      call iwind(intape)
      call iread(intape,iqbufr,X)
      Ifil=1
620   call iwait(intape)
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
      do 630 m=jq,lq,Nwpi
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
630   continue
      Intcnt=Intcnt+Kntt1
      endif
      
      if(Kntt2.GT.0)then
      lq=Ireset(2)+Ibase
      jq=lq-(Kntt2-1)*Nwpi
      do 640 m=jq,lq,Nwpi
      Ja=ix(m)
      Iia(1)=ix(m+1)
      Iia(2)=ix(m+2)
      call unpck4
      Sindx=Sindx+1
      if(Sindx.EQ.1)goto 635
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
      goto 635
      endif
      goto 640
      
635   indij=Ij(P)+Q
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
      
640   continue
      Intcnt=Intcnt+Kntt2
      endif
      if(iflst.LE.0)goto 620
      if(Iprint.GE.3)write(6,99009)
      
99009 format(' FIRST-SUFFIX')
      
      if(Iprint.GE.3)write(6,99010)(V(I),I=1,nucore)
      
99010 format(1x,i12)
      
      call iwind(intape)
      
      if(moi.LE.1)then
      if(Iprint.GT.0)write(Iout,99003)Intcnt
      endif
      if(Intcnt.NE.Itotal)call lnk1e
      
      do 660 I=1,Lspect
      Cmo(I)=Cmo(I)/tenp8
660   continue
      
      
      call fileio(2,-iscr,0,0,0)
      call fileio(1,-iscr,0,0,0)
      indmk=indmok
      do 700 mok=mkst,mken
      if(iflag.EQ.2)then
      
      leng=ntt
      call fileio(2,iscr,leng,S,0)
      else
      
      do 665 I=1,ntt
      S(I)=zero
665   continue
      endif
      do 680 kl=1,ntt
      a0=zero
      jc=kl
      do 670 J=nust,nuen
      a0=a0+Cmo(indmk+J)*gfloat(V(jc))
      jc=jc+ntt
670   continue
      S(kl)=S(kl)+a0
680   continue
      leng=ntt
      call fileio(1,iscr,leng,S,0)
      indmk=indmk+Nbasis
700   continue
      iflag=2
      if(nuen.LT.Nbasis)goto 520
      if(Iprint.GT.0.AND.moi.EQ.1.AND.ipass.EQ.1)write(Iout,99007)npass
      
      
      
      
      
      
      
720   mola=0
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
      ncount=0
      call fileio(2,-iscr,0,0,0)
      mklow=mkst-1
      mkhigh=mkst-1
      do 760 mok=mkst,mken
      if(mok.LT.mklow.OR.mok.GT.mkhigh)then
      mklow=mkhigh+1
      mkhigh=mkhigh+nmok
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
      do 730 L=1,Nbasis
      
      do 722 K=1,Nbasis
      ll=min0(K,L)
      kk=max0(K,L)
      moklk=Ij(kk)+ll+iadk
      T(K)=V1(moklk)
722   continue
      indmj=indmoj
      do 726 moj=mjst,mjen
      iads=iads+1
      a0=zero
      do 724 K=1,Nbasis
      a0=a0+Cmo(indmj+K)*T(K)
724   continue
      S(iads)=a0/tenp8
      indmj=indmj+Nbasis
726   continue
730   continue
      
      do 745 moj=mjst,mjen
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
      if(mlen.LT.mlst)goto 745
      indml=isr+(mlst-1)*Nbasis
      ibuck=ibadr(ib0,n2)
      
      if(mola.EQ.0)ibprev=ibuck
      if((ibuck.NE.ibprev).AND.(mola.NE.0))goto 734
      goto 736
      
732   ib0=4
      ibuck=ibadr(ib0,n2)
      mlst=nsrp
734   leng=mola
      call fileio(1,(ibprev+50),leng,V1,0)
      ibprev=ibuck
      mola=0
      
      
736   do 740 mol=mlst,mlen
      mola=mola+1
      m1=moja
      a0=zero
      do 738 L=1,Nbasis
      a0=a0+Cmo(indml+L)*S(m1)
      m1=m1+jdif
738   continue
      indml=indml+Nbasis
      nint=nint+1
      V1(mola)=a0
      if(iopt)write(Iout,99002)ibuck,ipass,nloop,moi,mok,moj,mol,a0
      if((mok.LE.nsl).AND.(moj.LE.nsr))then
      if((mol.EQ.nsr).AND.(nbuck.GT.3))goto 732
      endif
740   continue
745   continue
      mjst=mjsave
      endif
      iadk=iadk+ntt
760   continue
      leng=mola
      if(leng.NE.0)call fileio(1,(ibuck+50),leng,V1,0)
      
      if(nloop.EQ.0)then
      nloop=2
      goto 720
      endif
      
800   continue
900   continue
      write(Iout,99005)nint
      
1000  if(Idump.GT.1)call fdump
      return
      
      end
C* :1 * 
      
