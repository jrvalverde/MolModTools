
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nao"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nao.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 32 "nao.web"
      subroutine nao(T,S,OCC,BLK,SBLK,EVAL,C,EVECT,EVAL2,LISTAO,NBLOCK)
      implicit none
      double precision BLK,C,EVAL,EVAL2,EVECT,OCC,one,S,SBLK,T,zero
      integer i,iao,Iatcr,Iatno,Ichoos,icntr,ijr,il,ilbl,im,Ino,ioinqr,I
     &print,iprnt,Ipseud,iread,Ispin,Iw3c,Iwapol,Iwcubf
      integer Iwdetl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao,iwrit,Iwtnab,Iwtna
     &o,Iwtnbo,Iznuc,j,Jcore,Jprint,jr,Kopt,l,Label,lang,Larc
      integer Lbl,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lf
     &nnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr
     &,LISTAO
      integer Ll,Lorb,Lorbc,Lstemt,Lstocc,Lu,m,MAXATM,MAXBAS,Munit,Mxao,
     &Mxaolm,Mxbo,Naoctr,Naol,Natoms,Nbas,NBLOCK,Ndim,nemt
      integer nf,nl,nlang,nocc,Norbs,nsel1,nsel2
      
      
      
      
      
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      dimension T(Ndim,Ndim),S(Ndim,Ndim),OCC(Ndim),BLK(Ndim,Ndim),SBLK(
     &Mxaolm,Mxaolm),EVAL(Nbas),EVAL2(Nbas),LISTAO(Mxaolm,9),C(NBLOCK),E
     &VECT(Mxaolm,Mxaolm)
      character*80 title
      data zero,one/0.0D0,1.0D0/
      data iprnt,iwrit,iread/4HPRNT,4HWRIT,4HREAD/
      
      
      if(ioinqr(Iwpnao).NE.iread)then
      
      
      do 50 j=1,Nbas
      Lstocc(j)=0
      Lstemt(j)=0
      do 20 i=1,Nbas
      T(i,j)=zero
20    continue
50    continue
      
      
      nf=0
      
      
      nocc=0
      nemt=0
      
      
      do 100 icntr=1,Natoms
      
      
      do 80 il=1,5
      if(nf.LE.Nbas)then
      l=il-1
      m=2*l+1
      
      
      do 55 im=1,m
      lang=100*l+im+50
      nl=0
      do 52 i=1,Nbas
      if((Lbl(i).EQ.icntr).AND.(Lorb(i).EQ.lang))then
      nl=nl+1
      LISTAO(nl,im)=i
      endif
52    continue
55    continue
      if(nl.EQ.0)goto 100
      
      
      call loadav(LISTAO,nl,m,S,Ndim,BLK,SBLK,Mxaolm)
      
      
      call atdiag(nl,BLK,SBLK,EVAL,C)
      
      
      call rank(EVAL,nl,nl,Larc)
      
      
      do 60 im=1,m
      
      
      call setbas(Lstocc,Lstemt,nocc,nemt,icntr,l,nl,nf,Ndim)
      
      
      do 58 j=1,nl
      jr=Larc(j)
      nf=nf+1
      OCC(nf)=EVAL(j)
      do 56 i=1,nl
      iao=LISTAO(i,im)
      ijr=i+nl*(jr-1)
      T(iao,nf)=C(ijr)
56    continue
      
      
      Naoctr(nf)=icntr
      Naol(nf)=l*100+im+50
58    continue
60    continue
      endif
80    continue
100   continue
      endif
      
      
      if(ioinqr(Iwpnao).EQ.iread)then
      call rdppna(T,OCC)
      
      
      if(OCC(1).LT.zero)call newwts(S,T,OCC)
      nocc=0
      nemt=0
      lang=0
      ilbl=1
      nlang=0
      do 150 i=1,Nbas
      if(Lstocc(i).GT.0)nocc=nocc+1
      if((Naoctr(i).NE.ilbl).OR.(Naol(i).NE.lang))then
      if(nlang.GT.Mxaolm)Mxaolm=nlang
      nlang=1
      ilbl=Naoctr(i)
      lang=Naol(i)
      else
      nlang=nlang+1
      endif
      do 120 j=1,Nbas
      if(Lstocc(j).EQ.i)goto 150
120   continue
      nemt=nemt+1
      Lstemt(nemt)=i
150   continue
      endif
      
      
      if(ioinqr(Iwpnao).EQ.iwrit)call wrppna(T,OCC,Iwpnao)
      
      
      call svpnao(T)
      if(ioinqr(Iwpnao).EQ.iprnt)then
      title='PNAOs in the PAO basis:'
      call aout(T,Ndim,Nbas,Nbas,title,-1,Iwpnao)
      endif
      
      
      do 300 i=1,Nbas
      do 200 j=1,i
      S(j,i)=S(i,j)
200   continue
      S(i,i)=one
300   continue
      call worth(S,T,BLK,Lstocc,Ndim,Nbas,nocc,OCC,EVAL,BLK)
      if(nemt.NE.0)then
      call shmdt(T,S,Ndim,Nbas,nocc,Lstocc,nemt,Lstemt,BLK)
      
      
      call feppao(BLK)
      do 350 j=1,Nbas
      do 320 i=1,j
      S(i,j)=BLK(i,j)
320   continue
350   continue
      call newryd(T,S,BLK,C,SBLK,EVECT,OCC,EVAL,EVAL2,LISTAO,Jprint(11))
      
      
      call rydsel(Lstemt,nemt,nsel1,Larc,nsel2,LISTAO,OCC)
      if(nsel1.NE.0)then
      call worth(S,T,BLK,Larc,Ndim,Nbas,nsel1,OCC,EVAL,BLK)
      if(nsel2.EQ.0)goto 400
      endif
      if(nsel1.NE.0)call shmdt(T,S,Ndim,Nbas,nsel1,Larc,nsel2,LISTAO,BLK
     &)
      call worth(S,T,BLK,LISTAO,Ndim,Nbas,nsel2,OCC,EVAL,BLK)
      endif
400   call feppao(S)
      call simtrs(S,T,OCC,Ndim,Nbas)
      call rediag(S,T,BLK,OCC,SBLK,C,LISTAO,Jprint(11))
      
      
      do 500 i=1,Nbas
      Lstocc(i)=1
500   continue
      do 600 i=1,nemt
      Lstocc(Lstemt(i))=0
600   continue
      return
      end
C* :1 * 
      
