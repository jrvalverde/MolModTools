
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 xcited"
C  RUN TIME:     "Friday, June 5, 2009 at 15:06."
C  WEB FILE:     "xcited.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 32 "xcited.web"
      subroutine xcited(DM,T,HYB,THYB,S,OCC,SCR,ISCR)
      implicit none
      double precision coef,DM,HYB,OCC,one,pct,S,SCR,T,temp,tenth,thresh
     &,THYB,tot,zero
      integer i,iat,Iatcr,Iathy,Iatno,ib,Ibxm,Ichoos,icnt,ictr,idiff,ih,
     &ihyb,Ino,ip,ipar3c,Iprin,Iprint,Ipseud,ISCR
      integer isgn,Ispin,Iw3c,Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iw
     &mulp,Iwpnao,Iwtnab,Iwtnao,Iwtnbo,Iznuc,j,Jcore,jctr,jhyb,jp
      integer Jprint,k,kctr,kh,khyb,kl,Kopt,ku,l3c,Label,lbd,lbl,lbl1,lb
     &l2,lblnk,lcr,Lfnao,Lfnarc,Lfndaf,Lfndef
      integer Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpn
     &a,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Ll,llp,lry,lstar,Lstocc,Ltyp
      integer Ltyp1,Lu,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Naoa,Naoc,Na
     &octr,Naol,Natoms,Nbas,nbond,nctr,Ndim,nhyb,nl,nocc
      integer Norbs
      logical first
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbnao/Naoc(MAXBAS),Naoa(MAXBAS),Ltyp1(MAXBAS),Iprin(MAXBAS)
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Ibxm(MAXBAS),Ltyp(MAXBAS),Iathy(MAXBAS,3)
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      dimension DM(Ndim,Ndim),T(Ndim,Ndim),HYB(Mxao),THYB(Ndim,Ndim),S(N
     &dim,Ndim),OCC(Ndim),SCR(Ndim),ISCR(Ndim)
      dimension pct(5),iat(3)
      data llp,lbd,l3c,lcr,lry/'LP','BD','3C','CR','RY'/
      data zero,tenth,one,thresh/0.0D0,0.1D0,1.0D0,1.0D-4/
      data lstar,lblnk/'*',' '/
      
      
      
      nhyb=0
      
      
      do 200 nbond=1,Nbas
      ib=Ibxm(nbond)
      lbl=Label(ib,1)
      if(lbl.EQ.llp.OR.lbl.EQ.lcr.OR.lbl.EQ.lry)nctr=1
      if(lbl.EQ.lbd)nctr=2
      if(lbl.EQ.l3c)nctr=3
      
      
      do 100 ictr=1,nctr
      i=Label(ib,ictr+3)
      kl=Ll(i)
      ku=Lu(i)
      do 20 k=1,Mxao
      Ltyp(k)=0
      HYB(k)=zero
20    continue
      
      
      isgn=1
      if(Label(ib,2).EQ.lstar)then
      if(ictr.GE.2)then
      if(ictr.EQ.3)ipar3c=-ipar3c
      if(ictr.NE.3.OR.ipar3c.LE.0)isgn=-isgn
      endif
      endif
      
      
      kh=0
      do 40 k=kl,ku
      kh=kh+1
      HYB(kh)=T(k,nbond)
      Ltyp(kh)=Naoa(k)/100
40    continue
      call htype(HYB,Ltyp,Mxao,kh,coef,pct,nl,isgn)
      if(dabs(coef).GE.thresh)then
      
      
      do 50 ihyb=1,nhyb
      temp=zero
      ih=0
      do 45 k=kl,ku
      ih=ih+1
      temp=temp+HYB(ih)*THYB(k,ihyb)
45    continue
      if(dabs(dabs(temp)-one).LT.thresh)goto 100
      if(dabs(temp).GT.thresh)then
      write(Lfnpr,99001)nhyb+1,nbond,ictr,temp,ihyb
      stop
      endif
50    continue
      
      
      nhyb=nhyb+1
      if(nhyb.GT.Nbas)stop 'Too many hybrids'
      do 60 k=1,Nbas
      THYB(k,nhyb)=zero
60    continue
      ih=0
      do 70 k=kl,ku
      ih=ih+1
      THYB(k,nhyb)=HYB(ih)
70    continue
      endif
100   continue
200   continue
      if(nhyb.LT.Nbas)stop 'Missing hybrids'
      
      
      call fesnao(S)
      call simtrs(S,THYB,SCR,Ndim,Nbas)
      
      call trnspo(THYB,Ndim,Nbas)
      call matmlt(THYB,T,SCR,Ndim,Nbas)
      
      
      first=.TRUE.
      do 300 nbond=1,Nbas
      ib=Ibxm(nbond)
      lbl1=Label(ib,1)
      if(lbl1.EQ.llp.OR.lbl1.EQ.lcr.OR.lbl1.EQ.lry)ictr=1
      if(lbl1.EQ.lbd)ictr=2
      if(lbl1.EQ.l3c)ictr=3
      nctr=0
      do 250 ihyb=1,nhyb
      if(dabs(THYB(ihyb,nbond)).GT.thresh)then
      nctr=nctr+1
      if(nctr.GT.3)then
      write(Lfnpr,99002)nbond
      stop
      endif
      iat(nctr)=ihyb
      endif
250   continue
      if(nctr.GT.ictr)then
      write(Lfnpr,99003)ictr,nbond,nctr
      stop
      endif
      if(nctr.GT.1)then
      isgn=1
      do 280 jctr=1,nctr-1
      do 260 kctr=jctr+1,nctr
      jhyb=iat(jctr)
      khyb=iat(kctr)
      temp=S(jhyb,khyb)*THYB(jhyb,nbond)*THYB(khyb,nbond)
      if(temp.LT.-thresh)isgn=-1
260   continue
280   continue
      lbl2=Label(ib,2)
      if(lbl2.EQ.lblnk.AND.isgn.EQ.-1)then
      if(first)write(Lfnpr,99004)
      first=.FALSE.
      Label(ib,2)=lstar
      write(Lfnpr,99005)nbond,lbl1,lstar
      elseif(lbl2.EQ.lstar.AND.isgn.EQ.1)then
      if(first)write(Lfnpr,99004)
      first=.FALSE.
      Label(ib,2)=lblnk
      write(Lfnpr,99005)nbond,lbl1,lblnk
      endif
      endif
300   continue
      
      
      tot=zero
      do 400 i=1,Nbas
      tot=tot+DM(i,i)
400   continue
      nocc=tot+tenth
      if(Ispin.EQ.0)nocc=nocc/2+mod(nocc,2)
      
      
      icnt=0
      do 500 i=1,Nbas
      if(Label(Ibxm(i),2).NE.lstar)icnt=icnt+1
500   continue
      
      
      if(icnt.NE.nocc)then
      do 550 i=1,Nbas
      OCC(i)=DM(i,i)
550   continue
      call rank(OCC,Nbas,Ndim,ISCR)
      
      
      if(icnt.GT.nocc)then
      idiff=icnt-nocc
      do 580 i=1,idiff
      ip=0
      do 560 j=1,Nbas
      jp=Ibxm(ISCR(j))
      if(Label(jp,1).EQ.llp.AND.Label(jp,2).NE.lstar)ip=j
560   continue
      if(ip.EQ.0)then
      write(Lfnpr,99006)icnt,nocc
      stop
      endif
      Label(Ibxm(ISCR(ip)),2)=lstar
      write(Lfnpr,99005)ISCR(ip),Label(Ibxm(ISCR(ip)),1),lstar
580   continue
      
      
      else
      idiff=nocc-icnt
      do 600 i=1,idiff
      ip=0
      do 590 j=Nbas,1,-1
      jp=Ibxm(ISCR(j))
      if((Label(jp,1).EQ.llp.OR.Label(jp,1).EQ.lry).AND.Label(jp,2).EQ.l
     &star)ip=j
590   continue
      if(ip.EQ.0)then
      write(Lfnpr,99006)icnt,nocc
      stop
      endif
      Label(Ibxm(ISCR(ip)),2)=lblnk
      write(Lfnpr,99005)ISCR(ip),Label(Ibxm(ISCR(ip)),1),lblnk
600   continue
      endif
      endif
      return
      
99001 format(/1x,'Hybrid ',i3,' (NBO ',i3,', Center ',i2,') has a ','non
     &-negligible overlap of ',f8.5,/,1x,'with hybrid ',i3,'.')
99002 format(/1x,'NBO ',i3,' has hybrid contributions from more than ','
     &3 atomic centers.')
99003 format(/1x,'Error: the ',i1,'-center NBO ',i3,' has ','contributio
     &ns from ',i2,' atomic centers.')
99004 format(/1x,'          --- Apparent excited state configuration ','
     &---',/1x,'The following "inverted" NBO labels reflect the ','actua
     &l hybrid overlap:')
99005 format(1x,'                NBO ',i3,' has been relabelled ',a2,a1)
99006 format(/1x,'Unable to label the NBOs properly: ',i3,' starred ','o
     &rbitals',/1x,'                                   ',i3,' occupied o
     &rbitals')
      end
C* :1 * 
      
