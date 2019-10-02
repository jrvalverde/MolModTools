
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 wrarc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "wrarc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "wrarc.web"
      subroutine wrarc(SCR,ISCR,LFN)
      implicit none
      double precision ablnks,acd,acentr,acf,acp,acs,aexp,alabel,ancomp,
     &anexp,anprim,anptr,anshll,SCR,str,zero
      integer i,i1,i2,i3,i4,i5,i6,i7,i8,iaomo,Iatcr,Iatno,Ichoos,idip,if
     &lg,ii,ik,Ino,Iprint,Ipseud
      integer ISCR,Ispin,Iw3c,Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iw
     &mulp,Iwpnao,Iwtnab,Iwtnao,Iwtnbo,Iznuc,j,Jcore,Jprint,k,kbas
      integer kblnk,kbodm,kcal,kcubf,kend,keq,kev,kgen,knat,kopen,Kopt,k
     &ortho,kupper,l2,Lang,Lctr,LFN,Lfnao,Lfnarc,Lfndaf
      integer Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnl
     &m,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Ll,Lu,MAXATM,MAXBAS,MAX
     &D
      integer Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,nc,nd,Ndim,nexp,nint,nl
     &,Norbs,nreal,nshell,nu
      
      parameter(MAXD=4)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbao/Lctr(MAXBAS),Lang(MAXBAS)
      
      dimension SCR(1),ISCR(1),ik(MAXD)
      dimension kgen(7),knat(6),kbas(4),kopen(4),kortho(5),kupper(5),kbo
     &dm(4),kev(2),kcubf(6),kend(4),kcal(4)
      
      data kgen/1H$,1HG,1HE,1HN,1HN,1HB,1HO/,kbas/1HN,1HB,1HA,1HS/,knat/
     &1HN,1HA,1HT,1HO,1HM,1HS/,kopen/1HO,1HP,1HE,1HN/,kortho/1HO,1HR,1HT
     &,1HH,1HO/,kupper/1HU,1HP,1HP,1HE,1HR/,kbodm/1HB,1HO,1HD,1HM/,kev/1
     &HE,1HV/,kend/1H$,1HE,1HN,1HD/,kcubf/1HC,1HU,1HB,1HI,1HC,1HF/,kcal/
     &1HK,1HC,1HA,1HL/
      data kblnk,keq/1H ,1H=/
      data ablnks,acentr,alabel/8H        ,8HCENTER =,8H LABEL =/
      data anshll,anexp,ancomp/8HNSHELL =,8H  NEXP =,8H NCOMP =/
      data anprim,anptr,aexp/8H NPRIM =,8H  NPTR =,8H   EXP =/
      data acs,acp,acd,acf/8H    CS =,8H    CP =,8H    CD =,8H    CF =/
      data zero/0.0D0/
      
      
      
      if(Nbas.NE.Ndim)then
      write(Lfnpr,99001)
      return
      endif
      
      
      nc=0
      do 100 i=1,7
      nc=nc+1
      ISCR(nc)=kgen(i)
100   continue
      nc=nc+1
      ISCR(nc)=kblnk
      nc=nc+1
      ISCR(nc)=kblnk
      
      
      do 200 i=1,6
      nc=nc+1
      ISCR(nc)=knat(i)
200   continue
      nc=nc+1
      ISCR(nc)=keq
      call idigit(Natoms,ik,nd,MAXD)
      do 300 i=1,nd
      nc=nc+1
      ISCR(nc)=ik(i)
300   continue
      nc=nc+1
      ISCR(nc)=kblnk
      nc=nc+1
      ISCR(nc)=kblnk
      
      do 400 i=1,4
      nc=nc+1
      ISCR(nc)=kbas(i)
400   continue
      nc=nc+1
      ISCR(nc)=keq
      call idigit(Nbas,ik,nd,MAXD)
      do 500 i=1,nd
      nc=nc+1
      ISCR(nc)=ik(i)
500   continue
      nc=nc+1
      ISCR(nc)=kblnk
      nc=nc+1
      ISCR(nc)=kblnk
      
      
      if(Open)then
      do 550 i=1,4
      nc=nc+1
      ISCR(nc)=kopen(i)
550   continue
      nc=nc+1
      ISCR(nc)=kblnk
      nc=nc+1
      ISCR(nc)=kblnk
      endif
      
      
      if(Ortho)then
      do 600 i=1,5
      nc=nc+1
      ISCR(nc)=kortho(i)
600   continue
      nc=nc+1
      ISCR(nc)=kblnk
      nc=nc+1
      ISCR(nc)=kblnk
      endif
      
      
      do 700 i=1,5
      nc=nc+1
      ISCR(nc)=kupper(i)
700   continue
      nc=nc+1
      ISCR(nc)=kblnk
      nc=nc+1
      ISCR(nc)=kblnk
      
      
      if(Iwdm.EQ.1)then
      do 750 i=1,4
      nc=nc+1
      ISCR(nc)=kbodm(i)
750   continue
      nc=nc+1
      ISCR(nc)=kblnk
      nc=nc+1
      ISCR(nc)=kblnk
      endif
      
      
      if(Munit.EQ.1)then
      nc=nc+1
      ISCR(nc)=kev(1)
      nc=nc+1
      ISCR(nc)=kev(2)
      nc=nc+1
      ISCR(nc)=kblnk
      nc=nc+1
      ISCR(nc)=kblnk
      endif
      
      
      if(Munit.EQ.1)then
      nc=nc+1
      ISCR(nc)=kcal(1)
      nc=nc+1
      ISCR(nc)=kcal(2)
      nc=nc+1
      ISCR(nc)=kcal(3)
      nc=nc+1
      ISCR(nc)=kcal(4)
      nc=nc+1
      ISCR(nc)=kblnk
      nc=nc+1
      ISCR(nc)=kblnk
      endif
      
      
      if(Iwcubf.NE.0)then
      do 800 i=1,6
      nc=nc+1
      ISCR(nc)=kcubf(i)
800   continue
      nc=nc+1
      ISCR(nc)=kblnk
      nc=nc+1
      ISCR(nc)=kblnk
      endif
      
      
      do 900 i=1,4
      nc=nc+1
      ISCR(nc)=kend(i)
900   continue
      
      
      write(LFN,99002)(ISCR(i),i=1,nc)
      
      
      write(LFN,99003)
      
      
      write(LFN,99004)
      call fetitl(SCR)
      write(LFN,99005)(SCR(i),i=1,10)
      call fecoor(SCR)
      j=1
      do 1000 i=1,Natoms
      write(LFN,99006)Iatno(i),Iznuc(i),SCR(j),SCR(j+1),SCR(j+2)
      j=j+3
1000  continue
      write(LFN,99007)
      
      
      write(LFN,99008)
      nint=17
      str=acentr
      do 1100 i=1,(Nbas-1)/nint+1
      nl=(i-1)*nint+1
      nu=min0(nl+nint-1,Nbas)
      write(LFN,99009)str,(Lctr(j),j=nl,nu)
      str=ablnks
1100  continue
      str=alabel
      do 1200 i=1,(Nbas-1)/nint+1
      nl=(i-1)*nint+1
      nu=min0(nl+nint-1,Nbas)
      write(LFN,99009)str,(Lang(j),j=nl,nu)
      str=ablnks
1200  continue
      write(LFN,99007)
      
      
      
      call febas(nshell,nexp,ISCR)
      
      
      i1=3
      i2=i1+nshell
      i3=i2+nshell
      i4=2+(3*nshell+1)/2
      i5=i4+nexp
      i6=i5+nexp
      i7=i6+nexp
      i8=i7+nexp
      
      
      if(nshell.GT.0)then
      
      
      write(LFN,99010)
      write(LFN,99009)anshll,nshell
      write(LFN,99009)anexp,nexp
      
      
      nint=17
      str=ancomp
      do 1250 i=1,(nshell-1)/nint+1
      nl=(i-1)*nint+1
      nu=min0(nl+nint-1,nshell)
      write(LFN,99009)str,(ISCR(j),j=i1+nl-1,i1+nu-1)
      str=ablnks
1250  continue
      
      
      str=anprim
      do 1300 i=1,(nshell-1)/nint+1
      nl=(i-1)*nint+1
      nu=min0(nl+nint-1,nshell)
      write(LFN,99009)str,(ISCR(j),j=i2+nl-1,i2+nu-1)
      str=ablnks
1300  continue
      
      
      str=anptr
      do 1350 i=1,(nshell-1)/nint+1
      nl=(i-1)*nint+1
      nu=min0(nl+nint-1,nshell)
      write(LFN,99009)str,(ISCR(j),j=i3+nl-1,i3+nu-1)
      str=ablnks
1350  continue
      
      
      nreal=4
      str=aexp
      do 1400 i=1,(nexp-1)/nreal+1
      nl=(i-1)*nreal+1
      nu=min0(nl+nreal-1,nexp)
      write(LFN,99011)str,(SCR(j),j=i4+nl-1,i4+nu-1)
      str=ablnks
1400  continue
      
      
      do 1450 i=1,4
      if(i.EQ.1)then
      str=acs
      ii=i5
      elseif(i.EQ.2)then
      str=acp
      ii=i6
      elseif(i.EQ.3)then
      str=acd
      ii=i7
      elseif(i.EQ.4)then
      str=acf
      ii=i8
      endif
      iflg=0
      do 1420 j=ii,ii+nexp-1
      if(SCR(j).NE.zero)iflg=1
1420  continue
      if(iflg.EQ.1)then
      do 1430 j=1,(nexp-1)/nreal+1
      nl=(j-1)*nreal+1
      nu=min0(nl+nreal-1,nexp)
      write(LFN,99011)str,(SCR(k),k=ii+nl-1,ii+nu-1)
      str=ablnks
1430  continue
      endif
1450  continue
      write(LFN,99007)
      endif
      
      
      l2=Ndim*(Ndim+1)/2
      if(.NOT.Ortho)then
      write(LFN,99012)
      call fesraw(SCR)
      l2=Ndim*(Ndim+1)/2
      call pack(SCR,Ndim,Nbas,l2)
      write(LFN,99013)(SCR(i),i=1,l2)
      write(LFN,99007)
      endif
      
      
      write(LFN,99014)
      if(Open)then
      Alpha=.TRUE.
      Beta=.FALSE.
      call fedraw(SCR,SCR)
      call pack(SCR,Ndim,Nbas,l2)
      write(LFN,99013)(SCR(i),i=1,l2)
      Alpha=.FALSE.
      Beta=.TRUE.
      call fedraw(SCR,SCR)
      call pack(SCR,Ndim,Nbas,l2)
      write(LFN,99013)(SCR(i),i=1,l2)
      else
      Alpha=.FALSE.
      Beta=.FALSE.
      call fedraw(SCR,SCR)
      call pack(SCR,Ndim,Nbas,l2)
      write(LFN,99013)(SCR(i),i=1,l2)
      endif
      write(LFN,99007)
      
      
      if(Open)then
      Alpha=.TRUE.
      Beta=.FALSE.
      Iwfock=1
      call fefao(SCR,Iwfock)
      if(Iwfock.NE.0)then
      write(LFN,99015)
      call pack(SCR,Ndim,Nbas,l2)
      write(LFN,99013)(SCR(i),i=1,l2)
      Alpha=.FALSE.
      Beta=.TRUE.
      call fefao(SCR,Iwfock)
      call pack(SCR,Ndim,Nbas,l2)
      write(LFN,99013)(SCR(i),i=1,l2)
      write(LFN,99007)
      endif
      else
      Alpha=.FALSE.
      Beta=.FALSE.
      Iwfock=1
      call fefao(SCR,Iwfock)
      if(Iwfock.NE.0)then
      write(LFN,99015)
      call pack(SCR,Ndim,Nbas,l2)
      write(LFN,99013)(SCR(i),i=1,l2)
      write(LFN,99007)
      endif
      endif
      
      
      if(Open)then
      Alpha=.TRUE.
      Beta=.FALSE.
      call feaomo(SCR,iaomo)
      if(iaomo.EQ.1)then
      write(LFN,99016)
      write(LFN,99013)(SCR(i),i=1,Ndim*Ndim)
      Alpha=.FALSE.
      Beta=.TRUE.
      call feaomo(SCR,iaomo)
      write(LFN,99013)(SCR(i),i=1,Ndim*Ndim)
      write(LFN,99007)
      endif
      else
      Alpha=.FALSE.
      Beta=.FALSE.
      call feaomo(SCR,iaomo)
      if(iaomo.EQ.1)then
      write(LFN,99016)
      write(LFN,99013)(SCR(i),i=1,Ndim*Ndim)
      write(LFN,99007)
      endif
      endif
      
      
      idip=1
      call fedxyz(SCR,idip)
      if(idip.NE.0)then
      write(LFN,99017)
      call pack(SCR,Ndim,Nbas,l2)
      write(LFN,99013)(SCR(i),i=1,l2)
      idip=2
      call fedxyz(SCR,idip)
      call pack(SCR,Ndim,Nbas,l2)
      write(LFN,99013)(SCR(i),i=1,l2)
      idip=3
      call fedxyz(SCR,idip)
      call pack(SCR,Ndim,Nbas,l2)
      write(LFN,99013)(SCR(i),i=1,l2)
      write(LFN,99007)
      endif
      return
      
99001 format(/1x,'The routine which writes the ARCHIVE file assumes ','N
     &BAS = NDIM.  Since',/1x,'this condition is not satisfied, ','the A
     &RCHIVE file will not be written.')
99002 format(1x,78A1)
99003 format(1x,'$NBO  $END')
99004 format(1x,'$COORD')
99005 format(1x,9A8,a6)
99006 format(1x,2I5,3F15.6)
99007 format(1x,'$END')
99008 format(1x,'$BASIS')
99009 format(1x,1x,a8,1x,17(i3,1x))
99010 format(1x,'$CONTRACT')
99011 format(1x,1x,a8,1x,4(e15.7,1x))
99012 format(1x,'$OVERLAP')
99013 format(1x,1x,5E15.7)
99014 format(1x,'$DENSITY')
99015 format(1x,'$FOCK')
99016 format(1x,'$LCAOMO')
99017 format(1x,'$DIPOLE')
      end
C* :1 * 
      
