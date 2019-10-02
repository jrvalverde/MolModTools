
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 chsdrv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "chsdrv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 35 "chsdrv.web"
      subroutine chsdrv(DM,T,GUIDE,BNDOCC,POL,Q,V,BLK,C,EVAL,BORB,P,TA,H
     &YB,VA,VB,TOPO)
      implicit none
      double precision BLK,BNDOCC,BORB,C,DM,EVAL,GUIDE,HYB,P,POL,Q,T,TA,
     &TOPO,V,VA,VB
      integer i,I3ctr,iat,iat1,iat2,iat3,Iatcr,Iathy,Iatno,Ibxm,Ichoos,i
     &flg,Ino,Iorder,Iprin,Iprint,Ipseud,Ispin,ival,Iw3c
      integer Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao,Iwtn
     &ab,Iwtnao,Iwtnbo,Iznuc,jat,Jcore,Jorder,Jprint,k3cbon,kalpha,kalt,
     &kat
      integer kbeta,kbond,kd,keywd,klone,Kopt,kq,ks,kt,l,Label,Larc,leng
     &,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo
      integer Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lf
     &npnl,Lfnppa,Lfnpr,Ll,Lstocc,Ltyp,Lu,MAXATM,MAXBAS,Munit,Mxao,Mxaol
     &m
      integer Mxbo,N3ctr,Naoctr,Naol,Natoms,Nbas,nbd,Nbotyp,Nbouni,nctr,
     &nctro,Ndim,nlp,Norbs,Ntopo,num
      
      logical end,error,equal
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbnao/Naoctr(MAXBAS),Naol(MAXBAS),Ltyp(MAXBAS),Iprin(MAXBAS
     &)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbtopo/Iorder(MAXATM),Jorder(MAXATM),Ntopo(MAXATM,MAXATM),N
     &3ctr,I3ctr(10,3)
      dimension DM(Ndim,Ndim),T(Ndim,Ndim),GUIDE(Natoms,Natoms),BNDOCC(N
     &dim),POL(Ndim,3),Q(Mxao,Ndim),V(Ndim),BLK(Mxbo,Mxbo),C(Mxbo,Mxbo),
     &EVAL(Mxbo),BORB(Mxbo),P(Mxao,Mxao),TA(Mxao,Mxao),HYB(Mxao),VA(Mxao
     &),VB(Mxao),TOPO(Natoms,Natoms)
      dimension keywd(6),klone(4),kbond(4),k3cbon(6),kalpha(5),kbeta(4),
     &ival(4),kalt(4)
      data klone/1HL,1HO,1HN,1HE/,kbond/1HB,1HO,1HN,1HD/,k3cbon/1H3,1HC,
     &1HB,1HO,1HN,1HD/,kalpha/1HA,1HL,1HP,1HH,1HA/,kbeta/1HB,1HE,1HT,1HA
     &/,ks/1HS/,kd/1HD/,kt/1HT/,kq/1HQ/,kalt/1H$,1HE,1HN,1HD/
      
      
      if(Ispin.EQ.2)then
50    leng=5
      call hfld(keywd,leng,end)
      if(end.AND.leng.EQ.0)then
      
      write(Lfnpr,99007)
      Jprint(1)=-1
      return
      elseif(.NOT.equal(keywd,kalpha,5))then
      goto 50
      endif
      elseif(Ispin.EQ.-2)then
100   leng=5
      call hfld(keywd,leng,end)
      if(end.AND.leng.EQ.0)then
      
      write(Lfnpr,99008)
      Jprint(1)=-1
      return
      elseif(.NOT.equal(keywd,kbeta,4))then
      goto 100
      endif
      endif
      
      
      do 200 iat=1,Natoms
      nlp=0
      call valtbl(iat,ival)
      do 150 l=0,3
      nlp=nlp+ival(l+1)*(2*l+1)
150   continue
      Ntopo(iat,iat)=100+nlp
200   continue
      
      
      nctr=0
      N3ctr=0
300   leng=6
      call hfld(keywd,leng,end)
      if(end.OR.equal(keywd,kalt,4))then
      
      
      do 350 iat=1,Natoms
      nlp=Ntopo(iat,iat)
      if(nlp.GE.100)then
      nlp=mod(nlp,100)
      nbd=0
      do 310 jat=1,Natoms
      if(iat.NE.jat.AND.Ntopo(jat,iat).NE.0)nbd=nbd+Ntopo(jat,iat)
310   continue
      do 320 kat=1,3
      do 315 jat=1,N3ctr
      if(I3ctr(jat,kat).EQ.iat)nbd=nbd+1
315   continue
320   continue
      nlp=nlp-nbd
      if(nlp.LT.0)nlp=0
      Ntopo(iat,iat)=nlp
      endif
350   continue
      
      
      iflg=0
      call choose(DM,T,GUIDE,BNDOCC,POL,Q,V,BLK,C,EVAL,BORB,P,TA,HYB,VA,
     &VB,TOPO,iflg)
      return
      else
      nctro=nctr
      nctr=0
      if(equal(keywd,klone,4))nctr=1
      if(equal(keywd,kbond,4))nctr=2
      if(equal(keywd,k3cbon,6))nctr=3
      if(nctr.EQ.0)then
      
      write(Lfnpr,99001)(keywd(i),i=1,6)
      Jprint(1)=-1
      return
      elseif(nctr.LT.nctro)then
      
      write(Lfnpr,99002)
      Jprint(1)=-1
      return
      
99001 format(/1x,'Error in input of bond orbitals:',/,1x,'Keyword for or
     &bital type is not LONE, BOND, or 3CBOND (read `',6A1,''')')
99002 format(/1x,'Error in input of bond orbitals:',/,1x,'Orbital types 
     &should be in the order: LONE, BOND, 3CBOND')
99003 format(/1x,'Error in input of bond orbitals:',/,1x,'Unrecognizable
     & characters in input of lone orbitals')
99004 format(/1x,'Error in input of bond orbitals:',/,1x,'Unrecognizable
     & characters in input of two center orbitals')
99005 format(/1x,'Error in input of bond orbitals:',/,1x,'Unrecognizable
     & characters in input of three center orbitals')
99006 format(/1x,'Too many three center bonds:','  Increase parameter MA
     &X3C')
99007 format(/1x,'End of file encountered before the word ALPHA was ','f
     &ound')
99008 format(/1x,'End of file encountered before the word BETA was ','fo
     &und')
      elseif(nctr.EQ.2)then
      
      
360   leng=1
      call hfld(keywd,leng,end)
      if(end)goto 300
      num=0
      if(equal(keywd,ks,1))num=1
      if(equal(keywd,kd,1))num=2
      if(equal(keywd,kt,1))num=3
      if(equal(keywd,kq,1))num=4
      if(num.NE.0)then
      call ifld(iat1,error)
      if(.NOT.(error))then
      call ifld(iat2,error)
      if(.NOT.(error))then
      iat=max0(iat1,iat2)
      jat=min0(iat1,iat2)
      Ntopo(iat,jat)=num
      Ntopo(jat,iat)=num
      goto 360
      endif
      endif
      endif
      
      write(Lfnpr,99004)
      Jprint(1)=-1
      return
      elseif(nctr.EQ.3)then
      
      
380   if(Iw3c.NE.1)Iw3c=1
      leng=1
      call hfld(keywd,leng,end)
      if(end)goto 300
      num=0
      if(equal(keywd,ks,1))num=1
      if(equal(keywd,kd,1))num=2
      if(equal(keywd,kt,1))num=3
      if(equal(keywd,kq,1))num=4
      if(num.NE.0)then
      call ifld(iat1,error)
      if(.NOT.(error))then
      call ifld(iat2,error)
      if(.NOT.(error))then
      call ifld(iat3,error)
      if(.NOT.(error))then
      N3ctr=N3ctr+1
      if(N3ctr.GT.10)goto 500
      I3ctr(N3ctr,1)=iat1
      I3ctr(N3ctr,2)=iat2
      I3ctr(N3ctr,3)=iat3
      goto 380
      endif
      endif
      endif
      endif
      
      write(Lfnpr,99005)
      Jprint(1)=-1
      return
      else
      
      
400   call ifld(iat,error)
      if(error)then
      leng=6
      call hfld(keywd,leng,end)
      goto 300
      endif
      call ifld(num,error)
      if(error)then
      write(Lfnpr,99003)
      Jprint(1)=-1
      return
      else
      Ntopo(iat,iat)=num
      goto 400
      endif
      endif
      endif
      
500   write(Lfnpr,99006)
      Jprint(1)=-1
      return
      end
C* :1 * 
      
