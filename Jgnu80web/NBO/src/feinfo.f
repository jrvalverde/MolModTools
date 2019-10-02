
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 feinfo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "feinfo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "feinfo.web"
      subroutine feinfo(ICORE,ISWEAN)
      implicit none
      integer Iatcr,Iatno,Ichoos,ICORE,Imval,Ino,Iprint,Ipseud,Ispin,ISW
     &EAN,Iw3c,Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao,Iwt
     &nab
      integer Iwtnao,Iwtnbo,Iznuc,Jcore,Jprint,Kopt,Label,Lang,Larc,Lbl,
     &Lctr,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao
      integer Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lf
     &npr,Ll,Lorb,Lorbc,Lstemt,Lstocc,Lu,Lval,MAXATM,MAXBAS,Munit,Mxao
      integer Mxaolm,Mxbo,Natoms,Nbas,Ndim,nfile,Norbs
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      dimension ICORE(*)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbbas/Label(MAXBAS,6),Lval(MAXBAS),Imval(MAXBAS),Lstocc(MAX
     &BAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(MA
     &XBAS)
      common/nbao/Lctr(MAXBAS),Lang(MAXBAS)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      
      
      nfile=3
      call nbread(ICORE,6,nfile)
      Natoms=ICORE(1)
      Ndim=ICORE(2)
      Nbas=ICORE(3)
      Munit=ICORE(4)
      Rohf=.FALSE.
      if(ICORE(5).EQ.1)Rohf=.TRUE.
      Uhf=.FALSE.
      if(ICORE(6).EQ.1)Uhf=.TRUE.
      Ci=.FALSE.
      if(ICORE(7).EQ.1)Ci=.TRUE.
      Open=.FALSE.
      if(ICORE(8).EQ.1)Open=.TRUE.
      Mcscf=.FALSE.
      if(ICORE(9).EQ.1)Mcscf=.TRUE.
      Auhf=.FALSE.
      if(ICORE(10).EQ.1)Auhf=.TRUE.
      Ortho=.FALSE.
      if(ICORE(11).EQ.1)Ortho=.TRUE.
      ISWEAN=ICORE(12)
      
      
      if(ISWEAN.EQ.1)then
      ICORE(12)=0
      call nbwrit(ICORE,6,nfile)
      endif
      return
      end
C* :1 * 
      
