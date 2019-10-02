
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nbo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nbo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 65 "nbo.web"
      subroutine nbo(CORE,MEMORY,NBOOPT)
      implicit none
      double precision CORE
      integer i,Iatcr,Iatno,Ibxm,Ichoos,idone,Ino,Iprint,Ipseud,Ispin,Iw
     &3c,Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao,Iwtnab
      integer Iwtnao,Iwtnbo,Iznuc,Jcore,Jprint,Kopt,Label,Lang,Larc,Lbl,
     &Lctr,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao
      integer Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lf
     &npr,Ll,Lorb,Lorbc,Lstocc,Lu,MAXATM,MAXBAS,MEMORY,Munit,Mxao,Mxaolm
      integer Mxbo,n2,Natoms,Nbas,NBOOPT,Nbotyp,Nbouni,Ndim,ndm,Norbs,ns
     &cr,nt
      logical newdaf,error
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      common/nbao/Lctr(MAXBAS),Lang(MAXBAS)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      dimension CORE(MEMORY),NBOOPT(10)
      
      
      if(NBOOPT(1).EQ.-2)return
      
      
      call nboset(NBOOPT)
      
      
      if(NBOOPT(10).EQ.0)then
      call geninp(newdaf)
      else
      newdaf=.TRUE.
      endif
      
      
      call nboinp(NBOOPT,idone)
      if(idone.EQ.1)return
      
      
      call jobopt(NBOOPT)
      
      
      call nbopen(newdaf,error)
      if(error)then
      write(Lfnpr,99001)
      return
      endif
      
      
      call feaoin(CORE,CORE,NBOOPT)
      if(Complx)return
      
      
      call fetitl(CORE)
      write(Lfnpr,99002)(CORE(i),i=1,8),CORE(9)
      
      
      call nbodim(MEMORY)
      
      
      
      n2=Ndim*Ndim
      ndm=1
      nt=ndm+n2
      nscr=nt+n2
      
      
      Alpha=.FALSE.
      Beta=.FALSE.
      Ispin=0
      call fedraw(CORE(ndm),CORE(nscr))
      
      
      if(Ortho)then
      call naosim(CORE(ndm),CORE(nt),CORE(nscr))
      
      
      else
      call fesraw(CORE(nt))
      call naodrv(CORE(ndm),CORE(nt),CORE(nscr))
      endif
      
      
      
      if(.NOT.Open)then
      call nbodrv(CORE(ndm),CORE(nt),CORE(nscr))
      else
      
      
      
      Alpha=.TRUE.
      Beta=.FALSE.
      Ispin=2
      if(Ortho)then
      call dmsim(CORE(ndm),CORE(nt),CORE(nscr))
      else
      call dmnao(CORE(ndm),CORE(nt),CORE(nscr))
      endif
      call nbodrv(CORE(ndm),CORE(nt),CORE(nscr))
      
      
      Alpha=.FALSE.
      Beta=.TRUE.
      Ispin=-2
      if(Ortho)then
      call dmsim(CORE(ndm),CORE(nt),CORE(nscr))
      else
      call dmnao(CORE(ndm),CORE(nt),CORE(nscr))
      endif
      call nbodrv(CORE(ndm),CORE(nt),CORE(nscr))
      endif
      
      
      call nbclos
      return
      
99001 format(/1x,'NBO direct access file could not be opened.  NBO ','pr
     &ogram aborted.')
99002 format(/1x,'Job title: ',8A8,a5)
      end
C* :1 * 
      
