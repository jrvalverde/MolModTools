
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nbodim"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nbodim.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "nbodim.web"
      subroutine nbodim(MEMORY)
      implicit none
      integer i,Iatcr,Iatno,Ichoos,il,im,Imval,Ino,io,ioinqr,Iprint,Ipse
     &ud,iread,Ispin,ityp,Iw3c,Iwapol,Iwcubf,Iwdetl,Iwdm
      integer Iwfock,Iwhybs,Iwmulp,Iwpnao,Iwtnab,Iwtnao,Iwtnbo,Iznuc,j,J
     &core,Jprint,Kopt,l,Label,Lang,Larc,Lbl,Lctr,Lfnao,Lfnarc
      integer Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnh
     &o,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Ll,llu,lm,Lorb
      integer Lorbc,Lstemt,Lstocc,Lu,Lval,MAXATM,MAXBAS,MEMORY,Munit,Mxa
     &o,mxao2,mxao3,Mxaolm,Mxbo,n,Natoms,Nbas,Ndim,need,need0
      integer need1,need2,need3,Norbs,nspdfg
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      dimension nspdfg(5,2)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbbas/Label(MAXBAS,6),Lval(MAXBAS),Imval(MAXBAS),Lstocc(MAX
     &BAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(MA
     &XBAS)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbao/Lctr(MAXBAS),Lang(MAXBAS)
      
      data iread/4HREAD/
      
      
      
      do 100 i=1,Nbas
      lm=Lang(i)
      Lval(i)=lm/100
      im=lm-Lval(i)*100
      if(im.GT.50)im=im-50
      Imval(i)=im
100   continue
      
      Mxao=0
      mxao2=0
      mxao3=0
      Mxaolm=0
      llu=0
      do 300 i=1,Natoms
      n=0
      do 150 il=1,5
      do 120 ityp=1,2
      nspdfg(il,ityp)=0
120   continue
150   continue
      do 200 j=1,Nbas
      if(Lctr(j).EQ.i)then
      lm=Lang(j)
      l=lm/100
      im=lm-l*100
      
      
      if(im.EQ.1)then
      
      
      ityp=1
      if(im.GT.50)ityp=2
      il=l+1
      nspdfg(il,ityp)=nspdfg(il,ityp)+1
      endif
      endif
      if(Lctr(j).EQ.i)n=n+1
200   continue
      
      
      nspdfg(1,1)=nspdfg(1,1)+nspdfg(1,2)+nspdfg(3,1)+nspdfg(5,1)
      
      
      nspdfg(2,1)=nspdfg(2,1)+nspdfg(2,2)+nspdfg(4,1)
      
      
      nspdfg(3,1)=nspdfg(3,1)+nspdfg(3,2)+nspdfg(5,1)
      
      
      nspdfg(4,1)=nspdfg(4,1)+nspdfg(4,2)
      
      
      nspdfg(5,1)=nspdfg(5,1)+nspdfg(5,2)
      
      do 250 il=1,5
      if(nspdfg(il,1).GT.Mxaolm)Mxaolm=nspdfg(il,1)
250   continue
      
      Norbs(i)=n
      Ll(i)=llu+1
      Lu(i)=Ll(i)+n-1
      llu=Lu(i)
      if(n.GT.Mxao)then
      mxao3=mxao2
      mxao2=Mxao
      Mxao=n
      elseif(n.GT.mxao2)then
      mxao3=mxao2
      mxao2=n
      elseif(n.GT.mxao3)then
      mxao3=n
      endif
300   continue
      Mxbo=Mxao+mxao2
      if(Iw3c.EQ.1)Mxbo=Mxbo+mxao3
      
      
      
      need0=2*Ndim*Ndim
      
      
      need1=0
      io=ioinqr(Iwtnao)
      if(io.NE.iread.AND..NOT.Ortho)then
      need=Ndim+Ndim+Ndim*Ndim+Mxaolm*Mxaolm+Ndim+Mxaolm*Mxaolm+Mxaolm*M
     &xaolm+Ndim+(9*Mxaolm+1)/2
      need1=max(need1,need)
      endif
      
      need=Natoms*Natoms+Natoms+Natoms*Natoms+Ndim*Ndim+Ndim
      need1=max(need1,need)
      
      need=Natoms*Natoms+Ndim*Ndim+Ndim
      need1=max(need1,need)
      
      if(Jprint(9).NE.0)then
      need=Natoms*Natoms+Ndim*Ndim+Ndim*Ndim+Ndim*(Ndim+5)
      need1=max(need1,need)
      endif
      
      need1=need1+need0
      
      
      need2=0
      if(Jprint(1).EQ.0)then
      if(ioinqr(Iwtnab).NE.iread)then
      need=Natoms*Natoms+Ndim+3*Ndim+Mxao*Ndim+Ndim+Mxbo*Mxbo+Mxbo*Mxbo+
     &Mxbo+Mxbo+Mxao*Mxao+Mxao*Mxao+Mxao+Mxao+Mxao+Natoms*Natoms
      else
      need=Natoms*Natoms+Ndim+3*Ndim
      endif
      need2=max(need2,need)
      
      if(.NOT.Ortho)then
      need=Natoms*Natoms+4*Ndim*Ndim+Mxao+3*Ndim
      need2=max(need2,need)
      endif
      
      need=Natoms*Natoms+Ndim+Mxao+Ndim*Ndim+Ndim*Ndim+Ndim+Ndim
      need2=max(need2,need)
      
      need=Natoms*Natoms+Ndim+Ndim+Ndim+Ndim*Ndim
      need2=max(need2,need)
      
      if(Jprint(36).NE.0)then
      need=Natoms*Natoms+Ndim+3*Natoms+Ndim*Ndim+Ndim*Ndim+Ndim
      need2=max(need2,need)
      endif
      
      need=Natoms*Natoms+Ndim+Ndim*Ndim+Ndim*Ndim+Ndim*(Ndim+5)
      need2=max(need2,need)
      
      if(Jprint(6).NE.0)then
      need=Natoms*Natoms+Ndim+Ndim*Ndim+Ndim+Natoms+Ndim
      need2=max(need2,need)
      endif
      
      
      need3=0
      if(Jprint(8).NE.0)then
      need=Ndim+Ndim+Ndim*Ndim+Ndim*Ndim
      need3=max(need3,need)
      
      need=Ndim+Ndim+Ndim+Natoms*Natoms+Natoms*Natoms+Ndim*Natoms+Ndim*N
     &atoms*(Natoms-1)/2+Ndim*Ndim
      need3=max(need3,need)
      
      need=Ndim*Ndim+Ndim*Ndim+Ndim*(Ndim+5)
      need3=max(need3,need)
      
      if(Jprint(46).NE.0)then
      need=Ndim*Ndim+Ndim*Ndim+Ndim*Ndim+Ndim*Ndim+Ndim*Ndim+Ndim*Ndim+(
     &Ndim+1)/2
      need3=max(need3,need)
      endif
      endif
      endif
      
      
      if(Iprint.GE.0)then
      if(Jprint(1).NE.0)then
      need2=0
      need3=0
      write(Lfnpr,99001)need1,MEMORY
      elseif(Jprint(8).NE.0)then
      write(Lfnpr,99003)need1,need2,need3,MEMORY
      else
      need3=0
      write(Lfnpr,99002)need1,need2,MEMORY
      endif
      endif
      if(need1.GT.MEMORY.OR.need2.GT.MEMORY.OR.need3.GT.MEMORY)then
      
      write(Lfnpr,99004)
      stop
      
99001 format(/1x,'Storage needed:',i6,' in NPA (',i7,' available)')
99002 format(/1x,'Storage needed:',i6,' in NPA,',i6,' in NBO (',i7,' ava
     &ilable)')
99003 format(/1x,'Storage needed:',i6,' in NPA,',i6,' in NBO,',i6,' in N
     &LMO (',i7,' available)')
99004 format(/1x,'*** Not enough core storage is available ***'/)
      endif
      return
      end
C* :1 * 
      
