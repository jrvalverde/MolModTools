
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cycles"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cycles.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "cycles.web"
      subroutine cycles(ITER,THRESH,GUIDE,BNDOCC,TOPO,ICONT)
      implicit none
      double precision Accthr,Athr,best,BNDOCC,Crtset,dev,dev1,devmin,de
     &vthr,Dthr,E2thr,Ethr,GUIDE,hundrd,one,onept5,Prjset,Pthr,rho,rhomi
     &n
      double precision small,sum,sumlew,tenth,three,THRESH,thrmin,Thrset
     &,TOPO,totele,zero
      integer i,I3ctr,iat,iat1,iat2,iat3,Iatcr,Iathy,Iatno,ib,ibadl,ibad
     &nl,Ibxm,Ichoos,ICONT,iflg,Ino,Iorder,Iprint,Ipseud
      integer Ispin,ITER,Iw3c,Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iw
     &mulp,Iwpnao,Iwtnab,Iwtnao,Iwtnbo,Iznuc,j,jat,jbadl,Jcore,jflg
      integer Jorder,Jprint,jter,jtermx,Kopt,l3c,Label,Larc,lbd,lcr,Lfna
     &o,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo
      integer Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Ll,
     &llp,lstar,Lstocc,Lu,m3c,MAXATM,MAXBAS,mbd,mcr,mecp,mlp
      integer Munit,Mxao,Mxaolm,Mxbo,N3ctr,Natoms,Nbas,Nbotyp,Nbouni,nct
     &r,Ndim,nel,Norbs,Ntopo
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      common/nbthr/Thrset,Prjset,Accthr,Crtset,E2thr,Athr,Pthr,Ethr,Dthr
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbtopo/Iorder(MAXATM),Jorder(MAXATM),Ntopo(MAXATM,MAXATM),N
     &3ctr,I3ctr(10,3)
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      dimension GUIDE(Natoms,Natoms),BNDOCC(Ndim),TOPO(Natoms,Natoms)
      
      save jter,devmin,rhomin,best,rho,jbadl
      
      data lcr,lbd,l3c,llp,lstar/2HCR,2HBD,2H3C,2HLP,1H*/
      data small,zero,tenth,one,onept5,three,hundrd/1.0D-4,0.0D0,0.1D0,1
     &.0D0,1.5D0,3.0D0,1.0D2/
      data devthr/0.1D0/
      data jtermx/9/
      
      
      
      
      
      
      if(ITER.EQ.1)then
      jter=0
      ICONT=-1
      endif
      jter=jter+1
      if(jter.EQ.1)devmin=hundrd
      
      
      thrmin=onept5
      if(Ispin.NE.0)thrmin=thrmin-one
      
      
      ibadl=0
      ibadnl=0
      sumlew=zero
      totele=zero
      do 100 i=1,Nbas
      totele=totele+BNDOCC(i)
      if(Label(Ibxm(i),2).NE.lstar)then
      sumlew=sumlew+BNDOCC(i)
      if(BNDOCC(i).LT.THRESH)ibadl=ibadl+1
      else
      if(BNDOCC(i).GT.abs(Accthr))ibadnl=ibadnl+1
      endif
100   continue
      nel=totele+tenth
      totele=nel
      sum=totele-sumlew
      
      
      if(Ipseud.NE.0)then
      mecp=0
      do 150 iat=1,Natoms
      mecp=mecp+Iatno(iat)-Iznuc(iat)
150   continue
      if(Ispin.NE.0)mecp=mecp/2
      sumlew=sumlew+dble(mecp)
      endif
      
      
      if(jter.EQ.1)rhomin=hundrd
      if(ITER.EQ.1.OR.sum.LT.rho)then
      best=THRESH
      rho=sum
      jbadl=ibadl
      do 200 i=1,Natoms
      Jorder(i)=Iorder(i)
200   continue
      endif
      
      
      mcr=0
      mbd=0
      m3c=0
      mlp=0
      do 300 i=1,Nbas
      if(Label(i,1).EQ.lcr.AND.Label(i,2).NE.lstar)mcr=mcr+1
      if(Label(i,1).EQ.lbd.AND.Label(i,2).NE.lstar)mbd=mbd+1
      if(Label(i,1).EQ.l3c.AND.Label(i,2).NE.lstar)m3c=m3c+1
      if(Label(i,1).EQ.llp.AND.Label(i,2).NE.lstar)mlp=mlp+1
300   continue
      
      
      do 400 i=1,Natoms
      do 350 j=1,Natoms
      TOPO(i,j)=zero
350   continue
400   continue
      
      do 500 i=1,Nbas
      ib=Ibxm(i)
      if(Label(ib,1).NE.lcr.AND.Label(ib,2).NE.lstar)then
      iat1=Label(ib,4)
      nctr=1
      iat2=Label(ib,5)
      if(iat2.NE.0)nctr=2
      iat3=Label(ib,6)
      if(iat3.NE.0)nctr=3
      if(nctr.EQ.1)then
      TOPO(iat1,iat1)=TOPO(iat1,iat1)+one
      elseif(nctr.EQ.2)then
      TOPO(iat1,iat2)=TOPO(iat1,iat2)+one
      TOPO(iat2,iat1)=TOPO(iat2,iat1)+one
      else
      TOPO(iat1,iat2)=TOPO(iat1,iat2)+one/three
      TOPO(iat2,iat1)=TOPO(iat2,iat1)+one/three
      TOPO(iat1,iat3)=TOPO(iat1,iat3)+one/three
      TOPO(iat3,iat1)=TOPO(iat3,iat1)+one/three
      TOPO(iat2,iat3)=TOPO(iat2,iat3)+one/three
      TOPO(iat3,iat2)=TOPO(iat3,iat2)+one/three
      endif
      endif
500   continue
      
      
      dev=zero
      do 600 j=2,Natoms
      do 550 i=1,j-1
      if(GUIDE(i,j)-TOPO(i,j).GT.dev)then
      dev=GUIDE(i,j)-TOPO(i,j)
      iat=i
      jat=j
      endif
550   continue
600   continue
      
      
      if(Jprint(5).EQ.1)then
      if(ITER.EQ.1)write(Lfnpr,99001)
      write(Lfnpr,99002)ITER,jter,abs(THRESH),sumlew,sum,mcr,mbd,m3c,mlp
     &,ibadl,ibadnl,dev
      endif
      
      
      
      if(ibadl.EQ.0.AND.dev.LT.devthr)then
      if(Jprint(5).EQ.1)write(Lfnpr,99003)
      if(Jprint(5).EQ.1)write(Lfnpr,99004)
      ICONT=2
      return
      elseif(Natoms.EQ.1)then
      if(Jprint(5).EQ.1)write(Lfnpr,99003)
      if(Jprint(5).EQ.1)write(Lfnpr,99005)
      ICONT=2
      return
      elseif(Ichoos.EQ.1)then
      if(Jprint(5).EQ.1)write(Lfnpr,99003)
      if(Jprint(5).EQ.1)write(Lfnpr,99006)
      ICONT=2
      return
      elseif(Jprint(10).NE.0)then
      if(Jprint(5).EQ.1)write(Lfnpr,99003)
      if(Jprint(5).EQ.1)write(Lfnpr,99007)
      ICONT=2
      return
      endif
      
      
      if(ICONT.EQ.1)then
      if(Thrset.GE.zero)then
      if(Jprint(5).EQ.1)write(Lfnpr,99003)
      if(Jprint(5).EQ.1)write(Lfnpr,99008)
      ICONT=2
      elseif(Jprint(14).NE.0)then
      if(Jprint(5).EQ.1)write(Lfnpr,99003)
      if(Jprint(5).EQ.1)write(Lfnpr,99009)
      ICONT=2
      elseif(ibadl.NE.0)then
      if(Jprint(5).EQ.1)write(Lfnpr,99003)
      if(Jprint(5).EQ.1)write(Lfnpr,99004)
      ICONT=2
      endif
      return
      endif
      
      
      if((abs(dev-devmin).LT.small.AND.abs(sum-rhomin).LT.small).OR.jter
     &.GE.jtermx)then
      
      
      if(Thrset.GE.zero)then
      if(abs(sum-rho).LT.small)then
      if(Jprint(5).EQ.1)write(Lfnpr,99003)
      if(Jprint(5).EQ.1)write(Lfnpr,99008)
      ICONT=2
      else
      do 610 i=1,Natoms
      Iorder(i)=Jorder(i)
610   continue
      jter=0
      ICONT=1
      endif
      
      
      elseif(Jprint(14).NE.0)then
      THRESH=THRESH-tenth
      if(thrmin-THRESH.GT.small)then
      THRESH=THRESH+tenth
      if(abs(THRESH-best).LT.small.AND.abs(sum-rho).LT.small)then
      if(Jprint(5).EQ.1)write(Lfnpr,99003)
      if(Jprint(5).EQ.1)write(Lfnpr,99009)
      ICONT=2
      else
      do 615 i=1,Natoms
      Iorder(i)=Jorder(i)
615   continue
      THRESH=best
      jter=0
      ICONT=1
      endif
      else
      do 620 i=1,Natoms
      Iorder(i)=Jorder(i)
620   continue
      jter=0
      ICONT=-1
      endif
      
      
      elseif(abs(sum-rho).LT.small.AND.ibadl.EQ.0)then
      if(Jprint(5).EQ.1)write(Lfnpr,99003)
      if(Jprint(5).EQ.1)write(Lfnpr,99004)
      ICONT=2
      elseif(jbadl.EQ.0)then
      do 640 i=1,Natoms
      Iorder(i)=Jorder(i)
640   continue
      jter=0
      ICONT=1
      else
      if(Jprint(5).EQ.1)write(Lfnpr,99003)
      if(Jprint(5).EQ.1)write(Lfnpr,99010)
      ICONT=0
      endif
      return
      
      
      else
      if(dev.LT.devmin)devmin=dev
      if(sum.LT.rhomin)rhomin=sum
      if(iat.EQ.Iorder(1).AND.jat.EQ.Iorder(2))then
      dev1=zero
      do 660 j=2,Natoms
      do 650 i=1,j-1
      if(GUIDE(i,j)-TOPO(i,j).GT.dev1)then
      if((i.NE.Iorder(1).AND.j.NE.Iorder(2)).AND.(j.NE.Iorder(1).AND.i.N
     &E.Iorder(2)))then
      dev1=GUIDE(i,j)-TOPO(i,j)
      iat=i
      jat=j
      endif
      endif
650   continue
660   continue
      endif
      
      jflg=0
      do 700 i=Natoms,2,-1
      if(Iorder(i).EQ.jat)jflg=1
      if(jflg.EQ.1)Iorder(i)=Iorder(i-1)
700   continue
      Iorder(1)=jat
      iflg=0
      do 750 i=Natoms,2,-1
      if(Iorder(i).EQ.iat)iflg=1
      if(iflg.EQ.1)Iorder(i)=Iorder(i-1)
750   continue
      Iorder(1)=iat
      ICONT=-1
      endif
      return
      
99001 format(/1x,'                      Occupancies       Lewis ','Struc
     &ture    Low   High',/1x,'          Occ.    --------','----------- 
     & -----------------   occ   occ',/1x,' Cycle ','  Thresh.   Lewis  
     & Non-Lewis     CR  BD  3C  LP    (L) ','  (NL)   Dev',/1x,77('='))
99002 format(1x,i3,'(',i1,')',3x,f5.2,f12.5,f10.5,3x,4I4,2x,i4,3x,i4,3x,
     &f5.2)
99003 format(1x,77('-'))
99004 format(/1x,'Structure accepted: No low occupancy Lewis orbitals')
99005 format(/1x,'Structure accepted: Only a single atom')
99006 format(/1x,'Structure accepted: NBOs selected via the $CHOOSE ','k
     &eylist')
99007 format(/1x,'Structure accepted: Search for bonds prevented ','by N
     &OBOND keyword')
99008 format(/1x,'Structure accepted: Occupancy threshold (THRESH) ','se
     &t by user')
99009 format(/1x,'Structure accepted: RESONANCE keyword permits ','stron
     &gly delocalized structure')
99010 format(/1x,'Only strongly delocalized resonance structures can',' 
     &be found.',/1x,'The default procedure is to abort the NBO ','searc
     &h.')
      end
C* :1 * 
      
