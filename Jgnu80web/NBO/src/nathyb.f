
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nathyb"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nathyb.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 41 "nathyb.web"
      
      
      
      subroutine nathyb(DM,T,GUIDE,BNDOCC,POL,Q,V,BLK,C,EVAL,BORB,P,TA,H
     &YB,VA,VB,TOPO)
      implicit none
      double precision Accthr,Athr,BLK,BNDOCC,BORB,C,crthrs,Crtset,DM,Dt
     &hr,E2thr,Ethr,EVAL,four,gmax,gthrsh,GUIDE,HYB,hybexp,occ
      double precision occi,occmx,oldprj,one,P,POL,prjinc,Prjset,prjthr,
     &pt8,pt99,Pthr,Q,T,TA,tenth,thresh,Thrset,TOPO,totele
      double precision two,twop,V,VA,VB,zero,zerop,zeropm
      integer i,i1st,I3ctr,ia,ia1,iab,iaccep,ialarm,iat,iat1,iat2,iat3,I
     &atcr,Iathy,Iatno,iaugm,ib,ib1,ibd,iblnk
      integer ibo,Ibxm,ic,ic1,Ichoos,icnt,icont,iflg,ihyb,ilow,Ino,inxt,
     &iocc,iocclp,Iorder,Iprin,Iprint,Ipseud,iptr,ir
      integer irnk,iryd,Ispin,istar,iter,iula,Iw3c,Iwapol,Iwcubf,Iwdetl,
     &Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao,iwprj,Iwtnab,Iwtnao,Iwtnbo,Iznuc
      integer j,jcnt,Jcore,Jorder,Jprint,jptr,k,kflg,Kopt,kptr,Label,Lar
     &c,lcr,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo
      integer Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lf
     &npnl,Lfnppa,Lfnpr,Ll,lla,lry,Lstocc,Ltyp,MAXATM,MAXBAS,Munit,Mxao
      integer Mxaolm,Mxbo,N3ctr,na1,nab,nam,name,nameat,Naoctr,Naol,Nato
     &ms,naugm,nb,Nbas,Nbotyp,Nbouni,nctr,Ndim,nel,nexlp
      integer nmb,nocc,nopval,norb,Norbs,nstart,Ntopo
      
      
      
      
      logical detail,nobond,first
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      integer Ul
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),U
     &l(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbnao/Naoctr(MAXBAS),Naol(MAXBAS),Ltyp(MAXBAS),Iprin(MAXBAS
     &)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbthr/Thrset,Prjset,Accthr,Crtset,E2thr,Athr,Pthr,Ethr,Dthr
      common/nbtopo/Iorder(MAXATM),Jorder(MAXATM),Ntopo(MAXATM,MAXATM),N
     &3ctr,I3ctr(10,3)
      dimension DM(Ndim,Ndim),T(Ndim,Ndim),V(Ndim),BORB(Mxbo),POL(Ndim,3
     &),BNDOCC(Ndim),name(3),hybexp(3),Q(Mxao,Ndim),BLK(Mxbo,Mxbo),EVAL(
     &Mxbo),C(Mxbo,Mxbo),P(Mxao,Mxao),TA(Mxao,Mxao),HYB(Mxao),VA(Mxao),V
     &B(Mxao),GUIDE(Natoms,Natoms),TOPO(Natoms*Natoms)
      data gthrsh/1.5D-1/
      data istar,iblnk/'*',' '/
      data name/'LP','BD','3C'/
      data lry,lcr/'RY','CR'/
      data zero,zerop,tenth,one,two,four/0.D0,1.D-5,0.1D0,1.0D0,2.0D0,4.
     &0D0/
      data twop/2.0001D0/
      data pt8,pt99/0.8D0,0.99D0/
      
      
      data prjinc/0.05D0/
      
      nopval(i)=Norbs(i)-Ino(i)
      
      detail=.FALSE.
      if(Iwdetl.NE.0)detail=.TRUE.
      nobond=.FALSE.
      if(Jprint(10).NE.0)nobond=.TRUE.
      
      
      prjthr=abs(Prjset)
      thresh=abs(Thrset)
      if(Ispin.NE.0)thresh=thresh-one
      if(nobond)thresh=one
      if(nobond.AND.(Ispin.NE.0))thresh=one/two
      if(Ispin.NE.0)gthrsh=gthrsh/four
      
      
      if(Natoms.EQ.1)then
      Iorder(1)=1
      goto 500
      endif
      
      
      gmax=zero
      do 100 j=2,Natoms
      do 50 i=1,j-1
      if(GUIDE(i,j).GT.gmax)then
      gmax=GUIDE(i,j)
      iat=i
      endif
50    continue
100   continue
      Iorder(1)=iat
      
      
      icnt=1
      inxt=icnt
      jcnt=icnt
200   iptr=inxt
      i1st=1
      do 300 i=1,Natoms
      TOPO(i)=GUIDE(i,Iorder(iptr))
300   continue
      call rank(TOPO,Natoms,Natoms,Jorder)
      jptr=1
400   if(TOPO(jptr).GT.pt8)then
      iflg=1
      do 450 i=1,icnt
      if(Iorder(i).EQ.Jorder(jptr))iflg=0
450   continue
      if(iflg.EQ.1)then
      icnt=icnt+1
      Iorder(icnt)=Jorder(jptr)
      if(i1st.EQ.1)then
      i1st=0
      inxt=icnt
      endif
      endif
      else
      
      if(i1st.EQ.1)then
      jcnt=jcnt+1
      inxt=jcnt
      if(inxt.GT.Natoms)goto 500
      if(inxt.GT.icnt)then
      kptr=0
460   kptr=kptr+1
      kflg=1
      do 470 i=1,icnt
      if(Iorder(i).EQ.kptr)kflg=0
470   continue
      if(kflg.EQ.0)goto 460
      icnt=icnt+1
      Iorder(icnt)=kptr
      endif
      endif
      goto 200
      endif
      jptr=jptr+1
      goto 400
      
500   iter=0
      ialarm=0
600   if(ialarm.EQ.0)iter=iter+1
      
      
      do 700 j=1,Nbas
      do 650 i=1,j
      T(i,j)=DM(i,j)
650   continue
700   continue
      
      
      do 1000 i=1,Nbas
      do 750 k=1,2
      Label(i,k)=iblnk
750   continue
      do 800 k=3,6
      Label(i,k)=0
800   continue
      do 850 k=1,3
      POL(i,k)=zero
      Iathy(i,k)=0
850   continue
      do 900 k=1,Mxao
      Q(k,i)=zero
900   continue
1000  continue
      do 1100 i=1,Natoms
      Ino(i)=0
1100  continue
      
      
      ibd=0
      call core(DM,T,BORB,POL,Q,HYB,BNDOCC,ibd,detail,Lfnpr)
      
      
      occmx=thresh
      
      
      na1=Natoms+1
      do 1200 ia1=1,na1
      ia=ia1-1
      if((ia.LE.0).OR.(nopval(Iorder(ia)).GT.0))then
      do 1120 ib1=1,na1
      ib=ib1-1
      if((ib.LE.0).OR.(nopval(Iorder(ib)).GT.0))then
      do 1110 ic1=2,na1
      ic=ic1-1
      if((ic.LE.0).OR.(nopval(Iorder(ic)).GT.0))then
      if(ia.NE.0)then
      
      
      if(Iw3c.NE.1)goto 1300
      nctr=3
      iat1=Iorder(ia)
      iat2=Iorder(ib)
      iat3=Iorder(ic)
      if(iat1.GE.iat2)goto 1120
      if(iat2.GE.iat3)goto 1110
      if(GUIDE(iat1,iat2).LE.gthrsh)then
      if(GUIDE(iat1,iat3).LE.gthrsh)then
      if(GUIDE(iat2,iat3).LE.gthrsh)goto 1110
      endif
      endif
      elseif(ib.NE.0)then
      
      
      if(nobond)goto 1110
      nctr=2
      iat1=Iorder(ib)
      iat2=Iorder(ic)
      iat3=0
      if(iat2.LE.iat1)goto 1110
      if(GUIDE(iat1,iat2).LT.gthrsh)goto 1110
      else
      
      
      nctr=1
      iat1=Iorder(ic)
      iat2=0
      iat3=0
      endif
      
      
      if(iwprj(nctr).NE.0)call deplet(DM,T,Q,POL,BORB,BNDOCC,ibd)
      
      
      call load(DM,iat1,iat2,iat3,BLK,nb)
      
      
      call jacobi(nb,BLK,EVAL,C,Mxbo,Mxbo,1)
      
      
      call rank(EVAL,nb,Mxbo,Larc)
      if(detail)write(Lfnpr,99005)iat1,iat2,iat3
      if(detail)write(Lfnpr,99006)thresh
      if(detail)write(Lfnpr,99007)(EVAL(irnk),irnk=1,nb)
      iaccep=0
      do 1108 irnk=1,nb
      ir=Larc(irnk)
      occ=EVAL(irnk)
      do 1102 i=1,nb
      BORB(i)=C(i,ir)
1102  continue
      if(detail)write(Lfnpr,99008)irnk,occ
      if(detail)write(Lfnpr,99009)(BORB(i),i=1,nb)
      
      
      if(occ.LT.occmx)goto 1110
      
      
      if(nctr.NE.1)then
      call prjexp(BORB,iat1,iat2,iat3,Q,P,TA,HYB,VA,VB,hybexp)
      if(detail)then
      do 1104 ihyb=1,nctr
      write(Lfnpr,99010)ihyb,hybexp(ihyb)
1104  continue
      endif
      do 1106 ihyb=1,nctr
      if(hybexp(ihyb).LT.prjthr)goto 1108
1106  continue
      endif
      ibd=ibd+1
      iaccep=iaccep+1
      
      
      call stash(BORB,ibd,iat1,iat2,iat3,POL,Q,HYB)
      
      
      Label(ibd,1)=name(nctr)
      Label(ibd,2)=iblnk
      Label(ibd,3)=iaccep
      Label(ibd,4)=iat1
      Label(ibd,5)=iat2
      Label(ibd,6)=iat3
      BNDOCC(ibd)=occ
      if(detail)write(Lfnpr,99011)ibd,(Label(ibd,i),i=1,3)
1108  continue
      endif
1110  continue
      endif
1120  continue
      endif
1200  continue
      
      
1300  call orthyb(Q,BLK,TA,EVAL,C,ialarm,0)
      
      
      if(ialarm.NE.0)then
      oldprj=prjthr
      prjthr=oldprj+prjinc
      if(Jprint(5).NE.0)write(Lfnpr,99012)ialarm,oldprj,prjthr
      if(prjthr.GE.pt99)then
      write(Lfnpr,99013)ialarm
      Jprint(1)=-1
      return
      endif
      goto 1600
      endif
      
      
      do 1400 ia=1,Natoms
      if(nopval(ia).GT.0)then
      
      
      lla=Ll(ia)
      iula=Ul(ia)
      nmb=0
      do 1320 i=lla,iula
      if(Lstocc(i).EQ.1)nmb=nmb+1
1320  continue
      
      
      iocc=0
      iocclp=0
      do 1340 ib=1,ibd
      if((Label(ib,4).EQ.ia).OR.(Label(ib,5).EQ.ia).OR.(Label(ib,6).EQ.i
     &a))then
      iocc=iocc+1
      if(Label(ib,1).EQ.name(1))iocclp=iocclp+1
      endif
1340  continue
      
      
      nexlp=nmb-iocc
      if(nexlp.LT.0)nexlp=0
      
      nocc=Ino(ia)
      call frmpro(P,ia,Q,nocc,TA,VA,VB)
      norb=Norbs(ia)
      naugm=norb-nocc
      call augmnt(P,BLK,C,EVAL,DM,TA,BORB,V,Larc,ia,nocc,norb)
      
      
      do 1360 iaugm=1,nexlp
      do 1350 j=1,norb
      BORB(j)=BLK(j,iaugm)
1350  continue
      ibd=ibd+1
      call stash(BORB,ibd,ia,0,0,POL,Q,HYB)
      Label(ibd,1)=name(1)
      Label(ibd,2)=iblnk
      Label(ibd,3)=iaugm+iocclp
      Label(ibd,4)=ia
      Label(ibd,5)=0
      Label(ibd,6)=0
1360  continue
      
      
      iryd=0
      nstart=nexlp+1
      do 1380 iaugm=nstart,naugm
      do 1370 j=1,norb
      BORB(j)=BLK(j,iaugm)
1370  continue
      ibd=ibd+1
      iryd=iryd+1
      call stash(BORB,ibd,ia,0,0,POL,Q,HYB)
      Label(ibd,1)=lry
      Label(ibd,2)=istar
      Label(ibd,3)=iryd
      Label(ibd,4)=ia
      Label(ibd,5)=0
      Label(ibd,6)=0
1380  continue
      endif
1400  continue
      
      
      ibo=ibd
      do 1500 i=1,ibo
      
      
      if(Label(i,1).NE.name(1))then
      if(Label(i,1).NE.lry)then
      if(Label(i,1).NE.lcr)then
      nab=1
      if(Label(i,1).EQ.name(3))nab=2
      do 1405 iab=1,nab
      ibd=ibd+1
      do 1402 j=1,6
      Label(ibd,j)=Label(i,j)
1402  continue
      Label(ibd,2)=istar
1405  continue
      endif
      endif
      endif
1500  continue
      
      
1600  do 1700 j=1,Nbas
      do 1650 i=1,j
      DM(i,j)=T(i,j)
      DM(j,i)=DM(i,j)
      T(j,i)=zero
      T(i,j)=zero
1650  continue
1700  continue
      
      
      if(ialarm.NE.0)goto 600
      
      
      if(ibd.NE.Nbas)then
      write(Lfnpr,99001)thresh,ibd,Nbas
      write(Lfnpr,99002)(i,(Label(i,j),j=1,6),i=1,ibd)
      stop
      endif
      
      
      call repol(DM,Q,POL,BLK,EVAL,C,ibd)
      
      
      call formt(T,Q,POL)
      
      
      totele=zero
      do 1800 i=1,Nbas
      occi=zero
      do 1750 j=1,Nbas
      do 1720 k=1,Nbas
      occi=occi+T(j,i)*DM(j,k)*T(k,i)
1720  continue
1750  continue
      if(dabs(occi).LT.zerop)occi=zero
      if(occi.GT.twop)goto 2000
      zeropm=-zerop
      if(occi.LT.zeropm)goto 2000
      BNDOCC(i)=occi
      V(i)=occi
      totele=totele+BNDOCC(i)
1800  continue
      nel=totele+tenth
      if(abs(totele-nel).GT.1E-4)then
      
      
      write(Lfnpr,99004)totele
      Jprint(1)=-1
      return
      
99001 format(/,1x,'For an occupancy threshold of ',f4.2,' the search',' 
     &for NBOs found',/,1x,i3,' orbitals orbitals rather than ',i4)
99002 format(3x,'Label ',i3,':',a3,a1,i2,3I3)
99003 format(/,1x,'A bond orbital with an occupancy of ',f8.5,' electron
     &s was found!',/,1x,'Please check you input data.')
99004 format(/,1x,'The total number of electron is not an integer:',f10.
     &5,/,1x,'Please check your input data.')
99005 format(/,1x,'Search of DM block between the following atoms:',3I4)
99006 format(6x,'Select orbitals with eigenvalue > ',f9.6)
99007 format(6x,8F9.6)
99008 format(6x,'Eigenvector (',i2,') has occupancy ',f9.6,':')
99009 format(11x,8F7.4)
99010 format(11x,'Hybrid ',i1,' in eigenvector has a projection ','expec
     &tation of ',f6.3)
99011 format(11x,'*** NBO accepted: Number',i3,'.   Label:',a2,a1,'(',i2
     &,')')
99012 format(/4x,'The hybrids found for atom ',i2,' are linearly ','depe
     &ndent.  Remedy: PRJTHR',/,4x,'will be raised from',f6.3,' to',f6.3
     &,' and the NBO search repeated',/)
99013 format(//,1x,'Linearly independent hybrids for atom',i3,' cannot b
     &e found.',/,1x,'The NBO program must abort.')
99014 format(/,1x,'WARNING:',i3,' low occupancy (<',f6.4,'e) core orbita
     &ls ','found on ',a2,i2)
99015 format(/,1x,'WARNING:',i3,' low occupancy (<',f6.4,'e) core orbita
     &l  ','found on ',a2,i2)
99016 format(1x,'        ',i3,' low occupancy (<',f6.4,'e) core orbitals
     & ','found on ',a2,i2)
99017 format(1x,'        ',i3,' low occupancy (<',f6.4,'e) core orbital 
     & ','found on ',a2,i2)
      else
      totele=nel
      nocc=nel
      if(Ispin.EQ.0)nocc=nocc/2+mod(nocc,2)
      
      
      call rank(V,Nbas,Ndim,Larc)
      do 1850 i=1,nocc
      ir=Larc(i)
      Label(Ibxm(ir),2)=iblnk
1850  continue
      do 1900 i=nocc+1,Nbas
      ir=Larc(i)
      Label(Ibxm(ir),2)=istar
1900  continue
      
      
      call cycles(iter,thresh,GUIDE,BNDOCC,TOPO,icont)
      if(icont.EQ.0)then
      Jprint(1)=-1
      return
      endif
      if(icont.EQ.-1)goto 600
      if(icont.EQ.1)goto 600
      
      
      crthrs=Crtset
      if(Ispin.NE.0)crthrs=crthrs-one
      first=.TRUE.
      do 1950 iat=1,Natoms
      ilow=0
      do 1920 i=1,Nbas
      if(Label(Ibxm(i),1).EQ.lcr.AND.Label(Ibxm(i),4).EQ.iat.AND.BNDOCC(
     &i).LT.crthrs)ilow=ilow+1
1920  continue
      if(ilow.NE.0)then
      if(first)then
      first=.FALSE.
      nam=nameat(Iatno(iat))
      if(ilow.NE.1)then
      if(Jprint(5).EQ.1)write(Lfnpr,99014)ilow,crthrs,nam,iat
      else
      if(Jprint(5).EQ.1)write(Lfnpr,99015)ilow,crthrs,nam,iat
      endif
      else
      nam=nameat(Iatno(iat))
      if(ilow.NE.1)then
      if(Jprint(5).EQ.1)write(Lfnpr,99016)ilow,crthrs,nam,iat
      else
      if(Jprint(5).EQ.1)write(Lfnpr,99017)ilow,crthrs,nam,iat
      endif
      endif
      endif
1950  continue
      return
      endif
      
      
2000  write(Lfnpr,99003)occi
      Jprint(1)=-1
      return
      end
C* :1 * 
      
