
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 choose"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "choose.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 40 "choose.web"
      
      
      
      subroutine choose(DM,T,GUIDE,BNDOCC,POL,Q,V,BLK,C,EVAL,BORB,P,TA,H
     &YB,VA,VB,TOPO,IFLG)
      implicit none
      double precision Accthr,Athr,BLK,BNDOCC,BORB,C,crthrs,Crtset,DM,Dt
     &hr,E2thr,Ethr,EVAL,GUIDE,HYB,hybexp,occ,occi,occmax,occthr
      double precision oldprj,one,P,POL,prjinc,Prjset,prjthr,pt99,Pthr,Q
     &,scr,T,TA,tenth,Thrset,TOPO,totele,two,twop,V
      double precision VA,VB,zero,zerop,zeropm
      integer i,I3ctr,ia,iab,iaccep,ialarm,iat,iat1,iat2,iat3,Iatcr,Iath
     &y,Iatno,iaugm,ib,ibd,iblnk,ibo,Ibxm,Ichoos
      integer icntr,icont,IFLG,ihyb,ii,ilow,Ino,iocc,iocclp,ione,Iorder,
     &ipos,Iprin,Iprint,Ipseud,ipt,ir,irnk,iryd,ishift
      integer Ispin,istar,iter,iula,Iw3c,Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfoc
     &k,Iwhybs,Iwmulp,Iwpnao,Iwtnab,Iwtnao,Iwtnbo,Iznuc,j,jat,jcntr
      integer Jcore,Jorder,jpos,Jprint,k,kflg,Kopt,ktopo,Label,Larc,lcr,
     &Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao
      integer Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lf
     &npr,Ll,lla,lry,Lstocc,Ltyp,MAXATM,MAXBAS,mflg,Munit,Mxao,Mxaolm
      integer Mxbo,N3ctr,nab,nam,name,nameat,Naoctr,Naol,Natoms,naugm,nb
     &,Nbas,Nbotyp,Nbouni,nctr,Ndim,nel,nexlp,nmb,nn
      integer nocc,nopval,norb,Norbs,nostr,nstart,Ntopo,num
      
      
      logical detail,first,print,left
      integer Ul
      parameter(MAXATM=99,MAXBAS=500)
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
      
      dimension DM(Ndim,Ndim),T(Ndim,Ndim),GUIDE(Natoms,Natoms),BNDOCC(N
     &dim),POL(Ndim,3),Q(Mxao,Ndim),V(Ndim),BLK(Mxbo,Mxbo),C(Mxbo,Mxbo),
     &EVAL(Mxbo),BORB(Mxbo),P(Mxao,Mxao),TA(Mxao,Mxao),HYB(Mxao),VA(Mxao
     &),VB(Mxao),TOPO(Natoms,Natoms)
      dimension name(3),hybexp(3),ktopo(MAXATM,MAXATM),kflg(10)
      dimension scr(MAXATM*(MAXATM-1)/2),ipt(MAXATM*(MAXATM-1)/2)
      
      data istar,iblnk,name,lry,lcr/'*',' ','LP','BD','3C','RY','CR'/
      data zero,zerop,tenth,pt99,one,two,twop/0.0D0,1.0D-5,0.1D0,0.99D0,
     &1.0D0,2.0D0,2.0001D0/
      
      
      
      data prjinc/0.05D0/
      
      nopval(i)=Norbs(i)-Ino(i)
      
      print=.FALSE.
      if(IFLG.EQ.0)print=.TRUE.
      if(Jprint(5).EQ.0)print=.FALSE.
      detail=.FALSE.
      if(Iwdetl.NE.0)detail=.TRUE.
      prjthr=abs(Prjset)
      iter=0
      
      
      do 100 i=1,Natoms
      do 50 j=1,i
      ktopo(i,j)=Ntopo(i,j)
      ktopo(j,i)=Ntopo(j,i)
50    continue
100   continue
      do 200 i=1,N3ctr
      kflg(i)=1
200   continue
      
      
      if(Natoms.EQ.1)then
      Iorder(1)=1
      else
      ii=0
      do 250 jat=2,Natoms
      do 220 iat=1,jat-1
      ii=ii+1
      scr(ii)=ktopo(iat,jat)-GUIDE(iat,jat)
220   continue
250   continue
      nn=Natoms*(Natoms-1)/2
      call rank(scr,nn,nn,ipt)
      
      
      ipos=0
      jpos=0
300   jpos=jpos+1
      if(jpos.GT.nn)stop 'Problems with atom permutation list'
      iat=ipt(jpos)
      jat=2
350   if(jat.GT.iat)then
      
      
      mflg=0
      do 360 i=1,ipos
      if(Iorder(i).EQ.iat)mflg=1
360   continue
      if(mflg.EQ.0)then
      ipos=ipos+1
      Iorder(ipos)=iat
      endif
      mflg=0
      do 380 i=1,ipos
      if(Iorder(i).EQ.jat)mflg=1
380   continue
      if(mflg.EQ.0)then
      ipos=ipos+1
      Iorder(ipos)=jat
      endif
      if(ipos.LT.Natoms)goto 300
      else
      iat=iat-jat+1
      jat=jat+1
      goto 350
      endif
      endif
      
      
400   iter=iter+1
      occthr=two
      if(Ispin.NE.0)occthr=one
      
      
      do 500 j=1,Nbas
      do 450 i=1,j
      T(i,j)=DM(i,j)
450   continue
500   continue
      
      
      do 800 i=1,Nbas
      do 550 k=1,2
      Label(i,k)=iblnk
550   continue
      do 600 k=3,6
      Label(i,k)=0
600   continue
      do 650 k=1,3
      POL(i,k)=zero
      Iathy(i,k)=0
650   continue
      do 700 k=1,Mxao
      Q(k,i)=zero
700   continue
800   continue
      do 900 i=1,Natoms
      Ino(i)=0
900   continue
      
      
      ibd=0
      call core(DM,T,BORB,POL,Q,HYB,BNDOCC,ibd,detail,Lfnpr)
      
      
1000  occthr=occthr-tenth
      left=.FALSE.
      
      
      
      nctr=0
1100  nctr=nctr+1
      
      
      if(nctr.NE.1)call deplet(DM,T,Q,POL,BORB,BNDOCC,ibd)
      
      icntr=0
1200  icntr=icntr+1
      if(nctr.EQ.1)then
      if(icntr.GT.Natoms)goto 1100
      num=ktopo(Iorder(icntr),Iorder(icntr))
      if(num.LE.0)goto 1200
      iat1=Iorder(icntr)
      iat2=0
      iat3=0
      elseif(nctr.EQ.2)then
      if(icntr.GT.Natoms)goto 1100
      jcntr=icntr
1250  jcntr=jcntr+1
      if(jcntr.GT.Natoms)goto 1200
      num=ktopo(Iorder(jcntr),Iorder(icntr))
      if(num.EQ.0)goto 1250
      iat1=Iorder(icntr)
      iat2=Iorder(jcntr)
      iat3=0
      elseif(nctr.EQ.3)then
      if(icntr.GT.N3ctr)goto 1100
      if(kflg(icntr).EQ.0)goto 1200
      num=1
      iat1=I3ctr(icntr,1)
      iat2=I3ctr(icntr,2)
      iat3=I3ctr(icntr,3)
      else
      
      
      
      
      if(left)then
      occthr=occmax
      goto 1000
      endif
      
      
      call orthyb(Q,BLK,TA,EVAL,C,ialarm,IFLG)
      
      
      if(ialarm.NE.0)then
      oldprj=prjthr
      prjthr=oldprj+prjinc
      if(print)write(Lfnpr,99011)ialarm,oldprj,prjthr
      if(prjthr.GE.pt99)then
      if(print)write(Lfnpr,99012)ialarm
      IFLG=-1
      Jprint(1)=-1
      return
      endif
      goto 1900
      endif
      
      
      do 1300 ia=1,Natoms
      if(nopval(ia).GT.0)then
      
      
      lla=Ll(ia)
      iula=Ul(ia)
      nmb=0
      do 1260 i=lla,iula
      if(Lstocc(i).EQ.1)nmb=nmb+1
1260  continue
      
      
      iocc=0
      iocclp=0
      do 1270 ib=1,ibd
      if((Label(ib,4).EQ.ia).OR.(Label(ib,5).EQ.ia).OR.(Label(ib,6).EQ.i
     &a))then
      iocc=iocc+1
      if(Label(ib,1).EQ.name(1))iocclp=iocclp+1
      endif
1270  continue
      
      
      nexlp=nmb-iocc
      if(nexlp.LT.0)nexlp=0
      nocc=Ino(ia)
      call frmpro(P,ia,Q,nocc,TA,VA,VB)
      norb=Norbs(ia)
      naugm=norb-nocc
      call augmnt(P,BLK,C,EVAL,DM,TA,BORB,V,Larc,ia,nocc,norb)
      
      
      do 1280 iaugm=1,nexlp
      do 1275 j=1,norb
      BORB(j)=BLK(j,iaugm)
1275  continue
      ibd=ibd+1
      call stash(BORB,ibd,ia,0,0,POL,Q,HYB)
      Label(ibd,1)=name(1)
      Label(ibd,2)=istar
      Label(ibd,3)=iaugm+iocclp
      Label(ibd,4)=ia
      Label(ibd,5)=0
      Label(ibd,6)=0
1280  continue
      
      
      iryd=0
      nstart=nexlp+1
      do 1290 iaugm=nstart,naugm
      do 1285 j=1,norb
      BORB(j)=BLK(j,iaugm)
1285  continue
      ibd=ibd+1
      iryd=iryd+1
      call stash(BORB,ibd,ia,0,0,POL,Q,HYB)
      Label(ibd,1)=lry
      Label(ibd,2)=istar
      Label(ibd,3)=iryd
      Label(ibd,4)=ia
      Label(ibd,5)=0
      Label(ibd,6)=0
1290  continue
      endif
1300  continue
      
      
      ibo=ibd
      do 1350 i=1,ibo
      
      
      if(Label(i,1).NE.name(1))then
      if(Label(i,1).NE.lry)then
      if(Label(i,1).NE.lcr)then
      nab=1
      if(Label(i,1).EQ.name(3))nab=2
      do 1304 iab=1,nab
      ibd=ibd+1
      do 1302 j=1,6
      Label(ibd,j)=Label(i,j)
1302  continue
      Label(ibd,2)=istar
1304  continue
      endif
      endif
      endif
1350  continue
      if(ibd.EQ.Nbas)goto 1900
      write(Lfnpr,99015)
      stop
      endif
      
      
1400  call load(DM,iat1,iat2,iat3,BLK,nb)
      call jacobi(nb,BLK,EVAL,C,Mxbo,Mxbo,1)
      
      
      call rank(EVAL,nb,Mxbo,Larc)
      if(detail)write(Lfnpr,99003)iat1,iat2,iat3
      if(detail)write(Lfnpr,99004)num,occthr
      if(detail)write(Lfnpr,99005)(EVAL(irnk),irnk=1,nb)
      
      
      iaccep=0
      do 1600 irnk=1,nb
      ir=Larc(irnk)
      occ=EVAL(irnk)
      do 1450 i=1,nb
      BORB(i)=C(i,ir)
1450  continue
      if(detail)write(Lfnpr,99006)irnk,occ
      if(detail)write(Lfnpr,99007)(BORB(i),i=1,nb)
      
      
      if(occ.LT.occthr)then
      if(nctr.EQ.1)then
      ktopo(iat1,iat1)=num-iaccep
      if(detail)write(Lfnpr,99010)ktopo(iat1,iat1)
      elseif(nctr.EQ.2)then
      ktopo(iat1,iat2)=num-iaccep
      ktopo(iat2,iat1)=ktopo(iat1,iat2)
      if(detail)write(Lfnpr,99010)ktopo(iat1,iat2)
      else
      ione=1
      if(detail)write(Lfnpr,99010)ione
      endif
      if(left)then
      if(occmax.LT.occ)occmax=occ
      else
      left=.TRUE.
      occmax=occ
      endif
      goto 1700
      endif
      
      
      call prjexp(BORB,iat1,iat2,iat3,Q,P,TA,HYB,VA,VB,hybexp)
      if(detail)then
      do 1460 ihyb=1,nctr
      write(Lfnpr,99008)ihyb,hybexp(ihyb)
1460  continue
      endif
      do 1500 ihyb=1,nctr
      if(hybexp(ihyb).LT.prjthr)goto 1600
1500  continue
      ibd=ibd+1
      iaccep=iaccep+1
      
      
      call stash(BORB,ibd,iat1,iat2,iat3,POL,Q,HYB)
      
      
      if(nctr.EQ.1)then
      ishift=Ntopo(iat1,iat1)-ktopo(iat1,iat1)
      elseif(nctr.EQ.2)then
      ishift=Ntopo(iat1,iat2)-ktopo(iat1,iat2)
      else
      ishift=0
      endif
      Label(ibd,1)=name(nctr)
      Label(ibd,2)=iblnk
      Label(ibd,3)=iaccep+ishift
      Label(ibd,4)=iat1
      Label(ibd,5)=iat2
      Label(ibd,6)=iat3
      BNDOCC(ibd)=occ
      if(detail)write(Lfnpr,99009)ibd,(Label(ibd,i),i=1,3)
      if(iaccep.EQ.num)then
      if(nctr.EQ.1)then
      ktopo(iat1,iat1)=0
      elseif(nctr.EQ.2)then
      ktopo(iat1,iat2)=0
      ktopo(iat2,iat1)=0
      else
      kflg(icntr)=0
      endif
      goto 1700
      endif
1600  continue
      if(iaccep.NE.num.AND.nctr.EQ.2.AND.print)write(Lfnpr,99013)prjthr,
     &iaccep,num,iat1,iat2
      if(iaccep.NE.num.AND.nctr.EQ.3.AND.print)write(Lfnpr,99014)prjthr,
     &iaccep,num,iat1,iat2,iat3
      IFLG=-1
1700  if(nctr.EQ.1.OR.nctr.EQ.3)goto 1200
1800  jcntr=jcntr+1
      if(jcntr.GT.Natoms)goto 1200
      num=ktopo(Iorder(jcntr),Iorder(icntr))
      if(num.EQ.0)goto 1800
      iat1=Iorder(icntr)
      iat2=Iorder(jcntr)
      iat3=0
      goto 1400
      
      
1900  do 2000 j=1,Nbas
      do 1950 i=1,j
      DM(i,j)=T(i,j)
      DM(j,i)=DM(i,j)
      T(j,i)=zero
      T(i,j)=zero
1950  continue
2000  continue
      
      
      if(ialarm.NE.0)goto 400
      
      
      call repol(DM,Q,POL,BLK,EVAL,C,ibd)
      
      
      call formt(T,Q,POL)
      
      
      totele=zero
      do 2100 i=1,Nbas
      occi=zero
      do 2050 j=1,Nbas
      do 2020 k=1,Nbas
      occi=occi+T(j,i)*DM(j,k)*T(k,i)
2020  continue
2050  continue
      if(dabs(occi).LT.zerop)occi=zero
      if(occi.GT.twop)goto 2300
      zeropm=-zerop
      if(occi.LT.zeropm)goto 2300
      BNDOCC(i)=occi
      V(i)=occi
      totele=totele+BNDOCC(i)
2100  continue
      nel=totele+tenth
      if(abs(totele-nel).GT.1E-4)then
      
      
      write(Lfnpr,99002)totele
      IFLG=-1
      Jprint(1)=-1
      return
      
99001 format(/,1x,'A bond orbital with an occupancy of ',f8.5,' electron
     &s was found!',/,1x,'Please check you input data.')
99002 format(/,1x,'The total number of electron is not an integer:',f10.
     &5,/,1x,'Please check your input data.')
99003 format(/,1x,'Search of DM block between the following atoms:',3I4)
99004 format(6x,'Select ',i2,' orbital(s) with eigenvalue > ',f9.6)
99005 format(6x,8F9.6)
99006 format(6x,'Eigenvector (',i2,') has occupancy ',f9.6,':')
99007 format(11x,8F7.4)
99008 format(11x,'Hybrid ',i1,' in eigenvector has a projection ','expec
     &tation of ',f6.3)
99009 format(11x,'*** NBO accepted: Number',i3,'.   Label:',a2,a1,'(',i2
     &,')')
99010 format(1x,'Still need to find',i2,' more orbital(s)')
99011 format(/4x,'The hybrids found for atom ',i2,' are linearly ','depe
     &ndent.  Remedy: PRJTHR',/,4x,'will be raised from',f6.3,' to',f6.3
     &,' and the NBO search repeated',/)
99012 format(//,1x,'Linearly independent hybrids for atom',i3,' cannot b
     &e found.',/,1x,'The NBO program must abort.')
99013 format(/,1x,'At a projection threshold of',f6.3,', only ',i1,' of 
     &the ',i1,' requested bonds',/,1x,'between atoms ',i2,' and ',i2,' 
     &can be constructed.  The NBO analysis will',/,1x,'continue, augmen
     &ting the NBO set with extra lone pairs ','on the atoms',/,1x,'as n
     &ecessary.')
99014 format(/,1x,'At a projection threshold of',f6.3,', only ',i1,' of 
     &the ',i1,' requested bonds',/,1x,'between atoms ',i2,', ',i2,', an
     &d ',i2,' can be constructed.  The NBO analysis',/,1x,'will continu
     &e, augmenting the NBO set with extra lone pairs ','on the',/,1x,'a
     &toms as necessary.')
99015 format(/,1x,'Miscounted orbitals, program must abort')
99016 format(/,1x,'WARNING:',i3,' low occupancy (<',f6.4,'e) core orbita
     &ls ','found on ',a2,i2)
99017 format(/,1x,'WARNING:',i3,' low occupancy (<',f6.4,'e) core orbita
     &l  ','found on ',a2,i2)
99018 format(1x,'        ',i3,' low occupancy (<',f6.4,'e) core orbitals
     & ','found on ',a2,i2)
99019 format(1x,'        ',i3,' low occupancy (<',f6.4,'e) core orbital 
     & ','found on ',a2,i2)
      else
      totele=nel
      nocc=nel
      if(Ispin.EQ.0)nocc=nocc/2+mod(nocc,2)
      
      
      nostr=0
      do 2150 i=1,Nbas
      if(Label(Ibxm(i),2).NE.istar)nostr=nostr+1
2150  continue
      if(nostr.NE.nocc)then
      call rank(V,Nbas,Ndim,Larc)
      do 2160 i=1,nocc
      ir=Larc(i)
      Label(Ibxm(ir),2)=iblnk
2160  continue
      do 2180 i=nocc+1,Nbas
      ir=Larc(i)
      Label(Ibxm(ir),2)=istar
2180  continue
      endif
      
      
      call cycles(iter,abs(Thrset),GUIDE,BNDOCC,TOPO,icont)
      
      
      if(print)then
      crthrs=Crtset
      if(Ispin.NE.0)crthrs=crthrs-one
      first=.TRUE.
      do 2200 iat=1,Natoms
      ilow=0
      do 2190 i=1,Nbas
      if(Label(Ibxm(i),1).EQ.lcr.AND.Label(Ibxm(i),4).EQ.iat.AND.BNDOCC(
     &i).LT.crthrs)ilow=ilow+1
2190  continue
      if(ilow.NE.0)then
      if(first)then
      first=.FALSE.
      nam=nameat(Iatno(iat))
      if(ilow.NE.1)then
      write(Lfnpr,99016)ilow,crthrs,nam,iat
      else
      write(Lfnpr,99017)ilow,crthrs,nam,iat
      endif
      else
      nam=nameat(Iatno(iat))
      if(ilow.NE.1)then
      write(Lfnpr,99018)ilow,crthrs,nam,iat
      else
      write(Lfnpr,99019)ilow,crthrs,nam,iat
      endif
      endif
      endif
2200  continue
      endif
      return
      endif
      
      
2300  if(print)write(Lfnpr,99001)occi
      IFLG=-1
      Jprint(1)=-1
      return
      end
C* :1 * 
      
