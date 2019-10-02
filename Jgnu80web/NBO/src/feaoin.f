
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 feaoin"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "feaoin.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "feaoin.web"
      
      
      
      
      
      
      
      
      subroutine feaoin(CORE,ICORE,NBOOPT)
      implicit none
      double precision Atmchg,C,C1,C2,C3,C4,Clp,CORE,crit,eps,Exx,toang,
     &X,xa,xc,Y,ya,yc,Z,za
      double precision zc,zero,Zlp
      integer i,i5d6d,i7f10f,iaind,Ian,Iatcr,Iatno,iatom,ibas,Icharg,Ich
     &oos,icnstr,ICORE,iexist,ihold,ii,imax,Ino,Iprint,Ipseud
      integer iptr,irohf,iscf,ishell,Ispin,itqry,Iw3c,Iwapol,Iwcubf,Iwde
     &tl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao,Iwtnab,Iwtnao,Iwtnbo,Iznuc,j
      integer Jan,Jcore,Jprint,kd,kf,Kfirst,Klast,Kopt,kp,ks,l2,l3,Lang,
     &Lctr,len,LENB,Lfnao,Lfnarc,Lfndaf,Lfndef
      integer Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpn
     &a,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,listd,listf,listp,lists,Ll,Lma
     &x
      integer Lpskip,lptr,Lu,MAXATM,MAXBAS,maxl,MAXPRM,MAXS21,MAXSH1,MAX
     &SHL,Maxtyp,mptr,Multip,Munit,Mxao,Mxaolm,Mxbo,Nae,Natom,Natoms
      integer Nbas,Nbasis,Nbe,NBOOPT,nctr,Ndim,Ne,nexp,nfile,Nfroz,Nlp,N
     &orbs,Nshell
      dimension CORE(*),ICORE(*),NBOOPT(10)
      dimension listp(3),listd(6,2),listf(10,2)
      
      data lists/1/
      data listp/101,102,103/
      data listd/255,252,253,254,251,0,201,204,206,202,203,205/
      data listf/351,352,353,354,355,356,357,0,0,0,301,307,310,304,302,3
     &03,306,309,308,305/
      
      
      
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbao/Lctr(MAXBAS),Lang(MAXBAS)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      
      
      common/mol/Natom,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(1
     &00),C(300)
      
      common/lp2/Nlp(400),Clp(400),Zlp(400),Kfirst(35,5),Klast(35,5),Lma
     &x(35),Lpskip(35),Nfroz(35)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      data zero/0.0D0/
      data toang/0.529177249/
      
      
      len=600
      call tread(502,ICORE,len,1,len,1,0)
      nfile=2
      call nbwrit(ICORE(1001),10,nfile)
      
      
      len=0
      iexist=itqry(512)
      if(iexist.GT.0)then
      len=1210
      call tread(512,Nlp,len,1,len,1,0)
      endif
      Natoms=Natom
      do 100 i=1,Natoms
      Iatno(i)=Ian(i)
      eps=0.001D00
      Nfroz(i)=Ian(i)-int(Atmchg(i)+eps)
      if(iexist.GT.0)then
      Iznuc(i)=Iatno(i)-Nfroz(i)
      if(Nfroz(i).NE.0)Ipseud=1
      else
      Iznuc(i)=Iatno(i)
      endif
100   continue
      
      
      len=LENB
      call tread(506,Exx,len,1,len,1,0)
      
      
      do 200 ishell=1,Nshell
      xa=X(ishell)
      ya=Y(ishell)
      za=Z(ishell)
      do 150 iatom=1,Natom
      iaind=3*(iatom-1)
      xc=C(1+iaind)
      yc=C(2+iaind)
      zc=C(3+iaind)
      if((abs(xa-xc).LT.crit).AND.(abs(ya-yc).LT.crit).AND.(abs(za-zc).L
     &T.crit))Jan(ishell)=iatom
150   continue
200   continue
      
      
      
      Iwcubf=0
      call ilsw(2,2,i5d6d)
      call ilsw(2,16,i7f10f)
      
      
      ibas=0
      do 300 ishell=1,MAXSHL
      if(ibas.EQ.Nbasis)goto 400
      nctr=Jan(ishell)
      maxl=Shellt(ishell)
      icnstr=Shellc(ishell)
      
      
      ks=0
      if(maxl.EQ.0)ks=1
      if(maxl.EQ.1.AND.icnstr.NE.1)ks=1
      if(maxl.EQ.2.AND.icnstr.EQ.0)ks=1
      if(ks.NE.0)then
      
      
      ibas=ibas+1
      Lctr(ibas)=nctr
      Lang(ibas)=lists
      endif
      
      
      kp=0
      if(maxl.NE.0)then
      if(maxl.EQ.1)kp=1
      if(maxl.EQ.2.AND.icnstr.EQ.0)kp=1
      if(kp.NE.0)then
      
      
      do 210 i=1,3
      ibas=ibas+1
      Lctr(ibas)=nctr
      Lang(ibas)=listp(i)
210   continue
      endif
      
      
      if(maxl.EQ.2)then
      imax=i5d6d+5
      kd=i5d6d+1
      do 220 i=1,imax
      ibas=ibas+1
      Lctr(ibas)=nctr
      Lang(ibas)=listd(i,kd)
220   continue
      
      
      elseif(maxl.EQ.3)then
      imax=7
      if(i7f10f.EQ.1)imax=10
      kf=i7f10f+1
      do 230 i=1,imax
      ibas=ibas+1
      Lctr(ibas)=nctr
      Lang(ibas)=listf(i,kf)
230   continue
      endif
      endif
300   continue
400   Ndim=Nbasis
      Nbas=Nbasis
      
      
      if(Multip.GT.1)Open=.TRUE.
      if(NBOOPT(2).NE.0)then
      Ci=.TRUE.
      else
      call ilsw(2,1,iscf)
      call ilsw(2,22,irohf)
      if(iscf.EQ.1)Uhf=.TRUE.
      if(Uhf)Open=.TRUE.
      if(irohf.EQ.1)Rohf=.TRUE.
      if(irohf.EQ.2)Rohf=.TRUE.
      if(irohf.EQ.3)Mcscf=.TRUE.
      if(iscf.GT.1)Complx=.TRUE.
      if(Complx)then
      
      write(Lfnpr,99001)
      return
      
99001 format(/1x,'The NBO program is not set up to handle complex ','wav
     &e functions')
      endif
      endif
      if(NBOOPT(5).EQ.1)Auhf=.TRUE.
      Ortho=.FALSE.
      
      
      if(Rohf.OR.Mcscf.OR.Ci)Iwfock=0
      
      
      Munit=0
      
      
      ICORE(1)=Natoms
      ICORE(2)=Ndim
      ICORE(3)=Nbas
      ICORE(4)=Munit
      ICORE(5)=0
      if(Rohf)ICORE(5)=1
      ICORE(6)=0
      if(Uhf)ICORE(6)=1
      ICORE(7)=0
      if(Ci)ICORE(7)=1
      ICORE(8)=0
      if(Open)ICORE(8)=1
      ICORE(9)=0
      if(Mcscf)ICORE(9)=1
      ICORE(10)=0
      if(Auhf)ICORE(10)=1
      ICORE(11)=0
      if(Ortho)ICORE(11)=1
      ICORE(12)=1
      nfile=3
      call nbwrit(ICORE,6,nfile)
      
      
      ii=0
      do 500 i=1,Natoms
      ii=ii+1
      ICORE(ii)=Iatno(i)
500   continue
      do 600 i=1,Natoms
      ii=ii+1
      ICORE(ii)=Iznuc(i)
600   continue
      do 700 i=1,Nbas
      ii=ii+1
      ICORE(ii)=Lctr(i)
700   continue
      do 800 i=1,Nbas
      ii=ii+1
      ICORE(ii)=Lang(i)
800   continue
      nfile=4
      call nbwrit(ICORE,Natoms+Nbas,nfile)
      
      
      call tread(501,CORE,32,1,32,1,0)
      CORE(1)=CORE(32)
      CORE(2)=CORE(32)
      nfile=8
      call nbwrit(CORE,2,nfile)
      
      
      do 900 i=1,3*Natoms
      CORE(i)=C(i)*toang
900   continue
      nfile=9
      call nbwrit(CORE,3*Natoms,nfile)
      
      
      l2=Ndim*(Ndim+1)/2
      call tread(514,CORE,Ndim,Ndim,Nbas,Nbas,1)
      call pack(CORE,Ndim,Nbas,l2)
      nfile=10
      call nbwrit(CORE,l2,nfile)
      
      
      l2=Ndim*(Ndim+1)/2
      if(Ci)then
      call tread(203,CORE,Ndim,Ndim,Nbas,Nbas,1)
      call pack(CORE,Ndim,Nbas,l2)
      nfile=20
      call nbwrit(CORE,l2,nfile)
      if(Open)then
      call tread(204,CORE,Ndim,Ndim,Nbas,Nbas,1)
      call pack(CORE,Ndim,Nbas,l2)
      nfile=21
      call nbwrit(CORE,l2,nfile)
      endif
      else
      call tread(528,CORE,Ndim,Ndim,Nbas,Nbas,1)
      call pack(CORE,Ndim,Nbas,l2)
      nfile=20
      call nbwrit(CORE,l2,nfile)
      if(Open)then
      call tread(530,CORE,Ndim,Ndim,Nbas,Nbas,1)
      call pack(CORE,Ndim,Nbas,l2)
      nfile=21
      call nbwrit(CORE,l2,nfile)
      endif
      endif
      
      
      if(Iwfock.NE.0)then
      iexist=itqry(536)
      if(iexist.GT.0)then
      l2=Ndim*(Ndim+1)/2
      call tread(536,CORE,Ndim,Ndim,Nbas,Nbas,1)
      call pack(CORE,Ndim,Nbas,l2)
      nfile=30
      call nbwrit(CORE,l2,nfile)
      endif
      
      if(Open)then
      iexist=itqry(538)
      if(iexist.GT.0)then
      l2=Ndim*(Ndim+1)/2
      call tread(538,CORE,Ndim,Ndim,Nbas,Nbas,1)
      call pack(CORE,Ndim,Nbas,l2)
      nfile=31
      call nbwrit(CORE,l2,nfile)
      endif
      endif
      endif
      
      
      iexist=itqry(524)
      if(iexist.GT.0)then
      l3=Ndim*Ndim
      call tread(524,CORE,Ndim,Ndim,Nbas,Nbas,0)
      nfile=40
      call nbwrit(CORE,l3,nfile)
      endif
      
      if(Open)then
      iexist=itqry(526)
      if(iexist.GT.0)then
      l3=Ndim*Ndim
      call tread(526,CORE,Ndim,Ndim,Nbas,Nbas,0)
      nfile=41
      call nbwrit(CORE,l3,nfile)
      endif
      endif
      
      
      iexist=itqry(518)
      iexist=min(iexist,itqry(519))
      iexist=min(iexist,itqry(520))
      if(iexist.GT.0)then
      l2=Ndim*(Ndim+1)/2
      call tread(518,CORE(1),l2,1,l2,1,0)
      call tread(519,CORE(l2+1),l2,1,l2,1,0)
      call tread(520,CORE(2*l2+1),l2,1,l2,1,0)
      do 950 i=1,3*l2
      CORE(i)=CORE(i)*toang
950   continue
      nfile=50
      call nbwrit(CORE,l2,nfile)
      nfile=51
      call nbwrit(CORE(l2+1),l2,nfile)
      nfile=52
      call nbwrit(CORE(2*l2+1),l2,nfile)
      endif
      
      
      ICORE(1)=Nshell
      
      nexp=0
      do 1000 i=1,240
      if(Exx(i).NE.zero)nexp=i
1000  continue
      ICORE(2)=nexp
      
      
      call ilsw(2,2,i5d6d)
      call ilsw(2,16,i7f10f)
      
      
      ii=2
      do 1100 i=1,Nshell
      ii=ii+1
      ICORE(ii)=0
      maxl=Shellt(i)
      icnstr=Shellc(i)
      
      
      ks=0
      if(maxl.EQ.0)ks=1
      if(maxl.EQ.1.AND.icnstr.NE.1)ks=1
      if(maxl.EQ.2.AND.icnstr.EQ.0)ks=1
      
      
      if(ks.NE.0)ICORE(ii)=ICORE(ii)+1
      
      
      kp=0
      if(maxl.NE.0)then
      if(maxl.EQ.1)kp=1
      if(maxl.EQ.2.AND.icnstr.EQ.0)kp=1
      
      
      if(kp.NE.0)ICORE(ii)=ICORE(ii)+3
      
      
      if(maxl.GE.2)then
      if(maxl.LE.2)then
      
      
      imax=i5d6d+5
      ICORE(ii)=ICORE(ii)+imax
      endif
      
      
      if(maxl.GE.3)then
      
      
      imax=7
      if(i7f10f.EQ.1)imax=10
      ICORE(ii)=ICORE(ii)+imax
      endif
      endif
      endif
      
      
1100  continue
      
      
      do 1200 i=1,Nshell
      ii=ii+1
      ICORE(ii)=Shelln(i)
1200  continue
      
      
      do 1300 i=1,Nshell
      ii=ii+1
      ICORE(ii)=Shella(i)
1300  continue
      
      if(mod(ii,2).EQ.1)then
      ii=ii+1
      ICORE(ii)=0
      endif
      ii=ii/2
      
      
      do 1400 i=1,nexp
      ii=ii+1
      CORE(ii)=Exx(i)
1400  continue
      
      
      do 1500 i=1,nexp
      ii=ii+1
      CORE(ii)=C1(i)
1500  continue
      do 1600 i=1,nexp
      ii=ii+1
      CORE(ii)=C2(i)
1600  continue
      
      
      ihold=ii
      do 1700 i=1,2*nexp
      ii=ii+1
      CORE(ii)=zero
1700  continue
      
      
      do 1800 i=1,Nshell
      iptr=Shladf(i)
      if(iptr.GT.0)then
      do 1720 j=1,Shelln(i)
      lptr=j+Shella(i)+ihold-1
      mptr=j+iptr-1
      CORE(lptr)=C3(mptr)
      CORE(lptr+nexp)=C4(mptr)
1720  continue
      endif
1800  continue
      nfile=5
      call nbwrit(CORE,ii,nfile)
      return
      end
C* :1 * 
      
