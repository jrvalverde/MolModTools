
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 phoeni"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "phoeni.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 60 "phoeni.web"
      subroutine phoeni(D,F,IOP,JUMP)
      implicit none
      double precision A,as,asxa,asya,asza,Atmchg,bs,C,C1,C2,C3,C4,Ca,Cb
     &,Cc,Ccpx,Ccpy,Ccpz,Ccqx,Ccqy
      double precision Ccqz,Cd,cmaxi,cmaxj,Dbuf2e,dxyz,Ep,Ep2i,epeq,epi,
     &eppeqi,Eq,Eqsav,Exparg,Exx,F100,F20i,F6i,Fillct,four
      double precision half,one,pconst,Pexp,pi,pii,Pqcut1,Pqcut2,Pqcut3,
     &pqx,pqy,pqz,Pt5,ptemp,Ptest,px,py,pz,qexp,qx
      double precision Qxpsav,Qxsav,qy,Qysav,qz,Qzsav,R1,R2,R3,R3ov2,R4,
     &Rabsq,Rcdsq,rho,Rhot2,Root15,Root3,Root5,rpqsq,six
      double precision symfac,three,tp,Tq,twenty,tworho,wp,X,Xa,xap,Xb,x
     &bp,Xc,xcq,Xd,xdq,Xint,Xip,Y,Ya
      double precision yap,Yb,ybp,Yc,ycq,Yd,ydq,Yip,Z,Z1,Z2,Z3,Za,zap,Zb
     &,zbp,Zc,zconst,zcq,Zd
      double precision zdq,Zero,Zip,Zistar,ztemp
      integer i,iacc,Ian,Ibf,Ibuf2e,Icharg,idcout,Idmp,Idump,Iend,Ifao,I
     &fcont,Ifpure,Igauss,Igbeg,Igdf,Igend,ijcutp,Imj,Imk
      integer Imkjml,In,Indao,Indix,Indiy,Indiz,Indjx,Indjy,Indjz,Indkx,
     &Indky,Indkz,Indlx,Indly,Indlz,intc,intcp,iop20p,Iout,ipqcut
      integer Ipunch,Ipurd,Ipure,Ipurf,Irange,iret,irwb,Is1,Is2,Is3,iset
     &,ish,ishell,ishp,Ismode,ist,Istart,Istm,Isym2e,isymm
      integer isytmp,itemp,itqb,itqb1,itqb2,itqb3,itqbas,itqprm,Itype,iz
     &ero,j,Jan,Jend,Jgauss,Jgbeg,Jgdf,Jgend,Jml,Jpure,Jrange
      integer Js1,Js2,Js3,jsh,jshell,jshp,jst,Jstart,Jstm,Jtype,JUMP,k,K
     &end,Kgauss,Kgbeg,Kgdf,Kgend,Klcutq,Klind,Kml
      integer Kpure,Krange,Ks1,Ks2,Ks3,ksh,kshell,kshp,kst,Kstart,Kstm,K
     &type,l,Lamax,lambda,Lbmax,Lbound,Lcmax,Ldmax,LENB
      integer Lend,lenneq,Lentq,lentqf,lentqp,Lgauss,Lgbeg,Lgdf,Lgend,Li
     &mxyz,Lpmax,Lpqmax,Lpure,Lqmax,Lrange,lsave,lsh,lshell,lshp,lst
      integer Lstart,Lstm,Ltype,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,Maxxy
     &z,mtget,mtype,mu,Multip,N10ord,N5ord,N6ord,N7ord,Nae,Natoms,Nbasis
      integer Nbe,ndc,Ne,neq,neqshl,Nfa,Nfb,Nfc,Nfd,nga,ngb,ngc,ngd,Nord
     &r,nset,Nshell,nsymop,nu,Numdf,nzero
      integer IOP(*)
      double precision D(*),F(*)
      logical reject
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      integer sconap,sconbp,sconcp,scondp
      integer Ubound,Ulpure,sigma
      logical dbuf
      dimension tp(7),wp(7),Zip(256),Ibuf2e(9520),irwb(2)
      dimension lsave(4),itqbas(3),isytmp(2)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension neqshl(MAXSHL,8)
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/twbuf/Tq(5184),Dbuf2e(4760)
      common/io/In,Iout,Ipunch
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      common/limit/Imj,Imk,Jml,Kml,Imkjml,Istart,Jstart,Kstart,Lstart,Ie
     &nd,Jend,Kend,Lend,Irange,Jrange,Krange,Lrange,Lentq,Numdf
      common/stypes/Itype,Jtype,Ktype,Ltype
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/ipure/Ipurd,Ipurf
      common/coord/Xa,Ya,Za,Xb,Yb,Zb,Rabsq,Xc,Yc,Zc,Xd,Yd,Zd,Rcdsq
      common/gcloop/Igauss,Igbeg,Igend,Igdf,Jgauss,Jgbeg,Jgend,Jgdf,Kgau
     &ss,Kgbeg,Kgend,Kgdf,Lgauss,Lgbeg,Lgend,Lgdf
      common/qinfo/Eqsav(100),Qxsav(100),Qysav(100),Qzsav(100),Qxpsav(10
     &0),Exparg,Ptest,Pexp,Ep,Eq,Ep2i,Klind,Klcutq(100)
      common/dfcuts/Pqcut1,Pqcut2,Pqcut3,Fillct
      common/int/Zero,Xint(12)
      common/a/A(174)
      common/ccpq/Ccpx(48),Ccpy(48),Ccpz(48),Ccqx(48),Ccqy(48),Ccqz(48)
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      common/xyzint/Xip(256),Yip(256),Zistar(256)
      common/rhot2/Rhot2
      common/contr/Ca(20),Cb(20),Cc(20),Cd(20)
      common/intcon/F6i,F20i,F100
      common/jpure/Ifpure(4,4),Ipure,Jpure,Kpure,Lpure
      common/aoinds/Ifcont,Limxyz,Maxxyz,Ifao,Indao(1296)
      common/indxyz/Indix(20),Indiy(20),Indiz(20),Indjx(20),Indjy(20),In
     &djz(20),Indkx(20),Indky(20),Indkz(20),Indlx(20),Indly(20),Indlz(20
     &)
      common/ibf/Ibf(30)
      common/nf/Nfa,Nfb,Nfc,Nfd,Istm,Jstm,Kstm,Lstm
      common/site/Is1(10),Js1(10),Ks1(10),Is2(10),Js2(10),Ks2(10),Is3(10
     &),Js3(10),Ks3(10)
      common/dump/Idmp,Idump
      equivalence(Zip(1),Zistar(1))
      equivalence(Ibuf2e(1),Dbuf2e(1))
      equivalence(Ismode,Ibf(1)),(Isym2e,Ibf(30))
      data idcout/14/,dbuf/.TRUE./
      data irwb/506,1481/
      data isymm/551/,neq/565/
      data six/6.0D0/,twenty/20.0D0/
      data half/0.5D0/,one/1.0D0/,four/4.0D0/,three/3.0D0/
      
      
      
      
      
      
      
      
      
      
      
      irwb(2)=LENB
      Idump=IOP(34)
      iacc=IOP(28)-idcout
      call ilsw(2,16,Ipurf)
      call ilsw(2,2,Ipurd)
      iop20p=2*Ipurf+Ipurd+1
      lsave(1)=Ubound(1)
      lsave(2)=Ubound(2)
      lsave(3)=Ubound(3)
      lsave(4)=Ubound(4)
      if(Ipurd.EQ.0)lsave(3)=Ulpure(3)
      if(Ipurf.EQ.0)lsave(4)=Ulpure(4)
      pi=four*datan(one)
      Pt5=half
      Root3=dsqrt(three)
      Root5=dsqrt(Xint(5))
      Root15=dsqrt(Xint(10)+Xint(5))
      R1=Pt5*dsqrt(Xint(5)/Xint(2))
      R2=Xint(3)/(Xint(2)*Root5)
      R4=Pt5*dsqrt(Xint(3)/Xint(2))
      Z1=Xint(4)/Root5
      Z2=Xint(1)/Root5
      Z3=Xint(3)/Root5
      R3ov2=Pt5*Root3
      R3=R3ov2
      F6i=one/six
      F20i=one/twenty
      pconst=(pi+pi)*pi*dsqrt(pi)
      pii=one/pi
      call tread(irwb(1),Exx(1),irwb(2),1,irwb(2),1,0)
      call setord
      call out2e(-1,mu,nu,lambda,sigma,Tq(1),dbuf,Ibuf2e,Dbuf2e,iret,idc
     &out,IOP,D,F)
      if(iret.EQ.0)then
      
      if(Isym2e.EQ.1)then
      call tread(isymm,isytmp,1,1,1,1,0)
      nsymop=isytmp(1)
      lenneq=4*MAXSHL
      call tread(neq,neqshl(1,1),lenneq,1,lenneq,1,0)
      endif
      do 50 i=1,174
      A(i)=Zero
50    continue
      do 100 i=1,48
      Ccpx(i)=Zero
      Ccpy(i)=Zero
      Ccpz(i)=Zero
      Ccqx(i)=Zero
      Ccqy(i)=Zero
      Ccqz(i)=Zero
100   continue
      
      call dfcut(IOP)
      call setrys
      
      
      do 150 ish=1,Nshell
      do 140 jsh=1,ish
      do 130 ksh=1,jsh
      do 125 lsh=1,ksh
      
      mtype=mtget(ish,jsh,ksh,lsh,ishp,jshp,kshp,lshp,nset)
      if(Idump.EQ.11)then
      write(6,99002)ish,jsh,ksh,lsh
      write(6,99003)ishp,jshp,kshp,lshp
      write(6,99004)nset
      endif
      
      Numdf=Shellt(ish)/2+Shellt(jsh)/2+Shellt(ksh)/2+Shellt(lsh)/2
      if(iacc.NE.0)then
      if(Numdf.LE.0)goto 125
      endif
      
      call isymgo(ish,jsh,ksh,lsh,nsymop,neqshl,Isym2e,reject,symfac)
      if(.NOT.(reject))then
      
      Nfa=lentqf(ishp,Istm)
      Nfb=lentqf(jshp,Jstm)
      Nfc=lentqf(kshp,Kstm)
      Nfd=lentqf(lshp,Lstm)
      Lentq=Nfa*Nfb*Nfc*Nfd
      
      
      call sitind
      
      
      
      
      lentqp=Lentq
      ndc=Shelln(ish)*Shelln(jsh)*Shelln(ksh)*Shelln(lsh)
      if(ndc.EQ.1)lentqp=0
      
      itqprm=0
      if(Ismode.LE.0)then
      
      itqbas(1)=itqprm+lentqp
      itqbas(2)=itqbas(1)
      itqbas(3)=itqbas(1)
      else
      
      itqbas(1)=itqprm+lentqp
      itqbas(2)=itqbas(1)+Lentq
      itqbas(3)=itqbas(2)+Lentq
      endif
      if(Idump.EQ.11)write(6,99007)Lentq,lentqp,itqprm,itqbas
      
      do 120 iset=1,nset
      
      itqb=itqbas(iset)
      if(ndc.EQ.1)itqprm=itqb
      
      if(iset.EQ.2)then
      
      ishell=ishp
      jshell=lshp
      kshell=jshp
      lshell=kshp
      elseif(iset.EQ.3)then
      
      ishell=ishp
      jshell=kshp
      kshell=jshp
      lshell=lshp
      else
      
      ishell=ishp
      jshell=jshp
      kshell=kshp
      lshell=lshp
      endif
      if(Idump.EQ.11)write(6,99008)iset,itqb,ishell,jshell,kshell,lshell
      
      Xa=X(ishell)
      Ya=Y(ishell)
      Za=Z(ishell)
      Igbeg=Shella(ishell)
      nga=Shelln(ishell)
      Itype=Shellt(ishell)
      Igdf=Shladf(ishell)
      sconap=Shellc(ishell)+1
      Igend=Igbeg+nga-1
      Lamax=Itype+1
      Istart=Lbound(Lamax,sconap)
      Iend=Ubound(Lamax)
      Irange=Iend-Istart+1
      Ipure=Ifpure(Lamax,iop20p)
      
      Xb=X(jshell)
      Yb=Y(jshell)
      Zb=Z(jshell)
      Jgbeg=Shella(jshell)
      ngb=Shelln(jshell)
      Jtype=Shellt(jshell)
      Jgdf=Shladf(jshell)
      sconbp=Shellc(jshell)+1
      Jgend=Jgbeg+ngb-1
      Lbmax=Jtype+1
      Jstart=Lbound(Lbmax,sconbp)
      Jend=Ubound(Lbmax)
      Jrange=Jend-Jstart+1
      Jpure=Ifpure(Lbmax,iop20p)
      Lpmax=Lamax+Lbmax-1
      Imj=iabs(ishell-jshell)
      Rabsq=(Xb-Xa)**2+(Yb-Ya)**2+(Zb-Za)**2
      
      Xc=X(kshell)
      Yc=Y(kshell)
      Zc=Z(kshell)
      Kgbeg=Shella(kshell)
      ngc=Shelln(kshell)
      Ktype=Shellt(kshell)
      Kgdf=Shladf(kshell)
      sconcp=Shellc(kshell)+1
      Kgend=Kgbeg+ngc-1
      Lcmax=Ktype+1
      Kstart=Lbound(Lcmax,sconcp)
      Kend=Ubound(Lcmax)
      Krange=Kend-Kstart+1
      Kpure=Ifpure(Lcmax,iop20p)
      Imk=iabs(ishell-kshell)
      
      Xd=X(lshell)
      Yd=Y(lshell)
      Zd=Z(lshell)
      Lgbeg=Shella(lshell)
      ngd=Shelln(lshell)
      Ltype=Shellt(lshell)
      Lgdf=Shladf(lshell)
      scondp=Shellc(lshell)+1
      Lgend=Lgbeg+ngd-1
      Ldmax=Ltype+1
      Lstart=Lbound(Ldmax,scondp)
      Lend=Ubound(Ldmax)
      Lrange=Lend-Lstart+1
      Lpure=Ifpure(Ldmax,iop20p)
      Lqmax=Lcmax+Ldmax-1
      Lpqmax=Lpmax+Lqmax-1
      Jml=iabs(jshell-lshell)
      Kml=iabs(kshell-lshell)
      Imkjml=Imk+Jml
      nzero=((Itype+Jtype+Ktype+Ltype)/2)+1
      
      Rcdsq=(Xd-Xc)**2+(Yd-Yc)**2+(Zd-Zc)**2
      Iend=Ubound(Lamax)
      Jend=Ubound(Lbmax)
      Kend=Ubound(Lcmax)
      Lend=Ubound(Ldmax)
      call aclear(Lentq,Tq(1+itqb))
      
      call gaoind(IOP)
      
      call qinf
      intc=0
      do 110 Igauss=Igbeg,Igend
      as=Exx(Igauss)
      call fillcp(Itype,Igbeg,Igauss,Igdf,Ca,cmaxi)
      asxa=as*Xa
      asya=as*Ya
      asza=as*Za
      
      do 108 Jgauss=Jgbeg,Jgend
      bs=Exx(Jgauss)
      call fillcp(Jtype,Jgbeg,Jgauss,Jgdf,Cb,cmaxj)
      
      Ep=as+bs
      epi=one/Ep
      Ep2i=one/(Ep+Ep)
      px=(asxa+bs*Xb)*epi
      py=(asya+bs*Yb)*epi
      pz=(asza+bs*Zb)*epi
      Exparg=as*bs*Rabsq*epi
      if(Exparg.LT.Pqcut3)then
      Pexp=dexp(-Exparg)
      ptemp=pconst*Pexp/symfac
      Ptest=cmaxi*cmaxj*Pexp
      if(Ptest.GE.Pqcut1)then
      ijcutp=0
      
      elseif(Ptest.LT.Pqcut2)then
      ijcutp=2
      else
      
      ijcutp=1
      endif
      xap=px-Xa
      xbp=px-Xb
      yap=py-Ya
      ybp=py-Yb
      zap=pz-Za
      zbp=pz-Zb
      call getcc2(Ccpx,xap,xbp,Lamax,Lbmax)
      call getcc2(Ccpy,yap,ybp,Lamax,Lbmax)
      call getcc2(Ccpz,zap,zbp,Lamax,Lbmax)
      
      Klind=0
      do 106 Kgauss=Kgbeg,Kgend
      call fillc(Ktype,Kgbeg,Kgauss,Kgdf,Cc)
      
      do 104 Lgauss=Lgbeg,Lgend
      Klind=Klind+1
      call fillc(Ltype,Lgbeg,Lgauss,Lgdf,Cd)
      
      
      ipqcut=ijcutp+Klcutq(Klind)
      if(ipqcut.LT.2)then
      Eq=Eqsav(Klind)
      qx=Qxsav(Klind)
      qy=Qysav(Klind)
      qz=Qzsav(Klind)
      qexp=Qxpsav(Klind)
      epeq=Ep*Eq
      eppeqi=one/(Ep+Eq)
      rho=epeq*eppeqi
      tworho=rho+rho
      ztemp=ptemp*dsqrt(eppeqi)*qexp/epeq
      xcq=qx-Xc
      xdq=qx-Xd
      ycq=qy-Yc
      ydq=qy-Yd
      zcq=qz-Zc
      zdq=qz-Zd
      call getcc2(Ccqx,xcq,xdq,Lcmax,Ldmax)
      call getcc2(Ccqy,ycq,ydq,Lcmax,Ldmax)
      call getcc2(Ccqz,zcq,zdq,Lcmax,Ldmax)
      pqx=qx-px
      pqy=qy-py
      pqz=qz-pz
      rpqsq=pqx*pqx+pqy*pqy+pqz*pqz
      dxyz=rho*rpqsq
      call rpol2(nzero,dxyz,tp,wp)
      call geta2
      
      
      if(ndc.NE.1)call aclear(Lentq,Tq(1+itqprm))
      
      
      
      
      do 102 izero=1,nzero
      Rhot2=tworho*tp(izero)
      if(Idump.EQ.13)write(6,99001)izero,Rhot2,tworho,tp(izero)
      
99001 format(' IZERO,RHOT2,TWORHO,TP(IZERO)=',i10,3G15.3)
      
      zconst=ztemp*wp(izero)
      if(zconst.GT.Pqcut2)then
      call getip2(Xip,pqx,one,Ccpx,Ccqx)
      call getip2(Yip,pqy,one,Ccpy,Ccqy)
      call getip2(Zip,pqz,zconst,Ccpz,Ccqz)
      
      
      
      
      call aosumf(intc,Tq(itqprm+1),Xip,Yip,Zip)
      endif
      
      
      
102   continue
      
      
      call cntprm(ndc,intc,Tq(1+itqb),Tq(1+itqprm))
      endif
      
      
104   continue
106   continue
      endif
108   continue
110   continue
      
      
      itemp=iabs(Ipure)+iabs(Jpure)+iabs(Kpure)+iabs(Lpure)
      if(intc.EQ.0.AND.Ismode.NE.0.AND.iset.EQ.1.AND.itemp.NE.0)then
      Iend=Ulpure(Lamax)
      Jend=Ulpure(Lbmax)
      Kend=Ulpure(Lcmax)
      Lend=Ulpure(Ldmax)
      Nfa=Iend-Istart+1
      Nfb=Jend-Jstart+1
      Nfc=Kend-Kstart+1
      Nfd=Lend-Lstart+1
      endif
      
      if(intc.NE.0)then
      if(Idump.EQ.11)write(6,99005)intc
      if(Idump.EQ.11)write(6,99006)(i,Tq(i+itqb),i=1,intc)
      call purdf2(intc,Ismode,iset,Tq(1+itqb),Tq(1+itqb),intcp)
      if(Ismode.LE.0)then
      ist=Aos(ishell)-1
      jst=Aos(jshell)-1
      kst=Aos(kshell)-1
      lst=Aos(lshell)-1
      intc=itqb
      do 118 i=Istart,Iend
      mu=ist+Nordr(i)
      if(Imj.EQ.0)Jend=i
      if(Imkjml.EQ.0)Kend=i
      do 116 j=Jstart,Jend
      nu=jst+Nordr(j)
      do 114 k=Kstart,Kend
      lambda=kst+Nordr(k)
      Lend=lsave(Ldmax)
      if(Kml.EQ.0)Lend=k
      if(Imkjml.EQ.0.AND.i.EQ.k)Lend=j
      do 112 l=Lstart,Lend
      sigma=lst+Nordr(l)
      intc=intc+1
      
      call out2e(1,mu,nu,lambda,sigma,Tq(intc),dbuf,Ibuf2e,Dbuf2e,iret,i
     &dcout,IOP,D,F)
      
112   continue
114   continue
116   continue
118   continue
      endif
      endif
      
120   continue
      
      
      if(Ismode.NE.0)then
      
      
      itqb1=itqbas(1)
      itqb2=itqbas(2)
      itqb3=itqbas(3)
      if(mtype.EQ.2.OR.mtype.EQ.5)then
      
      
      call replct(Lentq,Tq(1+itqb2),Tq(1+itqb3),2,3)
      call dfout2(Tq(1+itqb1),Tq(1+itqb2),Tq(1+itqb3),dbuf,Ibuf2e,Dbuf2e
     &,ishp,jshp,kshp,lshp,IOP,D,F)
      elseif(mtype.EQ.3)then
      
      
      call replct(Lentq,Tq(1+itqb1),Tq(1+itqb3),1,3)
      call dfout3(Tq(1+itqb1),Tq(1+itqb2),Tq(1+itqb3),dbuf,Ibuf2e,Dbuf2e
     &,ishp,jshp,kshp,lshp,IOP,D,F)
      elseif(mtype.EQ.4.OR.mtype.EQ.7)then
      
      
      call replct(Lentq,Tq(1+itqb1),Tq(1+itqb2),1,2)
      call replct(Lentq,Tq(1+itqb1),Tq(1+itqb3),1,3)
      call dfout4(Tq(1+itqb1),Tq(1+itqb2),Tq(1+itqb3),dbuf,Ibuf2e,Dbuf2e
     &,ishp,jshp,kshp,lshp,IOP,D,F)
      elseif(mtype.EQ.6)then
      
      
      call replct(Lentq,Tq(1+itqb2),Tq(1+itqb3),2,3)
      call dfout6(Tq(1+itqb1),Tq(1+itqb2),Tq(1+itqb3),dbuf,Ibuf2e,Dbuf2e
     &,ishp,jshp,kshp,lshp,IOP,D,F)
      elseif(mtype.EQ.8)then
      
      
      call replct(Lentq,Tq(1+itqb1),Tq(1+itqb2),1,2)
      call replct(Lentq,Tq(1+itqb1),Tq(1+itqb3),1,3)
      call dfout8(Tq(1+itqb1),Tq(1+itqb2),Tq(1+itqb3),dbuf,Ibuf2e,Dbuf2e
     &,ishp,jshp,kshp,lshp,IOP,D,F)
      else
      
      call dfout1(Tq(1+itqb1),Tq(1+itqb2),Tq(1+itqb3),dbuf,Ibuf2e,Dbuf2e
     &,ishp,jshp,kshp,lshp,IOP,D,F)
      endif
      endif
      endif
      
      
125   continue
130   continue
140   continue
150   continue
      
      call out2e(0,mu,nu,lambda,sigma,Tq(1),dbuf,Ibuf2e,Dbuf2e,iret,idco
     &ut,IOP,D,F)
      endif
      
      JUMP=0
      
99002 format(' ISH,  ETC=',4I3)
99003 format(' ISHP, ETC=',4I3)
99004 format(' AFTER MTGET, NSET=',i2)
99005 format(' AT END OF LOOP, INTC=',i6)
99006 format(7(i5,d13.6))
99007 format(' LENTQ=',i5/' LENTQP=',i5/' ITQPRM=',i5/' ITQBAS=',3I5)
99008 format(' IN ISET LOOP:',i3,'   ITQB=',i6/' ISHELL, ETC=',4I3)
      
      return
      
      end
C* :1 * 
      
