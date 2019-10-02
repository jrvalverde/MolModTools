
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dphnix"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dphnix.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 64 "dphnix.web"
      subroutine dphnix(IACC,ISCF,DA,DB,VEE,FXYZ,C,NATOMS,USESYM,NSYMOP,
     &JTRANS,NEQATM,IPRINT,IDUMP)
      implicit none
      double precision A,Ab2i,Aiab,as,asxa,asya,asza,Biab,bs,C,C1,C2,C3,
     &C4,Ca,Cb,Cc,cc1,cc2,cc3
      double precision Ccpx,Ccpy,Ccpz,Ccqx,Ccqy,Ccqz,Cd,Cd2i,Cicd,cmaxi,
     &cmaxj,cmaxk,cmaxl,cs,d1234,D1abx,D1aby,D1abz,D1cdx,D1cdy
      double precision D1cdz,DA,DB,Dicd,dijkl,dnsmax,ds,dxyz,Ep,Ep2i,epe
     &q,epi,eppeqi,Eq,eqi,Eqsav,Exparg,Exx,F100,F20i
      double precision f42,F6i,Fillct,four,fxi,fxiv,fxj,fxjv,fxk,fxkv,fx
     &l,FXYZ,fyi,fyiv,fyj,fyjv,fyk,fykv,fyl,fzi
      double precision fziv,fzj,fzjv,fzk,fzkv,fzl,gabs,gatan,gexp,gsqrt,
     &half,one,pconst,Pexp,pi,pii,Pqcut1,Pqcut2,Pqcut3,pqcut4
      double precision pqcut5,pqcut6,Pqx,Pqy,Pqz,Pt5,ptemp,Ptest,px,py,p
     &z,qexp,qx,Qxpsav,Qxsav,qy,Qysav,qz,Qzsav,R1
      double precision R2,R3,R3ov2,R4,Rabsq,Rcdsq,rho,Rhot2,Root15,Root3
     &,Root5,rpqsq,six,temp,ten,tenm10,tenm35,tenm5,tenm9,three
      double precision tp,tstij,tstijk,twenty,tworho,VEE,wp,X,Xa,xap,Xb,
     &xbp,Xc,xcq,Xd,xdq,Xint,xip,xipi,xipj
      double precision xipk,xyip,Xyzdmy,xzip,Y,Ya,yap,Yb,ybp,Yc,ycq,Yd,y
     &dq,yip,yipi,yipj,yipk,yzip,Z,Z1
      double precision Z2,Z3,Za,zap,Zb,zbp,Zc,zconst,zcq,Zd,zdq,Zero,zip
     &,zipi,zipj,zipk,ztemp,ztest
      integer i,IACC,iatm,iatms,iatms1,idum,IDUMP,Iend,iforms,Ifpure,Iga
     &uss,Igbeg,Igdf,Igend,ijcutp,ijkl,Imj,Imk,Imkjml,In
      integer indx,indy,indz,ioffst,iop20p,Iout,ipqcut,IPRINT,iprio,Ipun
     &ch,Ipurd,Ipure,Ipurf,Irange,ISCF,ishell,Istart,isymop,itmp,Itype
      integer ix,ixtr,ixyznt,iy,iytr,iz,izero,iztr,j,Jan,jatms,jatms1,Je
     &nd,Jgauss,Jgbeg,Jgdf,Jgend,Jml,joffst,jop
      integer jopind,jopm1,Jpure,Jrange,jrop,jshell,Jstart,jtmp,JTRANS,J
     &type,jx,jy,jz,k,katms,katms1,Kend,Kgauss,Kgbeg,Kgdf
      integer Kgend,Klcutq,Klind,Kml,koffst,kop,Kpure,Krange,kshell,Ksta
     &rt,ktmp,Ktype,kx,ky,kz,l,Lamax,latms,latms1,Lbmax
      integer Lbound,Lcmax,Ldmax,LENB,Lend,Lentq,Lgauss,Lgbeg,Lgdf,Lgend
     &,Lpmax,Lpqmax,Lpure,Lqmax,Lrange,lshell,Lstart,ltmp,Ltype,lx
      integer ly,lz,maxl,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,mop,N10ord,N
     &5ord,N6ord,N7ord,NATOMS,ndc,nga,ngb,ngc,ngd,Nordr
      integer np1,np2,nprio,Nshell,NSYMOP,Numdf,numop,nxyznt,nzero,nzros
      integer NEQATM(*)
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      integer sconap,sconbp,sconcp,scondp
      integer Ubound,Ulpure
      logical USESYM
      dimension tp(7),wp(7)
      dimension xip(580),yip(580),zip(580),xipi(405),yipi(405),zipi(405)
     &,xipj(405),yipj(405),zipj(405),xipk(405),yipk(405),zipk(405)
      dimension indx(20),indy(20),indz(20)
      dimension DA(*),DB(*),C(*),FXYZ(3,NATOMS),d1234(1296)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      dimension iatm(MAXSHL),JTRANS(3,8),isymop(8),iprio(8)
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
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
      common/rhot2/Rhot2
      common/contr/Ca(20),Cb(20),Cc(20),Cd(20)
      common/intcon/F6i,F20i,F100
      common/jpure/Ifpure(4,4),Ipure,Jpure,Kpure,Lpure
      common/ipdrv/Ab2i,Cd2i,Aiab,Biab,Cicd,Dicd,Pqx,Pqy,Pqz,D1abx,D1aby
     &,D1abz,D1cdx,D1cdy,D1cdz,Xyzdmy(48)
      data indx/0,1,0,0,2,0,0,1,1,0,3,0,0,1,2,2,1,0,0,1/
      data indy/0,0,1,0,0,2,0,1,0,1,0,3,0,2,1,0,0,1,2,1/
      data indz/0,0,0,1,0,0,2,0,1,1,0,0,3,0,0,1,2,2,1,1/
      data six/6.0D0/,twenty/20.0D0/
      data half/0.5D0/,one/1.0D0/,ten/10.0D0/,f42/42.0D0/,four/4.0D0/,th
     &ree/3.0D0/
      data tenm5/1.0D-5/,tenm10/1.0D-10/,tenm9/1.0D-9/,tenm35/1.0D-35/
      
      
      
      
      
      
      
      
      
      
      
99001 format(' ',4I2,12F10.6)
99002 format(11x,'SYMOP',i3,18x,i2,38x,i2,38x,i2)
99003 format(' TOTAL VEE',f15.10)
99004 format(9x,'D VEE / D X    D VEE / D Y    D VEE / D Z')
99005 format(i5,3F15.8)
      
      
      Pqcut1=tenm5
      Pqcut2=tenm10
      Pqcut3=f42
      pqcut4=tenm9
      pqcut6=tenm35
      
      VEE=Zero
      do 100 i=1,3
      do 50 j=1,NATOMS
      FXYZ(i,j)=Zero
50    continue
100   continue
      
      iop20p=2*Ipurf+Ipurd+1
      pi=four*gatan(one)
      Pt5=half
      Root3=gsqrt(three)
      Root5=gsqrt(Xint(5))
      Root15=gsqrt(Xint(10)+Xint(5))
      R1=Pt5*gsqrt(Xint(5)/Xint(2))
      R2=Xint(3)/(Xint(2)*Root5)
      R4=Pt5*gsqrt(Xint(3)/Xint(2))
      Z1=Xint(4)/Root5
      Z2=Xint(1)/Root5
      Z3=Xint(3)/Root5
      R3ov2=Pt5*Root3
      F6i=one/six
      F20i=one/twenty
      pconst=(pi+pi)*pi*gsqrt(pi)
      pii=one/pi
      
      call setord
      
      call shlatm(NATOMS,C,Nshell,X,Y,Z,iatm)
      
      do 200 i=1,174
      A(i)=Zero
200   continue
      do 300 i=1,48
      Ccpx(i)=Zero
      Ccpy(i)=Zero
      Ccpz(i)=Zero
      Ccqx(i)=Zero
      Ccqy(i)=Zero
      Ccqz(i)=Zero
300   continue
      
      call setrys
      
      do 500 itmp=1,Nshell
      do 400 jtmp=1,itmp
      do 360 ktmp=1,itmp
      maxl=ktmp
      if(itmp.EQ.ktmp)maxl=jtmp
      do 340 ltmp=1,maxl
      Numdf=Shellt(itmp)/2+Shellt(jtmp)/2+Shellt(ktmp)/2+Shellt(ltmp)/2
      if((IACC.GE.2).OR.(Numdf.GE.1))then
      ishell=itmp
      jshell=jtmp
      kshell=ktmp
      lshell=ltmp
      if(iatm(ishell).EQ.iatm(jshell))then
      ishell=ktmp
      jshell=ltmp
      kshell=itmp
      lshell=jtmp
      endif
      if((iatm(ishell).EQ.iatm(kshell)).OR.(iatm(ishell).EQ.iatm(lshell)
     &))then
      idum=ishell
      ishell=jshell
      jshell=idum
      endif
      if(iatm(jshell).EQ.iatm(kshell))then
      idum=kshell
      kshell=lshell
      lshell=idum
      endif
      iatms=iatm(ishell)
      jatms=iatm(jshell)
      katms=iatm(kshell)
      latms=iatm(lshell)
      
      iforms=1
      if(katms.EQ.latms)iforms=iforms+3
      if(iatms.EQ.jatms)iforms=iforms+2
      if(jatms.EQ.latms)iforms=iforms+1
      if(iatms.EQ.katms)iforms=iforms+1
      if(iforms.NE.8)then
      if(USESYM)then
      
      
      numop=1
      np1=nprio(iatms,jatms,katms,latms)
      isymop(1)=1
      iprio(1)=np1
      jopind=0
      do 302 jop=2,NSYMOP
      jopind=jopind+NATOMS
      iatms1=NEQATM(iatms+jopind)
      jatms1=NEQATM(jatms+jopind)
      katms1=NEQATM(katms+jopind)
      latms1=NEQATM(latms+jopind)
      np2=nprio(iatms1,jatms1,katms1,latms1)
      if(np2.GT.np1)goto 340
      numop=numop+1
      isymop(numop)=jop
      iprio(numop)=np2
302   continue
      
      
      mop=numop-1
      do 306 jrop=1,mop
      jop=numop-jrop+1
      jopm1=jop-1
      do 304 kop=1,jopm1
      if(iprio(kop).EQ.iprio(jop))then
      isymop(jop)=0
      goto 306
      endif
      
304   continue
306   continue
      endif
      
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
      nzero=((Itype+Jtype+Ktype+Ltype+1)/2)+1
      
      Rcdsq=(Xd-Xc)**2+(Yd-Yc)**2+(Zd-Zc)**2
      Iend=Ubound(Lamax)
      Jend=Ubound(Lbmax)
      Kend=Ubound(Lcmax)
      Lend=Ubound(Ldmax)
      
      call efill1(ishell,jshell,kshell,lshell,Istart,Jstart,Kstart,Lstar
     &t,Iend,Jend,Kend,Lend,Imj,Imkjml,Kml,ISCF,DA,DB,d1234,dnsmax)
      ndc=nga*ngb*ngc*ngd
      koffst=Ldmax
      joffst=Lcmax*koffst
      ioffst=Lbmax*joffst
      nxyznt=Lamax*ioffst
      fxi=Zero
      fyi=Zero
      fzi=Zero
      fxj=Zero
      fyj=Zero
      fzj=Zero
      fxk=Zero
      fyk=Zero
      fzk=Zero
      
      call qinf
      do 332 Igauss=Igbeg,Igend
      as=Exx(Igauss)
      call fillcp(Itype,Igbeg,Igauss,Igdf,Ca,cmaxi)
      asxa=as*Xa
      asya=as*Ya
      asza=as*Za
      
      do 330 Jgauss=Jgbeg,Jgend
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
      Pexp=gexp(-Exparg)
      ptemp=pconst*Pexp
      Ptest=cmaxi*cmaxj*Pexp
      tstij=dnsmax*cmaxi*cmaxj*ten
      ijcutp=0
      if(Ptest.LT.Pqcut1)ijcutp=1
      if(Ptest.LT.Pqcut2)ijcutp=2
      xap=px-Xa
      xbp=px-Xb
      yap=py-Ya
      ybp=py-Yb
      zap=pz-Za
      zbp=pz-Zb
      call getcc2(Ccpx,xap,xbp,Lamax,Lbmax)
      call getcc2(Ccpy,yap,ybp,Lamax,Lbmax)
      call getcc2(Ccpz,zap,zbp,Lamax,Lbmax)
      Aiab=as*epi
      Biab=bs*epi
      Ab2i=Ep2i
      temp=-(as+as)*Biab
      D1abx=(Xa-Xb)*temp
      D1aby=(Ya-Yb)*temp
      D1abz=(Za-Zb)*temp
      
      Klind=0
      do 328 Kgauss=Kgbeg,Kgend
      call fillcp(Ktype,Kgbeg,Kgauss,Kgdf,Cc,cmaxk)
      cs=Exx(Kgauss)
      tstijk=tstij*cmaxk
      
      do 326 Lgauss=Lgbeg,Lgend
      Klind=Klind+1
      call fillcp(Ltype,Lgbeg,Lgauss,Lgdf,Cd,cmaxl)
      
      
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
      ztemp=ptemp*gsqrt(eppeqi)*qexp/epeq
      pqcut5=pqcut4/ztemp
      pqcut5=pqcut5/(one+Ep+Eq)
      ztest=tstijk*cmaxl
      if(ztest.GE.pqcut5)then
      xcq=qx-Xc
      xdq=qx-Xd
      ycq=qy-Yc
      ydq=qy-Yd
      zcq=qz-Zc
      zdq=qz-Zd
      call getcc2(Ccqx,xcq,xdq,Lcmax,Ldmax)
      call getcc2(Ccqy,ycq,ydq,Lcmax,Ldmax)
      call getcc2(Ccqz,zcq,zdq,Lcmax,Ldmax)
      Pqx=qx-px
      Pqy=qy-py
      Pqz=qz-pz
      rpqsq=Pqx*Pqx+Pqy*Pqy+Pqz*Pqz
      dxyz=rho*rpqsq
      call rpol2(nzero,dxyz,tp,wp)
      call geta2
      ds=Exx(Lgauss)
      eqi=one/Eq
      Cd2i=Pt5*eqi
      Cicd=cs*eqi
      Dicd=ds*eqi
      temp=-(cs+cs)*Dicd
      D1cdx=(Xc-Xd)*temp
      D1cdy=(Yc-Yd)*temp
      D1cdz=(Zc-Zd)*temp
      
      
      nzros=0
      ixyznt=1-nxyznt
      do 308 izero=1,nzero
      Rhot2=tworho*tp(izero)
      zconst=ztemp*wp(izero)
      if(zconst.GE.pqcut6)then
      nzros=nzros+1
      ixyznt=ixyznt+nxyznt
      call getip2(xip(ixyznt),Pqx,one,Ccpx,Ccqx)
      call getip2(yip(ixyznt),Pqy,one,Ccpy,Ccqy)
      call getip2(zip(ixyznt),Pqz,zconst,Ccpz,Ccqz)
      if(iforms.EQ.2)then
      
      call dipac(xip,yip,zip,xipi,yipi,zipi,xipk,yipk,zipk,ixyznt)
      elseif(iforms.EQ.3)then
      
      call dipac(xip,yip,zip,xipi,yipi,zipi,xipi,yipi,zipi,ixyznt)
      elseif(iforms.EQ.4)then
      
      call dipab(xip,yip,zip,xipi,yipi,zipi,xipj,yipj,zipj,ixyznt)
      elseif(iforms.EQ.5)then
      
      call dipa(xip,yip,zip,xipi,yipi,zipi,ixyznt)
      elseif(iforms.EQ.6)then
      
      call dipab(xip,yip,zip,xipi,yipi,zipi,xipi,yipi,zipi,ixyznt)
      else
      
      call dipabc(xip,yip,zip,xipi,yipi,zipi,xipj,yipj,zipj,xipk,yipk,zi
     &pk,ixyznt)
      endif
      endif
308   continue
      
      
      
      ijkl=0
      do 324 i=Istart,Iend
      ix=indx(i)*ioffst+1-nxyznt
      iy=indy(i)*ioffst+1-nxyznt
      iz=indz(i)*ioffst+1-nxyznt
      if(Imj.EQ.0)Jend=i
      if(Imkjml.EQ.0)Kend=i
      cc1=Ca(i)
      
      do 322 j=Jstart,Jend
      jx=indx(j)*joffst+ix
      jy=indy(j)*joffst+iy
      jz=indz(j)*joffst+iz
      cc2=cc1*Cb(j)
      
      do 320 k=Kstart,Kend
      Lend=Ubound(Ldmax)
      kx=indx(k)*koffst+jx
      ky=indy(k)*koffst+jy
      kz=indz(k)*koffst+jz
      cc3=cc2*Cc(k)
      if(Kml.EQ.0)Lend=k
      if(Imkjml.EQ.0.AND.i.EQ.k)Lend=j
      
      do 318 l=Lstart,Lend
      ijkl=ijkl+1
      dijkl=d1234(ijkl)*cc3*Cd(l)
      if(gabs(dijkl).GE.pqcut5)then
      lx=indx(l)+kx
      ly=indy(l)+ky
      lz=indz(l)+kz
      if(iforms.EQ.2)then
      
      fxiv=Zero
      fxkv=Zero
      fyiv=Zero
      fykv=Zero
      fziv=Zero
      fzkv=Zero
      do 310 izero=1,nzros
      lx=lx+nxyznt
      ly=ly+nxyznt
      lz=lz+nxyznt
      xyip=xip(lx)*yip(ly)
      xzip=xip(lx)*zip(lz)
      yzip=yip(ly)*zip(lz)
      fxiv=fxiv+xipi(lx)*yzip
      fxkv=fxkv+xipk(lx)*yzip
      fyiv=fyiv+yipi(ly)*xzip
      fykv=fykv+yipk(ly)*xzip
      fziv=fziv+zipi(lz)*xyip
      fzkv=fzkv+zipk(lz)*xyip
310   continue
      fxi=fxi+fxiv*dijkl
      fxk=fxk+fxkv*dijkl
      fyi=fyi+fyiv*dijkl
      fyk=fyk+fykv*dijkl
      fzi=fzi+fziv*dijkl
      fzk=fzk+fzkv*dijkl
      elseif(iforms.EQ.3.OR.iforms.EQ.5.OR.iforms.EQ.6)then
      
      fxiv=Zero
      fyiv=Zero
      fziv=Zero
      do 312 izero=1,nzros
      lx=lx+nxyznt
      ly=ly+nxyznt
      lz=lz+nxyznt
      fxiv=fxiv+xipi(lx)*yip(ly)*zip(lz)
      fyiv=fyiv+yipi(ly)*xip(lx)*zip(lz)
      fziv=fziv+zipi(lz)*xip(lx)*yip(ly)
312   continue
      fxi=fxi+fxiv*dijkl
      fyi=fyi+fyiv*dijkl
      fzi=fzi+fziv*dijkl
      elseif(iforms.EQ.4)then
      
      fxiv=Zero
      fxjv=Zero
      fyiv=Zero
      fyjv=Zero
      fziv=Zero
      fzjv=Zero
      do 314 izero=1,nzros
      lx=lx+nxyznt
      ly=ly+nxyznt
      lz=lz+nxyznt
      xyip=xip(lx)*yip(ly)
      xzip=xip(lx)*zip(lz)
      yzip=yip(ly)*zip(lz)
      fxiv=fxiv+xipi(lx)*yzip
      fxjv=fxjv+xipj(lx)*yzip
      fyiv=fyiv+yipi(ly)*xzip
      fyjv=fyjv+yipj(ly)*xzip
      fziv=fziv+zipi(lz)*xyip
      fzjv=fzjv+zipj(lz)*xyip
314   continue
      fxi=fxi+fxiv*dijkl
      fxj=fxj+fxjv*dijkl
      fyi=fyi+fyiv*dijkl
      fyj=fyj+fyjv*dijkl
      fzi=fzi+fziv*dijkl
      fzj=fzj+fzjv*dijkl
      else
      
      fxiv=Zero
      fxjv=Zero
      fxkv=Zero
      fyiv=Zero
      fyjv=Zero
      fykv=Zero
      fziv=Zero
      fzjv=Zero
      fzkv=Zero
      do 316 izero=1,nzros
      lx=lx+nxyznt
      ly=ly+nxyznt
      lz=lz+nxyznt
      xyip=xip(lx)*yip(ly)
      xzip=xip(lx)*zip(lz)
      yzip=yip(ly)*zip(lz)
      fxiv=fxiv+xipi(lx)*yzip
      fxjv=fxjv+xipj(lx)*yzip
      fxkv=fxkv+xipk(lx)*yzip
      fyiv=fyiv+yipi(ly)*xzip
      fyjv=fyjv+yipj(ly)*xzip
      fykv=fykv+yipk(ly)*xzip
      fziv=fziv+zipi(lz)*xyip
      fzjv=fzjv+zipj(lz)*xyip
      fzkv=fzkv+zipk(lz)*xyip
316   continue
      fxi=fxi+fxiv*dijkl
      fxj=fxj+fxjv*dijkl
      fxk=fxk+fxkv*dijkl
      fyi=fyi+fyiv*dijkl
      fyj=fyj+fyjv*dijkl
      fyk=fyk+fykv*dijkl
      fzi=fzi+fziv*dijkl
      fzj=fzj+fzjv*dijkl
      fzk=fzk+fzkv*dijkl
      endif
      endif
318   continue
320   continue
322   continue
324   continue
      endif
      endif
      
      
      
      
326   continue
328   continue
      endif
330   continue
332   continue
      
      fxl=-(fxi+fxj+fxk)
      fyl=-(fyi+fyj+fyk)
      fzl=-(fzi+fzj+fzk)
      FXYZ(1,iatms)=FXYZ(1,iatms)+fxi
      FXYZ(1,jatms)=FXYZ(1,jatms)+fxj
      FXYZ(1,katms)=FXYZ(1,katms)+fxk
      FXYZ(1,latms)=FXYZ(1,latms)+fxl
      FXYZ(2,iatms)=FXYZ(2,iatms)+fyi
      FXYZ(2,jatms)=FXYZ(2,jatms)+fyj
      FXYZ(2,katms)=FXYZ(2,katms)+fyk
      FXYZ(2,latms)=FXYZ(2,latms)+fyl
      FXYZ(3,iatms)=FXYZ(3,iatms)+fzi
      FXYZ(3,jatms)=FXYZ(3,jatms)+fzj
      FXYZ(3,katms)=FXYZ(3,katms)+fzk
      FXYZ(3,latms)=FXYZ(3,latms)+fzl
      if(IDUMP.GE.2)write(Iout,99001)ishell,jshell,kshell,lshell,fxi,fxj
     &,fxk,fxl,fyi,fyj,fyk,fyl,fzi,fzj,fzk,fzl
      if(USESYM)then
      
      
      do 334 kop=2,NSYMOP
      jop=isymop(kop)
      jopind=(jop-1)*NATOMS
      if(jop.NE.0)then
      iatms1=NEQATM(iatms+jopind)
      jatms1=NEQATM(jatms+jopind)
      katms1=NEQATM(katms+jopind)
      latms1=NEQATM(latms+jopind)
      ixtr=JTRANS(1,jop)
      iytr=JTRANS(2,jop)
      iztr=JTRANS(3,jop)
      FXYZ(1,iatms1)=FXYZ(1,iatms1)+fxi*ixtr
      FXYZ(1,jatms1)=FXYZ(1,jatms1)+fxj*ixtr
      FXYZ(1,katms1)=FXYZ(1,katms1)+fxk*ixtr
      FXYZ(1,latms1)=FXYZ(1,latms1)+fxl*ixtr
      FXYZ(2,iatms1)=FXYZ(2,iatms1)+fyi*iytr
      FXYZ(2,jatms1)=FXYZ(2,jatms1)+fyj*iytr
      FXYZ(2,katms1)=FXYZ(2,katms1)+fyk*iytr
      FXYZ(2,latms1)=FXYZ(2,latms1)+fyl*iytr
      FXYZ(3,iatms1)=FXYZ(3,iatms1)+fzi*iztr
      FXYZ(3,jatms1)=FXYZ(3,jatms1)+fzj*iztr
      FXYZ(3,katms1)=FXYZ(3,katms1)+fzk*iztr
      FXYZ(3,latms1)=FXYZ(3,latms1)+fzl*iztr
      if(IDUMP.GT.1)write(Iout,99002)jop,ixtr,iytr,iztr
      endif
334   continue
      endif
      endif
      endif
340   continue
360   continue
400   continue
500   continue
      if(IPRINT.GE.1)then
      write(Iout,99003)VEE
      write(Iout,99004)
      do 550 i=1,NATOMS
      write(Iout,99005)i,(FXYZ(l,i),l=1,3)
550   continue
      endif
      
      
      return
      
      end
C* :1 * 
      
