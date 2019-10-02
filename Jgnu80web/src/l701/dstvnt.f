
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dstvnt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dstvnt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 56 "dstvnt.web"
      subroutine dstvnt(NATOMS,ATMCHG,C,D1,D2,E1XX,F1XYZ,DXVNE1,DYVNE1,D
     &ZVNE1,DXVNE2,DYVNE2,DZVNE2,DXVNN,DYVNN,DZVNN,DXT,DYT,DZT,DXC,DYC,D
     &ZC,IPRINT,IDUMP)
      implicit none
      double precision A,abx,aby,abz,Aiab,arabsq,arg,as,asxa,asya,asza,A
     &TMCHG,Biab,bs,C,C1,C2,C3,C4,Ca
      double precision Cb,Cc,ccut,Ccx,Ccy,Ccz,Cd,cia,coef,D1,d12,d12c,d1
     &2s,d12sc,D2,Dabx,Daby,Dabz,dtemp,dtemps
      double precision DXC,dxk,dxs,DXT,dxv1,dxv2,DXVNE1,DXVNE2,DXVNN,DYC
     &,dyk,dys,DYT,dyv1,dyv2,DYVNE1,DYVNE2,DYVNN,DZC,dzk
      double precision dzs,DZT,dzv1,dzv2,DZVNE1,DZVNE2,DZVNN,E1XX,ep,epi
     &,Epio2,Exx,F100,f15,F1XYZ,f20,F20i,F42,F6i,Filla
      double precision Fillip,fn,Four,fxc,fxt,fxvne2,fxvne3,fyc,fyt,fyvn
     &e2,fyvne3,fzc,fzt,fzvne2,fzvne3,gabs,gatan,gexp,gsqrt,Half
      integer i,i1,ia,iaind,iatm,iatom,IDUMP,Iend,igauss,igbegn,igdf,ige
     &nd,ij,ijx,ijy,ijz,Imj,In,indix,indiy
      integer indiz,indjx,indjy,indjz,Inew,intc,Iout,IPRINT,Ipunch,Ipurd
     &,Ipurf,Irange,ishell,ist,Istart,Itype,ix,ixyz,iy,iz
      integer izero,j,ja,jaind,Jan,Jend,jgauss,jgbegn,jgdf,jgend,jnd,Jne
     &w,Jnktyp,Jrange,jshell,jst,Jstart,Jtype,Lamax,lax
      integer laxm,laxp,lay,laym,layp,laz,lazm,lazp,Lbmax,Lbound,lbx,lby
     &,lbz,LENB,Lentq,lim1ds,Limdum,Lpmax,Maxdum,MAXPRM
      integer MAXS21,MAXSH1,MAXSHL,Maxtyp,mu,munu,mus,N10ord,N5ord,N6ord
     &,N7ord,na,NATOMS,nb,Nordr,Nshell,nu,nzero
      double precision One,Onept5,Pcx,Pcy,Pcz,pexp,pi,pi3haf,Pt5,px,py,p
     &z,R1,R2,R3,R3ov2,R4,rabsq,Root15,Root3
      double precision Root5,rootpi,rpcsq,s,S1c,sterm,sums,sx,sy,sz,t,te
     &mp,temp1,temp2,Ten,Three,tp,Two,twoas,twoasq
      double precision Twocx,Twocy,Twocz,twop,twopi,twopt2,txx,vnexx,vnn
     &xx,vtemp,wp,X,xa,xap,xb,xbp,xc,Xint,xip,xip1
      double precision xip2,xyip,xzip,Y,ya,yap,yb,ybp,yc,yip,yip1,yip2,y
     &zip,Z,Z1,Z2,Z3,za,zap,zb
      double precision zbp,zc,zconst,Zero,Zero1,zip,zip1,zip2,zt,ztemp
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      integer scona,sconb
      integer Ubound,Ulpure
      dimension indix(20),indiy(20),indiz(20),indjx(20),indjy(20),indjz(
     &20)
      dimension tp(4),wp(4)
      dimension ATMCHG(1),C(1),F1XYZ(1),D1(1),D2(1)
      dimension DXVNE1(1),DYVNE1(1),DZVNE1(1),DXT(1),DYT(1),DZT(1)
      dimension DXVNE2(1),DYVNE2(1),DZVNE2(1),DXC(1),DYC(1),DZC(1)
      dimension DXVNN(1),DYVNN(1),DZVNN(1)
      dimension d12(100),d12s(100),d12c(100),d12sc(100)
      dimension xip(80),yip(80),zip(80),xip1(80),yip1(80),zip1(80),xip2(
     &80),yip2(80),zip2(80)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      dimension sx(36),sy(36),sz(36),iatm(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Limdu
     &m(11)
      common/type/Itype,Jtype,Jnktyp(10)
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      common/io/In,Iout,Ipunch
      common/contr/Ca(20),Cb(20),Cc(20),Cd(20)
      common/twoc/Twocx(7),Twocy(7),Twocz(7),S1c(9)
      common/int/Zero1,Xint(12)
      common/new/Inew,Jnew
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/ipdrv/Aiab,Biab,Epio2,Pcx,Pcy,Pcz,Dabx,Daby,Dabz,Fillip(54)
      common/ipure/Ipurd,Ipurf
      common/intcon/F6i,F20i,F100
      common/a/A(45),Filla(129)
      common/cc/Ccx(120),Ccy(120),Ccz(120)
      data indjx/1,2,1,1,3,1,1,2,2,1,4,1,1,2,3,3,2,1,1,2/
      data indjy/1,1,2,1,1,3,1,2,1,2,1,4,1,3,2,1,1,2,3,2/
      data indjz/1,1,1,2,1,1,3,1,2,2,1,1,4,1,1,2,3,3,2,2/
      data f20/20.0D0/
      data f15/15.0D0/
      data ccut/1.D-6/
      
      
      
      
      
      
      
      
      
      
      
99001 format(' V1,V2',2I3,6F15.9)
99002 format(' DK,DS',2I3,6F15.9)
99003 format(' ',40I3)
99004 format(' VNN ',3F20.10)
99005 format(' VNE1',3F20.10)
99006 format(' VNE2',3F20.10)
99007 format(' T   ',3F20.10)
99008 format(' C   ',3F20.10)
99009 format(' T=',f15.8,' VNE=',f15.8,' VNN=',f15.8,' SUMS=',f15.8)
      
      
      
      call setr1
      pi=Four*gatan(One)
      twopi=pi+pi
      rootpi=gsqrt(pi)
      pi3haf=pi*rootpi
      F6i=One/Xint(6)
      F20i=One/f20
      
      Root3=gsqrt(Xint(3))
      R3ov2=Half*Root3
      Root5=gsqrt(Xint(5))
      Root15=gsqrt(f15)
      R1=Pt5*gsqrt(Xint(5)/Xint(2))
      R2=Xint(3)/(Xint(2)*Root5)
      R4=Pt5*gsqrt(Xint(3)/Xint(2))
      Z1=Xint(4)/Root5
      Z2=Xint(1)/Root5
      Z3=Xint(3)/Root5
      
      do 100 i=1,20
      indix(i)=4*(indjx(i)-1)
      indiy(i)=4*(indjy(i)-1)
      indiz(i)=4*(indjz(i)-1)
100   continue
      
      call setord
      
      ia=1
      iaind=0
      do 200 i=1,Nshell
      if(gabs(X(i)-C(1+iaind)).LE.ccut)then
      if(gabs(Y(i)-C(2+iaind)).LE.ccut)then
      if(gabs(Z(i)-C(3+iaind)).LE.ccut)goto 150
      endif
      endif
      
      ia=ia+1
      iaind=iaind+3
150   iatm(i)=ia
200   continue
      
      txx=Zero
      vnexx=Zero
      vnnxx=Zero
      sums=Zero
      do 300 i=1,NATOMS
      
      DXVNE1(i)=Zero
      DYVNE1(i)=Zero
      DZVNE1(i)=Zero
      DXVNE2(i)=Zero
      DYVNE2(i)=Zero
      DZVNE2(i)=Zero
      DXVNN(i)=Zero
      DYVNN(i)=Zero
      DZVNN(i)=Zero
      DXT(i)=Zero
      DYT(i)=Zero
      DZT(i)=Zero
      DXC(i)=Zero
      DYC(i)=Zero
      DZC(i)=Zero
300   continue
      
      do 400 i=1,NATOMS
      i1=i-1
      if(i1.NE.0)then
      do 320 j=1,i1
      iaind=(i-1)*3
      jaind=(j-1)*3
      abx=C(1+iaind)-C(1+jaind)
      aby=C(2+iaind)-C(2+jaind)
      abz=C(3+iaind)-C(3+jaind)
      rabsq=abx**2+aby**2+abz**2
      fn=(ATMCHG(i)*ATMCHG(j))/gsqrt(rabsq)
      vnnxx=vnnxx+fn
      fn=fn/rabsq
      DXVNN(i)=DXVNN(i)-abx*fn
      DXVNN(j)=DXVNN(j)+abx*fn
      DYVNN(i)=DYVNN(i)-aby*fn
      DYVNN(j)=DYVNN(j)+aby*fn
      DZVNN(i)=DZVNN(i)-abz*fn
      DZVNN(j)=DZVNN(j)+abz*fn
320   continue
      endif
400   continue
      
      
      
      
      do 600 ishell=1,Nshell
      Inew=ishell
      xa=X(ishell)
      ya=Y(ishell)
      za=Z(ishell)
      igbegn=Shella(ishell)
      igend=igbegn+Shelln(ishell)-1
      na=Shelln(ishell)
      Itype=Shellt(ishell)
      Lamax=Itype+1
      scona=Shellc(ishell)
      Iend=Ubound(Lamax)
      Istart=Lbound(Lamax,scona+1)
      igdf=Shladf(Inew)
      
      
      do 500 jshell=1,ishell
      Jnew=jshell
      xb=X(jshell)
      yb=Y(jshell)
      zb=Z(jshell)
      jgbegn=Shella(jshell)
      jgend=jgbegn+Shelln(jshell)-1
      nb=Shelln(jshell)
      Jtype=Shellt(jshell)
      Lbmax=Jtype+1
      sconb=Shellc(jshell)
      Jstart=Lbound(Lbmax,sconb+1)
      Jend=Ubound(Lbmax)
      Jrange=Jend-Jstart+1
      jgdf=Shladf(Jnew)
      
      Lpmax=Lamax+Lbmax-1
      lim1ds=(Lpmax+4)/2
      Imj=iabs(ishell-jshell)
      nzero=(Itype+Jtype+1)/2+1
      abx=xb-xa
      aby=yb-ya
      abz=zb-za
      rabsq=abx*abx+aby*aby+abz*abz
      
      ist=Aos(ishell)-1
      jst=Aos(jshell)-1
      intc=0
      do 420 i=Istart,Iend
      mu=ist+Nordr(i)
      mus=(mu*(mu-1))/2
      jnd=Jend
      if(Imj.EQ.0)jnd=i
      do 410 j=Jstart,jnd
      nu=jst+Nordr(j)
      munu=mus+nu
      intc=intc+1
      d12(intc)=D1(munu)
      d12s(intc)=D2(munu)
      if(mu.NE.nu)then
      d12(intc)=d12(intc)+d12(intc)
      d12s(intc)=d12s(intc)+d12s(intc)
      endif
410   continue
420   continue
      
      fxvne2=Zero
      fyvne2=Zero
      fzvne2=Zero
      fxvne3=Zero
      fyvne3=Zero
      fzvne3=Zero
      fxt=Zero
      fyt=Zero
      fzt=Zero
      fxc=Zero
      fyc=Zero
      fzc=Zero
      
      
      do 460 igauss=igbegn,igend
      as=Exx(igauss)
      twoasq=Two*as*as
      twoas=as+as
      asxa=as*xa
      asya=as*ya
      asza=as*za
      arabsq=as*rabsq
      call fillc(Itype,igbegn,igauss,igdf,Ca)
      
      do 450 jgauss=jgbegn,jgend
      bs=Exx(jgauss)
      call fillc(Jtype,jgbegn,jgauss,jgdf,Cb)
      
      ep=as+bs
      epi=One/ep
      Epio2=Half*epi
      twop=ep+ep
      pexp=gexp(-bs*arabsq*epi)
      ztemp=twopi*epi*pexp
      px=(asxa+bs*xb)*epi
      py=(asya+bs*yb)*epi
      pz=(asza+bs*zb)*epi
      
      xap=px-xa
      xbp=px-xb
      yap=py-ya
      ybp=py-yb
      zap=pz-za
      zbp=pz-zb
      
      call getcc1(Ccx,xap,xbp,3)
      call getcc1(Ccy,yap,ybp,3)
      call getcc1(Ccz,zap,zbp,3)
      Aiab=as*epi
      Biab=bs*epi
      temp=(as+as)*Biab
      Dabx=abx*temp
      Daby=aby*temp
      Dabz=abz*temp
      
      intc=0
      do 425 i=Istart,Iend
      jnd=Jend
      if(Imj.EQ.0)jnd=i
      do 422 j=Jstart,jnd
      intc=intc+1
      coef=Ca(i)*Cb(j)
      d12c(intc)=d12(intc)*coef
      d12sc(intc)=d12s(intc)*coef
422   continue
425   continue
      
      
      iaind=-3
      do 435 iatom=1,NATOMS
      
      iaind=iaind+3
      xc=C(1+iaind)
      yc=C(2+iaind)
      zc=C(3+iaind)
      cia=ATMCHG(iatom)
      zt=-ztemp*cia
      Pcx=xc-px
      Pcy=yc-py
      Pcz=zc-pz
      rpcsq=Pcx*Pcx+Pcy*Pcy+Pcz*Pcz
      arg=ep*rpcsq
      call rpol1(nzero,arg,tp,wp)
      call geta1(A,Epio2,0)
      
      
      ixyz=-15
      do 426 izero=1,nzero
      
      twopt2=twop*tp(izero)
      zconst=zt*wp(izero)
      
      call get2c(Twocx,Pcx,One,A,twopt2,0)
      call get2c(Twocy,Pcy,One,A,twopt2,0)
      call get2c(Twocz,Pcz,zconst,A,twopt2,0)
      
      ixyz=ixyz+16
      call get3c(xip(ixyz),Twocx,Ccx)
      call get3c(yip(ixyz),Twocy,Ccy)
      call get3c(zip(ixyz),Twocz,Ccz)
      call drvip1(xip,yip,zip,xip1,yip1,zip1,xip2,yip2,zip2,twopt2,ixyz)
426   continue
      
      
      intc=0
      do 432 i=Istart,Iend
      
      ix=indix(i)-16
      iy=indiy(i)-16
      iz=indiz(i)-16
      
      jnd=Jend
      if(Imj.EQ.0)jnd=i
      do 430 j=Jstart,jnd
      ijx=indjx(j)+ix
      ijy=indjy(j)+iy
      ijz=indjz(j)+iz
      
      intc=intc+1
      dtemp=d12c(intc)
      vtemp=Zero
      dxv1=Zero
      dyv1=Zero
      dzv1=Zero
      dzv2=Zero
      dyv2=Zero
      dxv2=Zero
      do 428 izero=1,nzero
      ijx=ijx+16
      ijy=ijy+16
      ijz=ijz+16
      xyip=xip(ijx)*yip(ijy)
      xzip=xip(ijx)*zip(ijz)
      yzip=yip(ijy)*zip(ijz)
      vtemp=vtemp+xyip*zip(ijz)
      dxv1=dxv1+xip1(ijx)*yzip
      dyv1=dyv1+yip1(ijy)*xzip
      dzv1=dzv1+zip1(ijz)*xyip
      dxv2=dxv2+xip2(ijx)*yzip
      dyv2=dyv2+yip2(ijy)*xzip
      dzv2=dzv2+zip2(ijz)*xyip
428   continue
      if(IDUMP.GE.2)write(Iout,99001)i,j,dxv1,dyv1,dzv1,dxv2,dyv2,dzv2
      vnexx=vnexx+vtemp*dtemp
      temp1=dxv1*dtemp
      temp2=dxv2*dtemp
      DXVNE1(iatom)=DXVNE1(iatom)+temp1
      fxvne2=fxvne2+temp2
      fxvne3=fxvne3-temp1-temp2
      temp1=dyv1*dtemp
      temp2=dyv2*dtemp
      DYVNE1(iatom)=DYVNE1(iatom)+temp1
      fyvne2=fyvne2+temp2
      fyvne3=fyvne3-temp1-temp2
      temp1=dzv1*dtemp
      temp2=dzv2*dtemp
      DZVNE1(iatom)=DZVNE1(iatom)+temp1
      fzvne2=fzvne2+temp2
      fzvne3=fzvne3-temp1-temp2
      
430   continue
432   continue
      
435   continue
      
      sterm=rootpi*gsqrt(epi)
      call get1cs(S1c,sterm,Epio2,3)
      
      call get2cs(sx,S1c,Ccx,3)
      call get2cs(sy,S1c,Ccy,3)
      do 440 i=1,lim1ds
      S1c(i)=S1c(i)*pexp
440   continue
      call get2cs(sz,S1c,Ccz,3)
      
      
      intc=0
      do 445 i=Istart,Iend
      lax=indjx(i)
      lay=indjy(i)
      laz=indjz(i)
      laxp=lax+1
      layp=lay+1
      lazp=laz+1
      laxm=lax-1
      laym=lay-1
      lazm=laz-1
      jnd=Jend
      if(Imj.EQ.0)jnd=i
      do 442 j=Jstart,jnd
      lbx=indjx(j)
      lby=indjy(j)
      lbz=indjz(j)
      intc=intc+1
      dtemp=d12c(intc)
      dtemps=d12sc(intc)
      call rawst(s,t,lax,lay,laz,lbx,lby,lbz,as,twoasq,sx,sy,sz)
      txx=txx+t*dtemp
      sums=sums+s*dtemp
      call rawst(s,t,laxp,lay,laz,lbx,lby,lbz,as,twoasq,sx,sy,sz)
      dxs=twoas*s
      dxk=twoas*t
      call rawst(s,t,lax,layp,laz,lbx,lby,lbz,as,twoasq,sx,sy,sz)
      dys=twoas*s
      dyk=twoas*t
      call rawst(s,t,lax,lay,lazp,lbx,lby,lbz,as,twoasq,sx,sy,sz)
      dzs=twoas*s
      dzk=twoas*t
      if(lax.NE.1)then
      call rawst(s,t,laxm,lay,laz,lbx,lby,lbz,as,twoasq,sx,sy,sz)
      dxs=dxs-Xint(laxm)*s
      dxk=dxk-Xint(laxm)*t
      endif
      if(lay.NE.1)then
      call rawst(s,t,lax,laym,laz,lbx,lby,lbz,as,twoasq,sx,sy,sz)
      dys=dys-Xint(laym)*s
      dyk=dyk-Xint(laym)*t
      endif
      if(laz.NE.1)then
      call rawst(s,t,lax,lay,lazm,lbx,lby,lbz,as,twoasq,sx,sy,sz)
      dzs=dzs-Xint(lazm)*s
      dzk=dzk-Xint(lazm)*t
      endif
      if(IDUMP.GE.2)write(Iout,99002)i,j,dxk,dyk,dzk,dxs,dys,dzs
      fxt=fxt+dxk*dtemp
      fyt=fyt+dyk*dtemp
      fzt=fzt+dzk*dtemp
      fxc=fxc+dxs*dtemps
      fyc=fyc+dys*dtemps
      fzc=fzc+dzs*dtemps
442   continue
445   continue
      
450   continue
460   continue
      
      ia=iatm(ishell)
      DXVNE2(ia)=DXVNE2(ia)+fxvne2
      DYVNE2(ia)=DYVNE2(ia)+fyvne2
      DZVNE2(ia)=DZVNE2(ia)+fzvne2
      DXT(ia)=DXT(ia)+fxt
      DYT(ia)=DYT(ia)+fyt
      DZT(ia)=DZT(ia)+fzt
      DXC(ia)=DXC(ia)+fxc
      DYC(ia)=DYC(ia)+fyc
      DZC(ia)=DZC(ia)+fzc
      ja=iatm(jshell)
      DXVNE2(ja)=DXVNE2(ja)+fxvne3
      DYVNE2(ja)=DYVNE2(ja)+fyvne3
      DZVNE2(ja)=DZVNE2(ja)+fzvne3
      DXT(ja)=DXT(ja)-fxt
      DYT(ja)=DYT(ja)-fyt
      DZT(ja)=DZT(ja)-fzt
      DXC(ja)=DXC(ja)-fxc
      DYC(ja)=DYC(ja)-fyc
      DZC(ja)=DZC(ja)-fzc
500   continue
600   continue
      
      E1XX=txx+vnexx+vnnxx
      ij=0
      do 700 i=1,NATOMS
      ij=ij+1
      F1XYZ(ij)=DXVNN(i)+DXVNE1(i)+DXVNE2(i)+DXT(i)+DXC(i)
      ij=ij+1
      F1XYZ(ij)=DYVNN(i)+DYVNE1(i)+DYVNE2(i)+DYT(i)+DYC(i)
      ij=ij+1
      F1XYZ(ij)=DZVNN(i)+DZVNE1(i)+DZVNE2(i)+DZT(i)+DZC(i)
      if(IPRINT.NE.0)then
      write(Iout,99003)i
      write(Iout,99004)DXVNN(i),DYVNN(i),DZVNN(i)
      write(Iout,99005)DXVNE1(i),DYVNE1(i),DZVNE1(i)
      write(Iout,99006)DXVNE2(i),DYVNE2(i),DZVNE2(i)
      write(Iout,99007)DXT(i),DYT(i),DZT(i)
      write(Iout,99008)DXC(i),DYC(i),DZC(i)
      endif
700   continue
      if(IPRINT.NE.0)write(Iout,99009)txx,vnexx,vnnxx,sums
      
      return
      
      end
C* :1 * 
      
