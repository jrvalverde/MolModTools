
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dipole"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dipole.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 36 "dipole.web"
      subroutine dipole(JUMP)
      implicit none
      double precision abx,aby,abz,arabsq,as,asxa,asya,asza,Atmchg,bs,C,
     &C1,C2,C3,C4,Ca,Cb,Cc,Ccx,Ccy
      double precision Ccz,Cd,coef,Ddum,Ddx,Ddy,Ddz,Dx,Dy,Dz,ep,epi,epio
     &2,Exx,f15,F42,Fillnt,Four,gatan,gexp
      double precision gsqrt,Half,One,Onept5,pexp,pi,pi3haf,Pt5,px,py,pz
     &,R1,R2,R3,R3ov2,R4,rabsq,Root15,Root3,Root5
      double precision rootpi,S1c,sterm,Sx,Sy,Sz,Ten,Three,Two,twopi,X,x
     &a,xap,xb,xbp,Xint,Y,ya,yap,yb
      double precision ybp,Z,Z1,Z2,Z3,za,zap,zb,zbp,Zero,Zero1
      integer i,Ian,Icharg,idump,Iend,Ifilla,ifprt,igauss,igbegn,igdf,ig
     &end,Imj,In,Indix,Indiy,Indiz,Indjx,Indjy,Indjz,Inew
      integer intc,Iop,Iout,iprint,Ipunch,Ipurd,Ipurf,Irange,irwb,irwx,i
     &rwy,irwz,ishell,Istart,Itype,ix,ixnew,iy,iynew,iz
      integer iznew,j,Jan,Jend,jgauss,jgbegn,jgdf,jgend,Jnew,Jnktyp,jpri
     &nt,Jrange,jshell,Jstart,Jtype,JUMP,jx,jy,jz,Lamax
      integer Lbmax,Lbound,LENB,Lentq,lim1ds,Limdum,Lind,Lpmax,Maxdum,MA
     &XPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,Mdim,Multip,N10ord,N5ord,N6ord,N7
     &ord
      integer na,Nae,Natoms,nb,Nbasis,nbasp,Nbe,Ne,Nordr,Nshell,ntt
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      integer scona,sconb
      integer Ubound,Ulpure
      dimension ifprt(8)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Limdu
     &m(11)
      common/type/Itype,Jtype,Jnktyp(10)
      common/memry/Dx(16666),Dy(16666),Dz(16668)
      common/ia/Lind(164),Ifilla(92)
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/int/Zero1,Xint(12)
      common/mdim/Mdim
      common/inds/Indjx(20),Indjy(20),Indjz(20),Indix(20),Indiy(20),Indi
     &z(20)
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      common/io/In,Iout,Ipunch
      common/contr/Ca(20),Cb(20),Cc(20),Cd(20)
      common/new/Inew,Jnew
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/xyzint/Sx(20),Sy(20),Sz(20),S1c(8),Fillnt(700)
      common/block/Ddx(100),Ddy(100),Ddz(100),Ddum(100)
      common/cc/Ccx(120),Ccy(120),Ccz(120)
      common/ipure/Ipurd,Ipurf
      data f15/15.0D0/
      data ifprt/0,1,2,0,0,1,1,2/
      data irwb/506/
      data irwx/518/,irwy/519/,irwz/520/
      
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(42H ********** THE Y DIPOLE MATRIX **********)
99002 format(42H ********** THE Z DIPOLE MATRIX **********)
99003 format(42H ********** THE X DIPOLE MATRIX **********)
      
      
      call drum
      pi=Four*gatan(One)
      twopi=pi+pi
      rootpi=gsqrt(pi)
      pi3haf=pi*rootpi
      
      
      jprint=Iop(33)
      iprint=Iop(33)+1
      iprint=ifprt(iprint)
      call ilsw(2,2,Ipurd)
      call ilsw(2,16,Ipurf)
      idump=Iop(34)
      if(idump.NE.0)iprint=2
      
      
      call tread(irwb,Exx,LENB,1,LENB,1,0)
      if(idump.NE.0)call bdump(2)
      
      ntt=Nbasis*(Nbasis+1)/2
      
      Root3=gsqrt(Xint(3))
      R3ov2=Half*Root3
      Root5=gsqrt(Xint(5))
      Root15=gsqrt(f15)
      R1=Pt5*gsqrt(Xint(5)/Xint(2))
      R2=Xint(3)/(Xint(2)*Root5)
      R3=R3ov2
      R4=Pt5*gsqrt(Xint(3)/Xint(2))
      Z1=Xint(4)/Root5
      Z2=Xint(1)/Root5
      Z3=Xint(3)/Root5
      
      do 100 i=1,20
      Indix(i)=4*(Indjx(i)-1)
      Indiy(i)=4*(Indjy(i)-1)
      Indiz(i)=4*(Indjz(i)-1)
100   continue
      
      
      
      do 200 i=1,ntt
      Dx(i)=Zero
      Dy(i)=Zero
      Dz(i)=Zero
200   continue
      
      call setord
      
      
      nbasp=Nbasis+1
      do 300 i=1,nbasp
      Lind(i)=(i*(i-1))/2
300   continue
      
      
      
      do 500 ishell=1,Nshell
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
      Irange=Iend-Istart+1
      igdf=Shladf(Inew)
      
      
      do 400 jshell=1,ishell
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
      Lentq=Irange*Jrange
      lim1ds=(Lpmax+2)/2
      Imj=iabs(ishell-jshell)
      abx=xb-xa
      aby=yb-ya
      abz=zb-za
      rabsq=abx*abx+aby*aby+abz*abz
      do 320 i=1,Lentq
      Ddx(i)=Zero
      Ddy(i)=Zero
      Ddz(i)=Zero
320   continue
      
      
      do 360 igauss=igbegn,igend
      as=Exx(igauss)
      asxa=as*xa
      asya=as*ya
      asza=as*za
      arabsq=as*rabsq
      call fillc(Itype,igbegn,igauss,igdf,Ca)
      
      do 340 jgauss=jgbegn,jgend
      bs=Exx(jgauss)
      call fillc(Jtype,jgbegn,jgauss,jgdf,Cb)
      
      ep=as+bs
      epi=One/ep
      px=(asxa+bs*xb)*epi
      py=(asya+bs*yb)*epi
      pz=(asza+bs*zb)*epi
      xap=px-xa
      yap=py-ya
      zap=pz-za
      xbp=px-xb
      ybp=py-yb
      zbp=pz-zb
      epio2=epi*Half
      pexp=gexp(-bs*arabsq*epi)
      
      
      
      call getcc1(Ccx,xap,xbp,1)
      call getcc1(Ccy,yap,ybp,1)
      call getcc1(Ccz,zap,zbp,1)
      
      sterm=rootpi*gsqrt(epi)
      call get1cs(S1c,sterm,epio2,1)
      
      call get2cs(Sx,S1c,Ccx,1)
      call get2cs(Sy,S1c,Ccy,1)
      do 325 i=1,lim1ds
      S1c(i)=S1c(i)*pexp
325   continue
      call get2cs(Sz,S1c,Ccz,1)
      
      
      intc=0
      do 330 i=Istart,Iend
      ix=Indix(i)
      iy=Indiy(i)
      iz=Indiz(i)
      ixnew=ix+4
      iynew=iy+4
      iznew=iz+4
      do 326 j=Jstart,Jend
      jx=Indjx(j)
      jy=Indjy(j)
      jz=Indjz(j)
      intc=intc+1
      
      coef=Ca(i)*Cb(j)
      Ddx(intc)=Ddx(intc)+coef*Sy(iy+jy)*Sz(iz+jz)*(xa*Sx(ix+jx)+Sx(ixne
     &w+jx))
      Ddy(intc)=Ddy(intc)+coef*Sx(ix+jx)*Sz(iz+jz)*(ya*Sy(iy+jy)+Sy(iyne
     &w+jy))
      Ddz(intc)=Ddz(intc)+coef*Sx(ix+jx)*Sy(iy+jy)*(za*Sz(iz+jz)+Sz(izne
     &w+jz))
326   continue
330   continue
      
340   continue
360   continue
      
      
      call filmat(Ddx,Dx)
      call filmat(Ddy,Dy)
      call filmat(Ddz,Dz)
      
400   continue
500   continue
      
      if(iprint.GT.0)then
      write(Iout,99003)
      call linout(Dx,Nbasis,0)
      write(Iout,99001)
      call linout(Dy,Nbasis,0)
      write(Iout,99002)
      call linout(Dz,Nbasis,0)
      endif
      call twrite(irwx,Dx,ntt,1,ntt,1,0)
      call twrite(irwy,Dy,ntt,1,ntt,1,0)
      call twrite(irwz,Dz,ntt,1,ntt,1,0)
      
      
      call fermi(Nbasis,Natoms,C,Dx,jprint)
      
      JUMP=0
      return
      
      end
C* :1 * 
      
