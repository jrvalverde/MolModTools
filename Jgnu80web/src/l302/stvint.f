
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 stvint"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "stvint.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 38 "stvint.web"
      subroutine stvint(JUMP)
      implicit none
      double precision A,abx,aby,abz,arabsq,arg,as,asxa,asya,asza,Atmchg
     &,bs,C,C1,C2,C3,C4,Ca,Cb,Cc
      double precision Ccx,Ccy,Ccz,Cd,cia,coef,Eek,Eep,ep,epi,epio2,Exx,
     &F100,f15,f20,F20i,F42,F6i,Filla,Four
      double precision gatan,gexp,gfloat,gsqrt,Half,One,Onept5,pcx,pcy,p
     &cz,pexp,pi,pi3haf,Pt5,px,py,pz,R1,R2,R3
      double precision R3ov2,R4,rabsq,Root15,Root3,Root5,rootpi,rpcsq,S,
     &S1c,Ss,sterm,Sx,Sy,syz,Sz,T,tempk,temps,Ten
      double precision Three,tk,tp,Two,twoasq,Twocx,Twocy,Twocz,twop,two
     &pi,twopt2,V,wp,X,xa,xap,xb,xbp,xc,xiim1
      double precision Xint,Xip,xip1,xk,Y,ya,yap,yb,ybp,yc,yiim1,Yip,yip
     &1,Z,Z1,Z2,Z3,za,zap,zb
      double precision zbp,zc,zconst,Zero,Zero1,ziim1,Zip,zip1,zt,ztemp
      integer i,iaind,Ian,iatom,Icharg,Idum2,Idummy,idump,Iend,Ifilla,if
     &prt,igauss,igbegn,igdf,igend,ijx,ijy,ijz,Ilsw1,Imj
      integer In,indix,indiy,indiz,indjx,indjy,indjz,Inew,intc,Iop,Iout,
     &iprint,Ipunch,Ipurd,Ipurf,Irange,irwb,irwh,irws,irwt
      integer ishell,Istart,Itype,ix,ixp,iy,iyp,iz,izero,izp,j,Jan,Jend,
     &jgauss,jgbegn,jgdf,jgend,Jnew,Jnktyp,Jrange
      integer jshell,Jstart,Jtype,JUMP,jx,jy,jz,Lamax,Lbmax,Lbound,LENB,
     &Lentq,lim1,lim1ds,Limdum,Lind,Lpmax,Maxdum,MAXPRM,MAXS21
      integer MAXSH1,MAXSHL,Maxtyp,Multip,N10ord,N5ord,N6ord,N7ord,na,Na
     &e,Natoms,nb,Nbasis,nbasp,Nbe,nbfmax,Ne,Nordr,Nshell,ntt
      integer nzero
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      integer Ubound,Ulpure
      integer scona,sconb
      dimension tp(4),wp(4)
      dimension indix(20),indiy(20),indiz(20),indjx(20),indjy(20),indjz(
     &20)
      dimension ifprt(8)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/dump/Idummy,Idum2
      common/iop/Iop(50)
      common/ilsw1/Ilsw1(2)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Limdu
     &m(11)
      common/type/Itype,Jtype,Jnktyp(10)
      common/memry/S(16666),T(16666),V(16668)
      common/ia/Lind(164),Ifilla(92)
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      common/int/Zero1,Xint(12)
      common/io/In,Iout,Ipunch
      common/contr/Ca(20),Cb(20),Cc(20),Cd(20)
      common/twoc/Twocx(7),Twocy(7),Twocz(7),S1c(9)
      common/threec/Xip(16),Yip(16),Zip(16),Sx(36),Sy(36),Sz(36)
      common/new/Inew,Jnew
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/block/Ss(100),Eek(100),Eep(200)
      common/ipure/Ipurd,Ipurf
      common/intcon/F6i,F20i,F100
      common/a/A(45),Filla(129)
      common/cc/Ccx(120),Ccy(120),Ccz(120)
      data indjx/1,2,1,1,3,1,1,2,2,1,4,1,1,2,3,3,2,1,1,2/
      data indjy/1,1,2,1,1,3,1,2,1,2,1,4,1,3,2,1,1,2,3,2/
      data indjz/1,1,1,2,1,1,3,1,2,2,1,1,4,1,1,2,3,3,2,2/
      data f15/15.0D0/
      data f20/20.0D0/
      data ifprt/0,1,2,0,0,1,1,2/
      data irwb/506/,irws/514/,irwt/516/,irwh/515/
      data nbfmax/164/
      
      
      
      
      
      
      
      
      
      
99001 format(37H ********** KINETIC ENERGY **********)
99002 format(39H ********** POTENTIAL ENERGY **********)
99003 format(39H ********** CORE HAMILTONIAN **********)
99004 format(30H ********** OVERLAP **********)
99005 format(' TOO MANY BASIS FUNCTIONS: ',i6)
      
      call drum
      
      if(Nbasis.GT.nbfmax)then
      write(Iout,99005)Nbasis
      call lnk1e
      endif
      
      call rysset(6,0)
      pi=Four*gatan(One)
      twopi=pi+pi
      rootpi=gsqrt(pi)
      pi3haf=pi*rootpi
      F6i=One/Xint(6)
      F20i=One/f20
      
      
      iprint=Iop(33)+1
      iprint=ifprt(iprint)
      call ilsw(2,2,Ipurd)
      call ilsw(2,16,Ipurf)
      idump=Iop(34)
      if(idump.GE.2)iprint=2
      
      
      call tread(irwb,Exx,LENB,1,LENB,1,0)
      if(idump.GE.2)call bdump(2)
      
      ntt=Nbasis*(Nbasis+1)/2
      
      Root3=gsqrt(Xint(3))
      R3ov2=Half*Root3
      R3=Half*Root3
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
      
      
      call aclear(ntt,S)
      call aclear(ntt,T)
      call aclear(ntt,V)
      
      call aclear(30,Twocx)
      call aclear(45,A)
      call aclear(360,Ccx)
      call aclear(40,Ca)
      call aclear(156,Xip)
      
      call setord
      
      
      nbasp=Nbasis+1
      do 200 i=1,nbasp
      Lind(i)=(i*(i-1))/2
200   continue
      
      
      
      do 400 ishell=1,Nshell
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
      
      
      do 300 jshell=1,ishell
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
      lim1=Lentq+100
      lim1ds=(Lpmax+3)/2
      Imj=iabs(ishell-jshell)
      nzero=(Itype+Jtype)/2+1
      abx=xb-xa
      aby=yb-ya
      abz=zb-za
      rabsq=abx*abx+aby*aby+abz*abz
      if(idump.EQ.11)write(6,99006)Lentq
      
99006 format(' LENTQ=',i6)
      
      do 220 i=1,Lentq
      Ss(i)=Zero
      Eek(i)=Zero
      Eep(i)=Zero
220   continue
      
      
      do 260 igauss=igbegn,igend
      as=Exx(igauss)
      twoasq=Two*as*as
      asxa=as*xa
      asya=as*ya
      asza=as*za
      arabsq=as*rabsq
      call fillc(Itype,igbegn,igauss,igdf,Ca)
      
      do 250 jgauss=jgbegn,jgend
      bs=Exx(jgauss)
      call fillc(Jtype,jgbegn,jgauss,jgdf,Cb)
      
      ep=as+bs
      epi=One/ep
      epio2=Half*epi
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
      
      call getcc1(Ccx,xap,xbp,2)
      call getcc1(Ccy,yap,ybp,2)
      call getcc1(Ccz,zap,zbp,2)
      
      
      if(idump.EQ.11)write(6,99007)lim1
      
99007 format(' LIM1 =',i6)
      
      do 225 i=101,lim1
      Eep(i)=Zero
225   continue
      
      
      do 235 iatom=1,Natoms
      
      iaind=(iatom-1)*3
      xc=C(1+iaind)
      yc=C(2+iaind)
      zc=C(3+iaind)
      cia=Atmchg(iatom)
      zt=ztemp*cia
      pcx=xc-px
      pcy=yc-py
      pcz=zc-pz
      rpcsq=pcx*pcx+pcy*pcy+pcz*pcz
      arg=ep*rpcsq
      call ryspol(nzero,arg,tp,wp,Iop(34))
      call geta1(A,epio2,0)
      
      
      do 230 izero=1,nzero
      
      twopt2=twop*tp(izero)
      zconst=zt*wp(izero)
      
      call get2c(Twocx,pcx,One,A,twopt2,0)
      call get2c(Twocy,pcy,One,A,twopt2,0)
      call get2c(Twocz,pcz,zconst,A,twopt2,0)
      
      call get3c(Xip,Twocx,Ccx)
      call get3c(Yip,Twocy,Ccy)
      call get3c(Zip,Twocz,Ccz)
      
      
      intc=100
      do 228 i=Istart,Iend
      
      ix=indix(i)
      iy=indiy(i)
      iz=indiz(i)
      
      do 226 j=Jstart,Jend
      jx=indjx(j)
      jy=indjy(j)
      jz=indjz(j)
      
      intc=intc+1
      Eep(intc)=Eep(intc)+Xip(ix+jx)*Yip(iy+jy)*Zip(iz+jz)
      xip1=Xip(ix+jx)
      yip1=Yip(iy+jy)
      zip1=Zip(iz+jz)
      if(idump.EQ.11)write(6,99008)j,jx,jy,jz,intc,xip1,yip1,zip1,Eep(in
     &tc)
      
99008 format(' J,JX,JY,JZ,INTC,XIP1,YIP1,ZIP1,EEP(INTC)=',/,5I5,4G15.5)
      
      
226   continue
228   continue
      
230   continue
      
235   continue
      
      
      sterm=rootpi*gsqrt(epi)
      call get1cs(S1c,sterm,epio2,2)
      
      call get2cs(Sx,S1c,Ccx,2)
      call get2cs(Sy,S1c,Ccy,2)
      do 240 i=1,lim1ds
      S1c(i)=S1c(i)*pexp
240   continue
      call get2cs(Sz,S1c,Ccz,2)
      
      
      intc=0
      do 245 i=Istart,Iend
      ix=indix(i)
      iy=indiy(i)
      iz=indiz(i)
      ixp=indjx(i)
      iyp=indjy(i)
      izp=indjz(i)
      xk=gfloat(2*(ixp+iyp+izp)-3)*as
      xiim1=gfloat((ixp-1)*(ixp-2))
      yiim1=gfloat((iyp-1)*(iyp-2))
      ziim1=gfloat((izp-1)*(izp-2))
      do 242 j=Jstart,Jend
      jx=indjx(j)
      jy=indjy(j)
      jz=indjz(j)
      ijx=ix+jx
      ijy=iy+jy
      ijz=iz+jz
      intc=intc+1
      
      coef=Ca(i)*Cb(j)
      syz=Sy(ijy)*Sz(ijz)
      temps=Sx(ijx)*syz
      
      tempk=temps*xk
      tempk=tempk-twoasq*(Sx(ijx+8)*syz+Sx(ijx)*(Sy(ijy+8)*Sz(ijz)+Sy(ij
     &y)*Sz(ijz+8)))
      tk=Zero
      if(ixp.GT.2)tk=xiim1*Sx(ijx-8)*syz
      if(iyp.GT.2)tk=tk+yiim1*Sx(ijx)*Sy(ijy-8)*Sz(ijz)
      if(izp.GT.2)tk=tk+ziim1*Sx(ijx)*Sy(ijy)*Sz(ijz-8)
      tempk=tempk-Half*tk
      Ss(intc)=Ss(intc)+temps*coef
      Eek(intc)=Eek(intc)+tempk*coef
      Eep(intc)=Eep(intc)+Eep(intc+100)*coef
242   continue
245   continue
      
250   continue
260   continue
      if(idump.EQ.11)write(6,99009)intc
      
99009 format(' INTC =',4I6)
      
      if(idump.EQ.11)write(6,99010)(Eep(i),i=1,intc)
      
99010 format(' ',10G10.5)
      
      
      call filmat(Eek,T)
      call filmat(Ss,S)
      call filmat(Eep,V)
      
300   continue
400   continue
      
      if(iprint.GT.0)then
      write(Iout,99004)
      call linout(S,Nbasis,0)
      write(Iout,99001)
      call linout(T,Nbasis,0)
      write(Iout,99002)
      call linout(V,Nbasis,0)
      endif
      call twrite(irws,S,ntt,1,ntt,1,0)
      do 500 i=1,ntt
      V(i)=T(i)-V(i)
500   continue
      call twrite(irwh,V,ntt,1,ntt,1,0)
      call twrite(irwt,T,ntt,1,ntt,1,0)
      if(iprint.GT.0)then
      write(Iout,99003)
      call linout(V,Nbasis,0)
      endif
      idump=0
      
      JUMP=0
      return
      
      
      end
C* :1 * 
      
