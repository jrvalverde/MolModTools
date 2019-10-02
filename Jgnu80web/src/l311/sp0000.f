
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sp0000"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sp0000.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "sp0000.web"
      subroutine sp0000
      implicit none
      double precision A0,A1,A2,A3,A4,Acx,Acy,Acy2,Acz,Ag,Ap,App,Aqx,Aqz
     &,Auxvar,Ax,Ay,Az,B0,B1
      double precision B2,B3,B4,Bg,Bp,Bpp,Bx,By,Bz,C0,C1,C11,C12,C13,C2,
     &C21,C22,C23,C3,C31
      double precision C32,C33,C4,Cg,Cmax,Cmaxa,Cmaxb,Cmaxc,Cmaxd,Cosg,C
     &pa,Cpb,Cpc,Cpd,Cq,Csa,Csb,Csc,csck,Csd
      double precision Cx,Cy,Cz,Dg,Dp00,Dp00p,Dp01,Dp01p,Dp10,Dp10p,Dp11
     &,Dp11p,Dq,Dq00,Dq01,Dq10,Dq11,Dx,Dy,Dz
      double precision Eab,Ecd,Ep,Error1,Error2,f0,G,G0000,Ga,Gab,Gb,Gc,
     &Gcd,gcrcds,Gd,gdecd,Gout,Gp,H,H0000
      double precision H0001,H0002,H0003,one,P11,P12,P13,P21,P22,P23,P31
     &,P32,P33,Pa,Pb,Pc,Pd,Pidiv4,Pito52,Pq1
      double precision Pq2,Pq3,Px,Py,Pz,Q11,Q12,Q13,Q21,Q22,Q23,Q31,Q32,
     &Q33,Qperp,Qperp2,Qq,Qx,Qy,Qz
      double precision Rab,Rabsq,Rcd,Rcdsq,Rpq,Rpqsq,Sa,Sb,Sc,Sd,Sing,si
     &xty,Theta,theta2,theta3,theta4,twenty,two,Var1,Var2
      double precision x,xqq,xx,xxtest,zero
      integer i,Isml,Ismlp,Ismlq,k,l,La,Lb,Lc,Ld,Mab,Mcd,N,Nga,Ngangb,Ng
     &b,Ngc,Ngd
      common/astore/Qq,Theta,N
      common/ctable/A0(400),B0(400),C0(400),A1(400),B1(400),C1(400),A2(4
     &00),B2(400),C2(400),A3(400),B3(400),C3(400),A4(400),B4(400),C4(400
     &)
      common/auxvar/Auxvar,Var1,Var2
      common/misc/Mab,Mcd,Ngangb
      common/ginf/Ga,Gb,Gc,Gd,Sa,Sb,Sc,Sd,Pa,Pb,Pc,Pd,Gab,Gcd
      common/pqgeom/Ap,Bp,Cq,Dq,Px,Py,Pz,Qx,Qy,Qz,Rpq,Rpqsq,Pq1,Pq2,Pq3,
     &C11,C12,C13,C21,C22,C23,C31,C32,C33
      common/eabecd/Eab,Ecd
      common/h/H0000,H0001,H0002,H0003,H(156)
      common/pgeom/Gp(100),Ep(100),Dp00p(100),Dp01p(100),Dp10p(100),Dp11
     &p(100),App(100),Bpp(100)
      common/shlinf/Nga,La,Ag(10),Csa(10),Cpa(10),Ngb,Lb,Bg(10),Csb(10),
     &Cpb(10),Ngc,Lc,Cg(10),Csc(10),Cpc(10),Ngd,Ld,Dg(10),Csd(10),Cpd(10
     &)
      common/cgeom/Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,Dx,Dy,Dz,Rab,Rabsq,Rcd,Rcd
     &sq,P11,P12,P13,P21,P22,P23,P31,P32,P33,Q11,Q12,Q13,Q21,Q22,Q23,Q31
     &,Q32,Q33
      common/dpq/Dp00,Dp01,Dp10,Dp11,Dq00,Dq01,Dq10,Dq11
      common/g/G0000,G(159)
      common/gout/Gout(256)
      common/qgeom/Acx,Acy,Acz,Acy2,Cosg,Sing,Aqx,Aqz,Qperp,Qperp2
      common/maxc/Cmax(240),Cmaxa(10),Cmaxb(10),Cmaxc(10),Cmaxd(10),Isml
     &p(100),Ismlq,Isml,Error1,Error2
      common/picon/Pito52,Pidiv4
      data zero/0.0D0/,one/1.0D0/,two/2.0D0/,twenty/20.0D0/
      data sixty/60.0D0/
      
      
      
      
      
      
      G0000=zero
      do 100 k=1,Ngc
      Gc=Cg(k)
      csck=Csc(k)
      gcrcds=Gc*Rcdsq
      do 50 l=1,Ngd
      Gd=Dg(l)
      Gcd=Gc+Gd
      Ecd=one/Gcd
      gdecd=Gd*Ecd
      xqq=gdecd*gcrcds
      if(xqq.LE.sixty)then
      
      xx=dexp(-xqq)*Ecd
      else
      xx=zero
      endif
      xxtest=xx*Cmaxc(k)*Cmaxd(l)
      if(xxtest.LE.Error1)then
      
      if(xxtest.LE.Error2)goto 50
      Ismlq=1
      else
      Ismlq=0
      endif
      Cq=gdecd*Rcd
      Aqx=Acx+Sing*Cq
      Aqz=Acz+Cosg*Cq
      Qperp2=Aqx*Aqx+Acy2
      H0000=zero
      do 20 i=1,Ngangb
      Isml=Ismlq+Ismlp(i)
      if(Isml.LT.2)then
      if(Isml.LT.1)then
      Auxvar=Var1
      else
      
      Auxvar=Var2
      endif
      x=((Aqz-App(i))**2+Qperp2)/(Ep(i)+Ecd)
      if(x.LE.Auxvar)then
      
      Qq=x*twenty
      Theta=Qq-dint(Qq)
      N=Qq-Theta
      theta2=Theta*(Theta-one)
      theta3=theta2*(Theta-two)
      theta4=theta2*(Theta+one)
      f0=A0(N+1)+Theta*B0(N+1)-theta3*C0(N+1)+theta4*C0(N+2)
      H0000=H0000+Dp00p(i)*f0/dsqrt(Gp(i)+Gcd)
      else
      H0000=H0000+Dp00p(i)*dsqrt(Pidiv4/(x*(Gp(i)+Gcd)))
      endif
      endif
20    continue
      G0000=G0000+H0000*csck*Csd(l)*xx
50    continue
100   continue
      Gout(1)=G0000
      return
      
      end
C* :1 * 
      
