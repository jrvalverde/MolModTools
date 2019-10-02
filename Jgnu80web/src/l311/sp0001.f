
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sp0001"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sp0001.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "sp0001.web"
      subroutine sp0001
      implicit none
      double precision A0,A1,A2,A3,A4,Acx,Acy,Acy2,Acz,Ap,App,Aqx,Aqz,Au
     &xvar,B0,B1,B2,B3,B4,Bp
      double precision Bpp,C0,C1,C11,C12,C13,C2,C21,C22,C23,C3,C31,C32,C
     &33,C4,Cmax,Cmaxa,Cmaxb,Cmaxc,Cmaxd
      double precision Cosg,Cosp,Cq,Dp00,Dp00p,Dp01,Dp01p,Dp10,Dp10p,Dp1
     &1,Dp11p,Dq,Dq00,Dq01,Dq10,Dq11,Eab,Ecd,Ep,Error1
      double precision Error2,g,G0000,G0001,G0002,G0003,Ga,Gab,Gb,Gc,Gcd
     &,Gd,Gg,Gout1,Gout2,Gout3,Gout4,Gp,Gx,H
      double precision H0000,H0001,H0002,H0003,one,Pa,Pb,Pc,Pd,Pidiv4,Pi
     &to52,Pq1,Pq2,Pq3,pqab,pt5,Px,Py,Pz,Qperp
      double precision Qperp2,Qq,Qx,Qy,Qz,Rpq,Rpqsq,Sa,Sb,Sc,Sd,Sing,Sin
     &p,Theta,theta2,theta3,theta4,twenty,two,u
      double precision Var1,Var2,x,y,y0,y1,zero
      integer i,Isml,Ismlp,Ismlq,Mab,Mcd,N,Ngangb
      common/pqgeom/Ap,Bp,Cq,Dq,Px,Py,Pz,Qx,Qy,Qz,Rpq,Rpqsq,Pq1,Pq2,Pq3,
     &C11,C12,C13,C21,C22,C23,C31,C32,C33
      common/astore/Qq,Theta,N
      common/ctable/A0(400),B0(400),C0(400),A1(400),B1(400),C1(400),A2(4
     &00),B2(400),C2(400),A3(400),B3(400),C3(400),A4(400),B4(400),C4(400
     &)
      common/auxvar/Auxvar,Var1,Var2
      common/misc/Mab,Mcd,Ngangb
      common/eabecd/Eab,Ecd
      common/h/H0000,H0001,H0002,H0003,H(156)
      common/ginf/Ga,Gb,Gc,Gd,Sa,Sb,Sc,Sd,Pa,Pb,Pc,Pd,Gab,Gcd
      common/pgeom/Gp(100),Ep(100),Dp00p(100),Dp01p(100),Dp10p(100),Dp11
     &p(100),App(100),Bpp(100)
      common/qgeom/Acx,Acy,Acz,Acy2,Cosg,Sing,Aqx,Aqz,Qperp,Qperp2
      common/g/G0000,G0001,G0002,G0003,Gg(156)
      common/phi/Cosp,Sinp
      common/maxc/Cmax(240),Cmaxa(10),Cmaxb(10),Cmaxc(10),Cmaxd(10),Isml
     &p(100),Ismlq,Isml,Error1,Error2
      common/gout/Gout1,Gout2,Gout3,Gout4,Gx(252)
      common/dpq/Dp00,Dp01,Dp10,Dp11,Dq00,Dq01,Dq10,Dq11
      common/picon/Pito52,Pidiv4
      data zero/0.0D0/,pt5/0.5D0/,one/1.0D0/,two/2.0D0/,twenty/20.0D0/
      
      
      
      
      
      
      H0000=zero
      H0001=zero
      H0003=zero
      do 100 i=1,Ngangb
      Isml=Ismlq+Ismlp(i)
      if(Isml.LT.2)then
      if(Isml.LT.1)then
      Auxvar=Var1
      else
      
      Auxvar=Var2
      endif
      pqab=Aqz-App(i)
      g=one/(Ep(i)+Ecd)
      x=(pqab*pqab+Qperp2)*g
      if(x.LE.Auxvar)then
      
      y=Dp00p(i)/dsqrt(Gp(i)+Gcd)
      Qq=x*twenty
      Theta=Qq-dint(Qq)
      N=Qq-Theta
      theta2=Theta*(Theta-one)
      theta3=theta2*(Theta-two)
      theta4=theta2*(Theta+one)
      y0=(A0(N+1)+Theta*B0(N+1)-theta3*C0(N+1)+theta4*C0(N+2))*y
      y1=(A1(N+1)+Theta*B1(N+1)-theta3*C1(N+1)+theta4*C1(N+2))*y
      else
      y0=Dp00p(i)*dsqrt(Pidiv4/(x*(Gp(i)+Gcd)))
      y1=pt5*y0/x
      endif
      u=g*y1
      H0000=H0000+y0
      H0001=H0001+u
      H0003=H0003-u*pqab
      endif
100   continue
      H0001=H0001*Ecd*Qperp
      H0003=H0003*Ecd
      x=Dq*H0000
      G0001=H0001*Cosp+x*Sing
      G0002=H0001*Sinp
      G0003=H0003+x*Cosg
      Gout1=Gout1+Dq00*H0000
      Gout2=Gout2+Dq01*G0001
      Gout3=Gout3+Dq01*G0002
      Gout4=Gout4+Dq01*G0003
      return
      
      end
C* :1 * 
      
