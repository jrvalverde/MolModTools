
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sp0011"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sp0011.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "sp0011.web"
      subroutine sp0011
      implicit none
      double precision A0,A1,A2,A3,A4,Acx,Acy,Acy2,Acz,Ap,App,Aqx,Aqz,Au
     &xvar,B0,B1,B2,B3,B4,Bp
      double precision Bpp,C,C0,C1,C11,C12,C13,C2,C21,C22,C23,C3,C31,C32
     &,C33,C4,Cmax,Cmaxa,Cmaxb,Cmaxc
      double precision Cmaxd,Cosg,Cq,Dp00p,Dp01p,Dp10p,Dp11p,Dq,Eab,Ecd,
     &Ep,Error1,Error2,f0,f1,f2,g,Ga,Gab,Gb
      double precision Gc,Gcd,Gd,ggy,Gp,gtx,gy,H,H0000,H0001,H0002,H0003
     &,H0011,H0012,H0013,H0022,H0023,H0033,one,onept5
      double precision Pa,Pb,Pc,Pd,Pidiv4,Pito52,Pq1,Pq2,Pq3,pqab,pqab2,
     &pt5,Px,Py,Pz,Qperp,Qperp2,Qq,Qx,Qy
      double precision Qz,Rpq,Rpqsq,Sa,Sb,Sc,Sd,Sing,Theta,theta2,theta3
     &,theta4,twenty,two,Var1,Var2,x,y,zero
      integer i,Isml,Ismlp,Ismlq,Mab,Mcd,N,Ngangb
      common/misc/Mab,Mcd,Ngangb
      common/pqgeom/Ap,Bp,Cq,Dq,Px,Py,Pz,Qx,Qy,Qz,Rpq,Rpqsq,Pq1,Pq2,Pq3,
     &C11,C12,C13,C21,C22,C23,C31,C32,C33
      common/eabecd/Eab,Ecd
      common/h/H0000,H0001,H0002,H0003,H0011,H0012,H0013,H0022,H0023,H00
     &33,H(150)
      common/ginf/Ga,Gb,Gc,Gd,Sa,Sb,Sc,Sd,Pa,Pb,Pc,Pd,Gab,Gcd
      common/pgeom/Gp(100),Ep(100),Dp00p(100),Dp01p(100),Dp10p(100),Dp11
     &p(100),App(100),Bpp(100)
      common/cos/C
      common/qgeom/Acx,Acy,Acz,Acy2,Cosg,Sing,Aqx,Aqz,Qperp,Qperp2
      common/astore/Qq,Theta,N
      common/ctable/A0(400),B0(400),C0(400),A1(400),B1(400),C1(400),A2(4
     &00),B2(400),C2(400),A3(400),B3(400),C3(400),A4(400),B4(400),C4(400
     &)
      common/auxvar/Auxvar,Var1,Var2
      common/maxc/Cmax(240),Cmaxa(10),Cmaxb(10),Cmaxc(10),Cmaxd(10),Isml
     &p(100),Ismlq,Isml,Error1,Error2
      common/picon/Pito52,Pidiv4
      data zero/0.0D0/,pt5/0.5D0/,one/1.0D0/,onept5/1.5D0/,two/2.0D0/,tw
     &enty/20.0D0/
      
      
      
      
      
      
      H0000=zero
      H0001=zero
      H0003=zero
      H0011=zero
      H0013=zero
      H0033=zero
      do 100 i=1,Ngangb
      Isml=Ismlq+Ismlp(i)
      if(Isml.LT.2)then
      if(Isml.LT.1)then
      Auxvar=Var1
      else
      
      Auxvar=Var2
      endif
      pqab=Aqz-App(i)
      pqab2=pqab*pqab
      g=one/(Ep(i)+Ecd)
      x=g*(pqab2+Qperp2)
      g=g*Ecd
      if(x.LE.Auxvar)then
      
      y=Dp00p(i)/dsqrt(Gp(i)+Gcd)
      gy=g*y
      ggy=g*gy
      Qq=x*twenty
      Theta=Qq-dint(Qq)
      N=Qq-Theta
      theta2=Theta*(Theta-one)
      theta3=theta2*(Theta-two)
      theta4=theta2*(Theta+one)
      f0=(A0(N+1)+Theta*B0(N+1)-theta3*C0(N+1)+theta4*C0(N+2))*y
      f1=(A1(N+1)+Theta*B1(N+1)-theta3*C1(N+1)+theta4*C1(N+2))*gy
      f2=(A2(N+1)+Theta*B2(N+1)-theta3*C2(N+1)+theta4*C2(N+2))*ggy
      else
      f0=Dp00p(i)*dsqrt(Pidiv4/(x*(Gp(i)+Gcd)))
      gtx=g/x
      f1=pt5*f0*gtx
      f2=onept5*f1*gtx
      endif
      H0000=H0000+f0
      H0001=H0001+f1
      H0003=H0003-f1*pqab
      H0011=H0011+f2
      H0013=H0013-f2*pqab
      H0033=H0033+f2*pqab2
      endif
100   continue
      H0022=pt5*Ecd*(H0000-H0001)
      H0001=H0001*Qperp
      H0011=H0011*Qperp2+H0022
      H0013=H0013*Qperp
      H0033=H0033+H0022
      return
      
      end
C* :1 * 
      
