
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sp0101"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sp0101.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "sp0101.web"
      subroutine sp0101
      implicit none
      double precision A0,A1,A2,A3,A4,Acx,Acy,Acy2,Acz,Ag,Ap,App,Aqx,Aqz
     &,Auxvar,B0,B1,B2,B3,B4
      double precision Bg,Bp,Bpp,C,C0,C1,C11,C12,C13,C2,C21,C22,C23,C3,C
     &31,C32,C33,C4,Cg,Cmax
      double precision Cmaxa,Cmaxb,Cmaxc,Cmaxd,Conp,Const,Cosg,Cpa,Cpb,C
     &pc,Cpd,Cq,Csa,Csb,Csc,Csd,Dg,dp00,Dp00p,Dp01p
      double precision Dp10p,Dp11p,Dq,Eab,Ecd,Ep,Error1,Error2,f0,f1,f2,
     &g,g03,Ga,Gab,Gb,Gc,Gcd,Gd,ggy
      double precision Gp,gtx,gy,H,H0000,H0001,H0002,H0003,H0011,H0012,H
     &0013,H0022,H0023,H0033,H0100,H0101,H0102,H0103,H0111,H0112
      double precision H0113,H0122,H0123,H0133,H0200,H0201,H0202,H0203,H
     &0211,H0212,H0213,H0222,H0223,H0233,H0300,H0301,H0302,H0303,H0311,H
     &0312
      double precision H0313,H0322,H0323,H0333,one,onept5,Pa,Pb,Pc,Pd,Pi
     &div4,Pito52,Pq1,Pq2,Pq3,pqab,pqab2,pt5,Px,Py
      double precision Pz,Qperp,Qperp2,Qq,Qx,Qy,Qz,Rpq,Rpqsq,Sa,Sb,Sc,Sd
     &,Sing,Theta,theta2,theta3,theta4,twenty,two
      double precision Var1,Var2,x,y,zero
      integer i,Isml,Ismlp,Ismlq,La,Lb,Lc,Ld,Mab,Mcd,N,Nga,Ngangb,Ngb,Ng
     &c,Ngd
      common/astore/Qq,Theta,N
      common/shlinf/Nga,La,Ag(10),Csa(10),Cpa(10),Ngb,Lb,Bg(10),Csb(10),
     &Cpb(10),Ngc,Lc,Cg(10),Csc(10),Cpc(10),Ngd,Ld,Dg(10),Csd(10),Cpd(10
     &)
      common/ctable/A0(400),B0(400),C0(400),A1(400),B1(400),C1(400),A2(4
     &00),B2(400),C2(400),A3(400),B3(400),C3(400),A4(400),B4(400),C4(400
     &)
      common/auxvar/Auxvar,Var1,Var2
      common/misc/Mab,Mcd,Ngangb
      common/pqgeom/Ap,Bp,Cq,Dq,Px,Py,Pz,Qx,Qy,Qz,Rpq,Rpqsq,Pq1,Pq2,Pq3,
     &C11,C12,C13,C21,C22,C23,C31,C32,C33
      common/eabecd/Eab,Ecd
      common/h/H0000,H0001,H0002,H0003,H0011,H0012,H0013,H0022,H0023,H00
     &33,H0100,H0101,H0102,H0103,H0111,H0112,H0113,H0122,H0123,H0133,H02
     &00,H0201,H0202,H0203,H0211,H0212,H0213,H0222,H0223,H0233,H0300,H03
     &01,H0302,H0303,H0311,H0312,H0313,H0322,H0323,H0333,H(120)
      common/ginf/Ga,Gb,Gc,Gd,Sa,Sb,Sc,Sd,Pa,Pb,Pc,Pd,Gab,Gcd
      common/pgeom/Gp(100),Ep(100),Dp00p(100),Dp01p(100),Dp10p(100),Dp11
     &p(100),App(100),Bpp(100)
      common/cos/C
      common/qgeom/Acx,Acy,Acz,Acy2,Cosg,Sing,Aqx,Aqz,Qperp,Qperp2
      common/maxc/Cmax(240),Cmaxa(10),Cmaxb(10),Cmaxc(10),Cmaxd(10),Isml
     &p(100),Ismlq,Isml,Error1,Error2
      common/cconst/Const,Conp(100)
      common/picon/Pito52,Pidiv4
      data zero/0.0D0/,pt5/0.5D0/,one/1.0D0/,onept5/1.5D0/,two/2.0D0/,tw
     &enty/20.0D0/
      
      
      
      
      
      
      H0000=zero
      H0001=zero
      H0003=zero
      H0100=zero
      H0101=zero
      H0103=zero
      H0300=zero
      H0301=zero
      H0303=zero
      do 100 i=1,Ngangb
      Isml=Ismlq+Ismlp(i)
      if(Isml.LT.2)then
      if(Isml.LT.1)then
      Auxvar=Var1
      else
      
      Auxvar=Var2
      endif
      Eab=Ep(i)
      dp00=Dp00p(i)
      Bp=Bpp(i)
      pqab=Aqz-App(i)
      pqab2=pqab*pqab
      g=one/(Ep(i)+Ecd)
      x=(pqab2+Qperp2)*g
      if(x.LE.Auxvar)then
      
      y=Conp(i)/dsqrt(Gp(i)+Gcd)
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
      f0=Conp(i)*dsqrt(Pidiv4/(x*(Gp(i)+Gcd)))
      gtx=g/x
      f1=pt5*f0*gtx
      f2=onept5*f1*gtx
      endif
      g03=-pqab*f1
      H0000=H0000+f0*dp00
      H0001=H0001+f1*dp00
      H0003=H0003+g03*dp00
      H0100=H0100-f1
      H0101=H0101-f2
      H0103=H0103+pqab*f2
      H0300=H0300-g03+Bp*f0
      H0301=H0301+Bp*f1
      H0303=H0303-pqab2*f2+Bp*g03
      endif
100   continue
      x=Qperp*Ecd
      H0001=H0001*x
      H0003=H0003*Ecd
      H0202=-pt5*Ecd*H0100
      H0100=H0100*Qperp
      H0101=H0101*Qperp2*Ecd
      H0103=H0103*x
      H0301=H0301*x
      H0303=H0303*Ecd
      H0301=H0301+H0103
      H0101=H0101+H0202
      H0303=H0303+H0202
      return
      
      end
C* :1 * 
      
