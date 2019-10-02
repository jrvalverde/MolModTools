
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sp0111"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sp0111.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "sp0111.web"
      subroutine sp0111
      implicit none
      double precision A0,A1,A2,A3,A4,Acx,Acy,Acy2,Acz,Ag,Ap,App,Aqx,Aqz
     &,Auxvar,Ax,Ay,Az,B0,B1
      double precision B2,B3,B4,Bg,Bp,Bpp,Bx,By,Bz,C,C0,C1,C11,C12,C13,C
     &2,C21,C22,C23,C3
      double precision C31,C32,C33,C4,Cg,Cmax,Cmaxa,Cmaxb,Cmaxc,Cmaxd,Co
     &np,Const,Cosg,Cpa,Cpb,Cpc,Cpd,Cq,Csa,Csb
      double precision Csc,Csd,Cx,Cy,Cz,Dg,dp00,Dp00p,Dp01p,Dp10p,Dp11p,
     &Dq,Dx,Dy,Dz,E1,Eab,Ecd,ecd2,Ep
      double precision Error1,Error2,f0,f1,f1pqab,f2,f2pqa2,f2pqab,f3,f3
     &pqab,g,Ga,Gab,Gb,Gc,Gcd,Gd,gggy,ggy,Gp
      double precision gtx,gy,H,H0000,H0001,H0002,H0003,H0011,H0012,H001
     &3,H0022,H0023,H0033,H0100,H0101,H0102,H0103,H0111,H0112,H0113
      double precision H0122,H0123,H0133,H0200,H0201,H0202,H0203,H0211,H
     &0212,H0213,H0222,H0223,H0233,H0300,H0301,H0302,H0303,H0311,H0312,H
     &0313
      double precision H0322,H0323,H0333,hecd,one,onept5,P11,P12,P13,P21
     &,P22,P23,P31,P32,P33,Pa,Pb,Pc,Pd,Pidiv4
      double precision Pito52,Pq1,Pq2,Pq3,pqab,pqab2,pt5,Px,Py,Pz,Q11,Q1
     &2,Q13,Q21,Q22,Q23,q2ecd,q2ecd2,Q31,Q32
      double precision Q33,qecd,qecd2,Qperp,Qperp2,Qq,Qx,Qy,Qz,Rab,Rabsq
     &,Rcd,Rcdsq,Rpq,Rpqsq,Sa,Sb,Sc,Sd,Sing
      double precision Theta,theta2,theta3,theta4,twenty,two,twopt5,Var1
     &,Var2,x,x1,x2,x3,x4,x5,x6,y,y1,y2,y3
      double precision y4,y5,y6,z1,z2,z3,z4,z5,z6,z7,z8,z9,zero
      integer i,Isml,Ismlp,Ismlq,La,Lb,Lc,Ld,Mab,Mcd,N,Nga,Ngangb,Ngb,Ng
     &c,Ngd
      common/shlinf/Nga,La,Ag(10),Csa(10),Cpa(10),Ngb,Lb,Bg(10),Csb(10),
     &Cpb(10),Ngc,Lc,Cg(10),Csc(10),Cpc(10),Ngd,Ld,Dg(10),Csd(10),Cpd(10
     &)
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
      common/cgeom/Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,Dx,Dy,Dz,Rab,Rabsq,Rcd,Rcd
     &sq,P11,P12,P13,P21,P22,P23,P31,P32,P33,Q11,Q12,Q13,Q21,Q22,Q23,Q31
     &,Q32,Q33
      common/cconst/Const,Conp(100)
      common/astore/Qq,Theta,N
      common/ctable/A0(400),B0(400),C0(400),A1(400),B1(400),C1(400),A2(4
     &00),B2(400),C2(400),A3(400),B3(400),C3(400),A4(400),B4(400),C4(400
     &)
      common/auxvar/Auxvar,Var1,Var2
      common/picon/Pito52,Pidiv4
      equivalence(E1,Eab)
      data zero/0.0D0/,pt5/0.5D0/,one/1.0D0/,onept5/1.5D0/,two/2.0D0/,tw
     &opt5/2.5D0/,twenty/20.0D0/
      
      
      
      
      
      
      
      x1=zero
      x2=zero
      x3=zero
      x4=zero
      x5=zero
      x6=zero
      y1=zero
      y2=zero
      y3=zero
      y4=zero
      y5=zero
      y6=zero
      z1=zero
      z2=zero
      z3=zero
      z4=zero
      z5=zero
      z6=zero
      z7=zero
      z8=zero
      z9=zero
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
      g=one/(Eab+Ecd)
      x=g*(pqab2+Qperp2)
      if(x.LE.Auxvar)then
      
      y=Conp(i)/dsqrt(Gp(i)+Gcd)
      gy=g*y
      ggy=g*gy
      gggy=g*ggy
      Qq=x*twenty
      Theta=Qq-dint(Qq)
      N=Qq-Theta
      theta2=Theta*(Theta-one)
      theta3=theta2*(Theta-two)
      theta4=theta2*(Theta+one)
      f0=(A0(N+1)+Theta*B0(N+1)-theta3*C0(N+1)+theta4*C0(N+2))*y
      f1=(A1(N+1)+Theta*B1(N+1)-theta3*C1(N+1)+theta4*C1(N+2))*gy
      f2=(A2(N+1)+Theta*B2(N+1)-theta3*C2(N+1)+theta4*C2(N+2))*ggy
      f3=(A3(N+1)+Theta*B3(N+1)-theta3*C3(N+1)+theta4*C3(N+2))*gggy
      else
      f0=Conp(i)*dsqrt(Pidiv4/(x*(Gp(i)+Gcd)))
      gtx=g/x
      f1=pt5*f0*gtx
      f2=onept5*f1*gtx
      f3=twopt5*f2*gtx
      endif
      f1pqab=f1*pqab
      f2pqab=f2*pqab
      f3pqab=f3*pqab
      f2pqa2=f2*pqab2
      x1=x1+f0*dp00
      x2=x2+f1*dp00
      x3=x3+f2*dp00
      x4=x4+f1pqab*dp00
      x5=x5+f2pqab*dp00
      x6=x6+f2pqa2*dp00
      y1=y1+f0*Bp
      y2=y2+f1*Bp
      y3=y3+f2*Bp
      y4=y4+f1pqab*Bp
      y5=y5+f2pqab*Bp
      y6=y6+f2pqa2*Bp
      z1=z1+f1
      z2=z2+f2
      z3=z3+f3
      z4=z4+f1pqab
      z5=z5+f2pqab
      z6=z6+f3pqab
      z7=z7+f2pqa2
      z8=z8+f3*pqab2
      z9=z9+f3pqab*pqab2
      endif
100   continue
      hecd=pt5*Ecd
      ecd2=Ecd*Ecd
      qecd=Qperp*Ecd
      qecd2=Qperp*ecd2
      q2ecd=Qperp2*Ecd
      q2ecd2=Qperp2*ecd2
      H0000=x1
      H0001=qecd*x2
      H0003=-Ecd*x4
      H0022=hecd*(x1-Ecd*x2)
      H0011=H0022+q2ecd2*x3
      H0013=-qecd2*x5
      H0033=H0022+ecd2*x6
      H0100=-Qperp*z1
      H0300=z4+y1
      H0202=hecd*z1
      H0101=H0202-q2ecd*z2
      H0103=qecd*z5
      H0301=H0103+qecd*y2
      H0303=H0202-Ecd*z7-Ecd*y4
      H0212=pt5*qecd2*z2
      H0223=-pt5*ecd2*z5
      H0122=H0212-Qperp*H0202
      H0322=H0223+hecd*(H0300-Ecd*y2)
      H0113=H0223+q2ecd2*z6
      H0313=H0212-qecd2*(z8+y5)
      H0111=H0122+H0212+H0212-q2ecd2*Qperp*z3
      H0133=H0122-qecd2*z8
      H0311=H0322+q2ecd2*(z6+y3)
      H0333=H0322+H0223+H0223+ecd2*(z9+y6)
      return
      
      end
C* :1 * 
      
