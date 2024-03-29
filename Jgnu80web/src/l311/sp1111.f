#if(0)
  FTANGLE v1.61,
 created with UNIX on "Friday, September 25, 1998 at 8:02." 
  COMMAND LINE: "ftangle -ybs15000 sp1111"
  RUN TIME:     "Friday, June 5, 2009 at 15:05."
  WEB FILE:     "sp1111.web"
  CHANGE FILE:  (none) 
#endif

      subroutine sp1111
      implicit none
      double precision a1 , a10 , a2 , a3 , a4 , a5 , a6 , a8 , a9 ,
     &                 Aa , Ab , Ac , Acx , Acy , Acy2 , Acz , Ad , Ae ,
     &                 Ag , Ap
      double precision apbp , apdp10 , App , Aqx , Aqz , Auxvar , Ax ,
     &                 Ay , Az , b1 , b10 , b2 , b3 , b4 , b5 , b6 ,
     &                 b8 , b9 , Ba , Bb
      double precision Bc , Bd , Be , Bg , Bp , bpdp01 , Bpp , bqz ,
     &                 Bx , By , Bz , c1 , C11 , C12 , C13 , c2 , C21 ,
     &                 C22 , C23 , c3
      double precision C31 , C32 , C33 , c4 , c5 , c6 , Ca , Cb , Cc ,
     &                 Cd , Ce , Cg , Cmax , Cmaxa , Cmaxb , Cmaxc ,
     &                 Cmaxd , Conp , Const , Cosg
      double precision Cpa , Cpb , Cpc , Cpd , Cq , Csa , Csb , Csc ,
     &                 Csd , Cx , Cy , Cz , Dg , Dp00 , Dp00p , Dp01 ,
     &                 Dp01p , Dp10 , Dp10p , Dp11
      double precision Dp11p , Dq , Dq00 , Dq01 , Dq10 , Dq11 , Dx ,
     &                 Dy , Dz , Eab , eab2 , Ecd , ecd2 , edp01 ,
     &                 edp10 , Ep , Error1 , Error2 , f0 , f1
      double precision f1pqa2 , f1pqab , f2 , f2pqa2 , f2pqa3 , f2pqab ,
     &                 f3 , f3pqa2 , f3pqa3 , f3pqab , f4 , f4pqa2 ,
     &                 f4pqa3 , f4pqab , g , Ga , Gab , Gb , Gc , Gcd
      double precision Gd , gggy , ggy , Gp , gtx , gy , H0000 , H0001 ,
     &                 H0002 , H0003 , H0011 , H0012 , H0013 , H0022 ,
     &                 H0023 , H0033 , H0100 , H0101 , H0102 , H0103
      double precision H0111 , H0112 , H0113 , H0122 , H0123 , H0133 ,
     &                 H0200 , H0201 , H0202 , H0203 , H0211 , H0212 ,
     &                 H0213 , H0222 , H0223 , H0233 , H0300 , H0301 ,
     &                 H0302 , H0303
      double precision H0311 , H0312 , H0313 , H0322 , H0323 , H0333 ,
     &                 H1000 , H1001 , H1002 , H1003 , H1011 , H1012 ,
     &                 H1013 , H1022 , H1023 , H1033 , H1100 , H1101 ,
     &                 H1102 , H1103
      double precision H1111 , H1112 , H1113 , H1122 , H1123 , H1133 ,
     &                 H1200 , H1201 , H1202 , H1203 , H1211 , H1212 ,
     &                 H1213 , H1222 , H1223 , H1233 , H1300 , H1301 ,
     &                 H1302 , H1303
      double precision H1311 , H1312 , H1313 , H1322 , H1323 , H1333 ,
     &                 H2000 , H2001 , H2002 , H2003 , H2011 , H2012 ,
     &                 H2013 , H2022 , H2023 , H2033 , H2100 , H2101 ,
     &                 H2102 , H2103
      double precision H2111 , H2112 , H2113 , H2122 , H2123 , H2133 ,
     &                 H2200 , H2201 , H2202 , H2203 , H2211 , H2212 ,
     &                 H2213 , H2222 , H2223 , H2233 , H2300 , H2301 ,
     &                 H2302 , H2303
      double precision H2311 , H2312 , H2313 , H2322 , H2323 , H2333 ,
     &                 H3000 , H3001 , H3002 , H3003 , H3011 , H3012 ,
     &                 H3013 , H3022 , H3023 , H3033 , H3100 , H3101 ,
     &                 H3102 , H3103
      double precision H3111 , H3112 , H3113 , H3122 , H3123 , H3133 ,
     &                 H3200 , H3201 , H3202 , H3203 , H3211 , H3212 ,
     &                 H3213 , H3222 , H3223 , H3233 , H3300 , H3301 ,
     &                 H3302 , H3303
      double precision H3311 , H3312 , H3313 , H3322 , H3323 , H3333 ,
     &                 hecd , hecd2 , hqecd , hqecd2 , hxxyy , one ,
     &                 onept5 , P11 , P12 , P13 , P21 , P22 , P23 , P31
      double precision P32 , P33 , Pa , Pb , Pc , Pd , Pidiv4 , Pito52 ,
     &                 Pq1 , Pq2 , Pq3 , pqab , pqab2 , pt25 , pt5 ,
     &                 Px , Py , Pz , Q11 , Q12
      double precision Q13 , Q21 , Q22 , Q23 , q2ecd , q2ecd2 , Q31 ,
     &                 Q32 , Q33 , q3ecd , q3ecd2 , qecd , qecd2 ,
     &                 Qperp , Qperp2 , Qq , Qx , Qy , Qz , Rab
      double precision Rabsq , Rcd , Rcdsq , Rpq , Rpqsq , s1 , s10 ,
     &                 s11 , s12 , s13 , s14 , s2 , s3 , s4 , s5 , s6 ,
     &                 s7 , s8 , s9 , Sa
      double precision Sb , Sc , Sd , Sing , t1 , t10 , t11 , t12 ,
     &                 t13 , t14 , t2 , t3 , t4 , t5 , t6 , t7 , t8 ,
     &                 t9 , temp , Theta
      double precision theta2 , theta3 , theta4 , three , thrpt5 ,
     &                 twenty , two , twopt5 , v1 , v2 , v3 , v4 , v5 ,
     &                 v6 , Var1 , Var2 , w1 , w2 , w3 , w4
      double precision w5 , w6 , w7 , w8 , w9 , x , x1 , x2 , x3 , x4 ,
     &                 x5 , x6 , y , y1 , y2 , y3 , y4 , y5 , y6 , z1
      double precision z2 , z3 , z4 , z5 , z6 , z7 , z8 , z9 , zero
      integer ind , Isml , Ismlp , Ismlq , La , Lb , Lc , Ld , Mab ,
     &        Mcd , N , Nga , Ngangb , Ngb , Ngc , Ngd
      common /misc  / Mab , Mcd , Ngangb
      common /shlinf/ Nga , La , Ag(10) , Csa(10) , Cpa(10) , Ngb , Lb ,
     &                Bg(10) , Csb(10) , Cpb(10) , Ngc , Lc , Cg(10) ,
     &                Csc(10) , Cpc(10) , Ngd , Ld , Dg(10) , Csd(10) ,
     &                Cpd(10)
      common /cgeom / Ax , Ay , Az , Bx , By , Bz , Cx , Cy , Cz , Dx ,
     &                Dy , Dz , Rab , Rabsq , Rcd , Rcdsq , P11 , P12 ,
     &                P13 , P21 , P22 , P23 , P31 , P32 , P33 , Q11 ,
     &                Q12 , Q13 , Q21 , Q22 , Q23 , Q31 , Q32 , Q33
      common /pgeom / Gp(100) , Ep(100) , Dp00p(100) , Dp01p(100) ,
     &                Dp10p(100) , Dp11p(100) , App(100) , Bpp(100)
      common /pqgeom/ Ap , Bp , Cq , Dq , Px , Py , Pz , Qx , Qy , Qz ,
     &                Rpq , Rpqsq , Pq1 , Pq2 , Pq3 , C11 , C12 , C13 ,
     &                C21 , C22 , C23 , C31 , C32 , C33
      common /ginf  / Ga , Gb , Gc , Gd , Sa , Sb , Sc , Sd , Pa , Pb ,
     &                Pc , Pd , Gab , Gcd
      common /eabecd/ Eab , Ecd
      common /dpq   / Dp00 , Dp01 , Dp10 , Dp11 , Dq00 , Dq01 , Dq10 ,
     &                Dq11
      common /qgeom / Acx , Acy , Acz , Acy2 , Cosg , Sing , Aqx , Aqz ,
     &                Qperp , Qperp2
      common /cconst/ Const , Conp(100)
      common /maxc  / Cmax(240) , Cmaxa(10) , Cmaxb(10) , Cmaxc(10) ,
     &                Cmaxd(10) , Ismlp(100) , Ismlq , Isml , Error1 ,
     &                Error2
      common /astore/ Qq , Theta , N
      common /ctable/ Aa(400) , Ba(400) , Ca(400) , Ab(400) , Bb(400) ,
     &                Cb(400) , Ac(400) , Bc(400) , Cc(400) , Ad(400) ,
     &                Bd(400) , Cd(400) , Ae(400) , Be(400) , Ce(400)
      common /auxvar/ Auxvar , Var1 , Var2
      common /h     / H0000 , H0001 , H0002 , H0003 , H0011 , H0012 ,
     &                H0013 , H0022 , H0023 , H0033 , H0100 , H0101 ,
     &                H0102 , H0103 , H0111 , H0112 , H0113 , H0122 ,
     &                H0123 , H0133 , H0200 , H0201 , H0202 , H0203 ,
     &                H0211 , H0212 , H0213 , H0222 , H0223 , H0233 ,
     &                H0300 , H0301 , H0302 , H0303 , H0311 , H0312 ,
     &                H0313 , H0322 , H0323 , H0333 , H1000 , H1001 ,
     &                H1002 , H1003 , H1011 , H1012 , H1013 , H1022 ,
     &                H1023 , H1033 , H1100 , H1101 , H1102 , H1103 ,
     &                H1111 , H1112 , H1113 , H1122 , H1123 , H1133 ,
     &                H1200 , H1201 , H1202 , H1203 , H1211 , H1212 ,
     &                H1213 , H1222 , H1223 , H1233 , H1300 , H1301 ,
     &                H1302 , H1303 , H1311 , H1312 , H1313 , H1322 ,
     &                H1323 , H1333 , H2000 , H2001 , H2002 , H2003 ,
     &                H2011 , H2012 , H2013 , H2022 , H2023 , H2033 ,
     &                H2100 , H2101 , H2102 , H2103 , H2111 , H2112 ,
     &                H2113 , H2122 , H2123 , H2133 , H2200 , H2201 ,
     &                H2202 , H2203 , H2211 , H2212 , H2213 , H2222 ,
     &                H2223 , H2233 , H2300 , H2301 , H2302 , H2303 ,
     &                H2311 , H2312 , H2313 , H2322 , H2323 , H2333 ,
     &                H3000 , H3001 , H3002 , H3003 , H3011 , H3012 ,
     &                H3013 , H3022 , H3023 , H3033 , H3100 , H3101 ,
     &                H3102 , H3103 , H3111 , H3112 , H3113 , H3122 ,
     &                H3123 , H3133 , H3200 , H3201 , H3202 , H3203 ,
     &                H3211 , H3212 , H3213 , H3222 , H3223 , H3233 ,
     &                H3300 , H3301 , H3302 , H3303 , H3311 , H3312 ,
     &                H3313 , H3322 , H3323 , H3333
      common /picon / Pito52 , Pidiv4
      data zero/0.0D0/ , pt25/0.25D0/ , pt5/0.5D0/ , one/1.0D0/ ,
     &     onept5/1.5D0/ , two/2.0D0/ , twopt5/2.5D0/ , three/3.0D0/ ,
     &     thrpt5/3.5D0/ , twenty/20.0D0/
C
C
C
C
C
      x1 = zero
      x2 = zero
      x3 = zero
      x4 = zero
      x5 = zero
      x6 = zero
      y1 = zero
      y2 = zero
      y3 = zero
      y4 = zero
      y5 = zero
      y6 = zero
      z1 = zero
      z2 = zero
      z3 = zero
      z4 = zero
      z5 = zero
      z6 = zero
      z7 = zero
      z8 = zero
      z9 = zero
      v1 = zero
      v2 = zero
      v3 = zero
      v4 = zero
      v5 = zero
      v6 = zero
      w1 = zero
      w2 = zero
      w3 = zero
      w4 = zero
      w5 = zero
      w6 = zero
      w7 = zero
      w8 = zero
      w9 = zero
      s1 = zero
      s2 = zero
      s3 = zero
      s4 = zero
      s5 = zero
      s6 = zero
      s7 = zero
      s8 = zero
      s9 = zero
      s10 = zero
      s11 = zero
      s12 = zero
      s13 = zero
      s14 = zero
      t1 = zero
      t2 = zero
      t3 = zero
      t4 = zero
      t5 = zero
      t6 = zero
      t7 = zero
      t8 = zero
      t9 = zero
      t10 = zero
      t11 = zero
      t12 = zero
      t13 = zero
      t14 = zero
      c1 = zero
      c2 = zero
      c3 = zero
      c4 = zero
      c5 = zero
      c6 = zero
      do 100 ind = 1 , Ngangb
         Isml = Ismlq + Ismlp(ind)
         if ( Isml.lt.2 ) then
            if ( Isml.lt.1 ) then
               Auxvar = Var1
            else
C
               Auxvar = Var2
            endif
            Eab = Ep(ind)
            Dp00 = Dp00p(ind)
            Dp01 = Dp01p(ind)
            Dp10 = Dp10p(ind)
            Ap = App(ind)
            Bp = Bpp(ind)
            pqab = Aqz - Ap
            pqab2 = pqab*pqab
            g = one/(Eab+Ecd)
            x = g*(Qperp2+pqab2)
            if ( x.le.Auxvar ) then
C
               y = Conp(ind)/dsqrt(Gp(ind)+Gcd)
               gy = g*y
               ggy = g*gy
               gggy = g*ggy
               Qq = x*twenty
               Theta = Qq - dint(Qq)
               N = Qq - Theta
               theta2 = Theta*(Theta-one)
               theta3 = theta2*(Theta-two)
               theta4 = theta2*(Theta+one)
               f0 = (Aa(N+1)+Theta*Ba(N+1)-theta3*Ca(N+1)+theta4*Ca(N+2)
     &              )*y
               f1 = (Ab(N+1)+Theta*Bb(N+1)-theta3*Cb(N+1)+theta4*Cb(N+2)
     &              )*gy
               f2 = (Ac(N+1)+Theta*Bc(N+1)-theta3*Cc(N+1)+theta4*Cc(N+2)
     &              )*ggy
               f3 = (Ad(N+1)+Theta*Bd(N+1)-theta3*Cd(N+1)+theta4*Cd(N+2)
     &              )*gggy
               f4 = (Ae(N+1)+Theta*Be(N+1)-theta3*Ce(N+1)+theta4*Ce(N+2)
     &              )*gggy*g
            else
               f0 = Conp(ind)*dsqrt(Pidiv4/(x*(Gp(ind)+Gcd)))
               gtx = g/x
               f1 = pt5*f0*gtx
               f2 = onept5*f1*gtx
               f3 = twopt5*f2*gtx
               f4 = thrpt5*f3*gtx
            endif
            apbp = Ap*Bp
            eab2 = Eab*Eab
            bpdp01 = Bp*Dp01
            apdp10 = Ap*Dp10
            edp01 = Eab*Dp01
            edp10 = Eab*Dp10
            f1pqab = f1*pqab
            f2pqab = f2*pqab
            f3pqab = f3*pqab
            f4pqab = f4*pqab
            f1pqa2 = f1*pqab2
            f2pqa2 = f2*pqab2
            f3pqa2 = f3*pqab2
            f4pqa2 = f4*pqab2
            f2pqa3 = f2pqa2*pqab
            f3pqa3 = f3pqa2*pqab
            f4pqa3 = f4pqa2*pqab
            x1 = x1 + f0*Dp00
            x2 = x2 + f1*Dp00
            x3 = x3 + f2*Dp00
            x4 = x4 + f1pqab*Dp00
            x5 = x5 + f2pqab*Dp00
            x6 = x6 + f2pqa2*Dp00
            z1 = z1 + f1*edp01
            z2 = z2 + f2*edp01
            z3 = z3 + f3*edp01
            z4 = z4 + f1pqab*edp01
            z5 = z5 + f2pqab*edp01
            z6 = z6 + f3pqab*edp01
            z7 = z7 + f2pqa2*edp01
            z8 = z8 + f3pqa2*edp01
            z9 = z9 + f3pqa3*edp01
            w1 = w1 + f1*edp10
            w2 = w2 + f2*edp10
            w3 = w3 + f3*edp10
            w4 = w4 + f1pqab*edp10
            w5 = w5 + f2pqab*edp10
            w6 = w6 + f3pqab*edp10
            w7 = w7 + f2pqa2*edp10
            w8 = w8 + f3pqa2*edp10
            w9 = w9 + f3pqa3*edp10
            s1 = s1 + f0*Eab
            s2 = s2 + f1*Eab
            s3 = s3 + f2*Eab
            s4 = s4 + f3*Eab
            s6 = s6 + f1pqab*Eab
            s7 = s7 + f2pqab*Eab
            s8 = s8 + f3pqab*Eab
            s9 = s9 + f1pqa2*Eab
            s10 = s10 + f2pqa2*Eab
            s11 = s11 + f3pqa2*Eab
            s12 = s12 + f2pqa3*Eab
            s13 = s13 + f3pqa3*Eab
            s14 = s14 + f3pqa3*pqab*Eab
            t1 = t1 + f0*eab2
            t2 = t2 + f1*eab2
            t3 = t3 + f2*eab2
            t4 = t4 + f3*eab2
            t5 = t5 + f4*eab2
            t6 = t6 + f2pqab*eab2
            t7 = t7 + f3pqab*eab2
            t8 = t8 + f4pqab*eab2
            t9 = t9 + f2pqa2*eab2
            t10 = t10 + f3pqa2*eab2
            t11 = t11 + f4pqa2*eab2
            t12 = t12 + f3pqa3*eab2
            t13 = t13 + f4pqa3*eab2
            t14 = t14 + f4pqa3*pqab*eab2
            if ( Rabsq.ne.0 ) then
               y1 = y1 + f0*bpdp01
               y2 = y2 + f1*bpdp01
               y3 = y3 + f2*bpdp01
               y4 = y4 + f1pqab*bpdp01
               y5 = y5 + f2pqab*bpdp01
               y6 = y6 + f2pqa2*bpdp01
               v1 = v1 + f0*apdp10
               v2 = v2 + f1*apdp10
               v3 = v3 + f2*apdp10
               v4 = v4 + f1pqab*apdp10
               v5 = v5 + f2pqab*apdp10
               v6 = v6 + f2pqa2*apdp10
               c1 = c1 + f0*apbp
               c2 = c2 + f1*apbp
               c3 = c3 + f2*apbp
               c4 = c4 + f1pqab*apbp
               c5 = c5 + f2pqab*apbp
               c6 = c6 + f2pqa2*apbp
            endif
         endif
 100  continue
      a1 = Aqz*s2 - s6
      a2 = Aqz*s3 - s7
      a3 = Aqz*s4 - s8
      a4 = Aqz*s6 - s9
      a5 = Aqz*s7 - s10
      a6 = Aqz*s8 - s11
      a8 = Aqz*s10 - s12
      a9 = Aqz*s11 - s13
      a10 = Aqz*s13 - s14
      bqz = Aqz - Rab
      b1 = bqz*s2 - s6
      b2 = bqz*s3 - s7
      b3 = bqz*s4 - s8
      b4 = bqz*s6 - s9
      b5 = bqz*s7 - s10
      b6 = bqz*s8 - s11
      b8 = bqz*s10 - s12
      b9 = bqz*s11 - s13
      b10 = bqz*s13 - s14
      hecd = pt5*Ecd
      ecd2 = Ecd*Ecd
      hecd2 = pt5*ecd2
      qecd = Qperp*Ecd
      hqecd = pt5*qecd
      qecd2 = Qperp*ecd2
      hqecd2 = pt5*qecd2
      q2ecd = Qperp2*Ecd
      q3ecd = Qperp*q2ecd
      q2ecd2 = Qperp2*ecd2
      q3ecd2 = q2ecd2*Qperp
      H0000 = x1
      H0001 = qecd*x2
      H0003 = -Ecd*x4
      H0022 = hecd*(x1-Ecd*x2)
      H0011 = H0022 + q2ecd2*x3
      H0013 = -qecd2*x5
      H0033 = H0022 + ecd2*x6
      H0100 = -Qperp*z1
      H0300 = z4 + y1
      H0202 = hecd*z1
      H0101 = H0202 - q2ecd*z2
      H0103 = qecd*z5
      H0301 = H0103 + qecd*y2
      H0303 = H0202 - Ecd*z7 - Ecd*y4
      H0212 = hqecd2*z2
      H0223 = -hecd2*z5
      H0122 = H0212 - Qperp*H0202
      H0322 = H0223 + hecd*(H0300-Ecd*y2)
      H0113 = H0223 + q2ecd2*z6
      H0313 = H0212 - qecd2*(z8+y5)
      H0111 = H0122 + H0212 + H0212 - q3ecd2*z3
C      write(6,*) ' h0111 set in sp111 ',h0111
      H0133 = H0122 - qecd2*z8
      H0311 = H0322 + q2ecd2*(z6+y3)
      H0333 = H0322 + H0223 + H0223 + ecd2*(z9+y6)
      H1000 = -Qperp*w1
      H3000 = w4 + v1
      H2002 = hecd*w1
      H1001 = H2002 - q2ecd*w2
      H1003 = qecd*w5
      H3001 = H1003 + qecd*v2
      H3003 = H2002 - Ecd*w7 - Ecd*v4
      H2012 = hqecd2*w2
      H2023 = -hecd2*w5
      H1022 = H2012 - Qperp*H2002
      H3022 = H2023 + hecd*(H3000-Ecd*v2)
      H1013 = H2023 + q2ecd2*w6
      H3013 = H2012 - qecd2*(w8+v5)
      H1011 = H1022 + H2012 + H2012 - q3ecd2*w3
      H1033 = H1022 - qecd2*w8
      H3011 = H3022 + q2ecd2*(w6+v3)
      H3033 = H3022 + H2023 + H2023 + ecd2*(w9+v6)
      H2200 = pt5*(s1-t2)
      H1100 = H2200 + Qperp2*t3
      H1300 = -Qperp*(t6+b1)
      H3100 = -Qperp*(t6+a1)
      H3300 = H2200 + t9 + a4 + b4 + c1
      H2201 = hqecd*(s2-t3)
      H1101 = H2201 - qecd*t3 + q3ecd*t4
      temp = hecd*t6 - q2ecd*t7
      H1301 = temp + hecd*b1 - q2ecd*b2
      H3101 = temp + hecd*a1 - q2ecd*a2
      H3301 = H2201 + qecd*(t10+a5+b5+c2)
      H1202 = -hqecd*t3
      H2102 = H1202
      H2302 = hecd*(t6+b1)
      H3202 = hecd*(t6+a1)
      H2203 = hecd*(t6-s6)
      H1103 = H2203 - q2ecd*t7
      temp = -hqecd*t3 + qecd*t10
      H1303 = temp + qecd*b5
      H3103 = temp + qecd*a5
      H3303 = H2203 + Ecd*(t6-t12-a8-b8-c4) + hecd*(a1+b1)
      H1212 = pt25*ecd2*t3 - pt5*q2ecd2*t4
      H2112 = H1212
      H2312 = hqecd2*(t7+b2)
      H3212 = hqecd2*(t7+a2)
      H1223 = hqecd2*t7
      H2123 = H1223
      H2323 = hecd2*(pt5*t3-t10-b5)
      H3223 = hecd2*(pt5*t3-t10-a5)
      hxxyy = pt25*(Ecd*(s1-t2)-ecd2*(s2-t3))
      H2222 = hxxyy + hecd2*t3
      H1122 = hxxyy + pt5*(q2ecd*t3-q2ecd2*t4)
      temp = hqecd*(Ecd*t7-t6)
      H1322 = temp + hqecd*(Ecd*b2-b1)
      H3122 = temp + hqecd*(Ecd*a2-a1)
      H3322 = hxxyy + hecd*(t9+a4+b4+c1) - hecd2*(t10+a5+b5+c2)
      H2211 = hxxyy + pt5*q2ecd2*(s3-t4)
      H1111 = hxxyy + (hecd2+pt5*q2ecd)
     &        *t3 + q2ecd2*(-three*t4+pt5*s3+Qperp2*t5)
      H1311 = onept5*qecd2*(t7+b2) - hqecd*(t6+b1) - q3ecd2*(b3+t8)
      H3111 = onept5*qecd2*(t7+a2) - hqecd*(t6+a1) - q3ecd2*(a3+t8)
      H3311 = hxxyy - hecd2*(Qperp2*t4+t10+a5+b5)
     &        + hecd*(t9+a4+b4+c1-Ecd*c2) + q2ecd2*(t11+pt5*s3+a6+b6+c3)
      H2213 = hqecd2*(t7-s7)
      H1113 = onept5*qecd2*t7 - hqecd2*s7 - q3ecd2*t8
      temp = hecd2*(pt5*t3-t10) + q2ecd2*(t11-pt5*t4)
      H1313 = temp - hecd2*b5 + q2ecd2*b6
      H3113 = temp - hecd2*a5 + q2ecd2*a6
      H3313 = qecd2*(onept5*t7-t13-a9-b9-c5) - hqecd2*(s7-a2-b2)
      H2233 = hxxyy + hecd2*(s10-t10)
      H1133 = hxxyy - hecd2*(Qperp2*t4+t10-s10) + pt5*q2ecd*t3 +
     &        q2ecd2*t11
      H1333 = qecd2*(onept5*t7-t13-b9) - hqecd*(t6+b1) + hqecd2*b2
      H3133 = qecd2*(onept5*t7-t13-a9) - hqecd*(t6+a1) + hqecd2*a2
      H3333 = hxxyy + hecd2*(-three*(a5+b5)+t3+s10-c2)
     &        + ecd2*(-three*t10+t14+a10+b10+c6) + hecd*(t9+a4+b4+c1)
      return
C
      end

