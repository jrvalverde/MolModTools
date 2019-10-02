#if(0)
  FTANGLE v1.61,
 created with UNIX on "Friday, September 25, 1998 at 8:02." 
  COMMAND LINE: "ftangle -ybs15000 tq0111"
  RUN TIME:     "Friday, June 5, 2009 at 15:05."
  WEB FILE:     "tq0111.web"
  CHANGE FILE:  (none) 
#endif


      subroutine tq0111
      implicit none
      double precision Acx , Acy , Acy2 , Acz , Ap , Aqx , Aqz , Ax ,
     &                 Ay , Az , Bp , Bx , By , Bz , C11 , C12 , C13 ,
     &                 C21 , C22 , C23
      double precision C31 , C32 , C33 , Cosg , Cq , Cx , Cy , Cz ,
     &                 Dp00 , Dp01 , Dp10 , Dp11 , Dq , Dq00 , Dq01 ,
     &                 Dq10 , Dq11 , Dx , Dy , Dz
      double precision G0000 , G0001 , G0002 , G0003 , g0010 , G0011 ,
     &                 G0012 , G0013 , g0020 , g0021 , G0022 , G0023 ,
     &                 g0030 , g0031 , g0032 , G0033 , G0100 , G0101 ,
     &                 G0102 , G0103
      double precision g0110 , G0111 , G0112 , G0113 , g0120 , g0121 ,
     &                 G0122 , G0123 , g0130 , g0131 , g0132 , G0133 ,
     &                 G0200 , G0201 , G0202 , G0203 , g0210 , G0211 ,
     &                 G0212 , G0213
      double precision g0220 , g0221 , G0222 , G0223 , g0230 , g0231 ,
     &                 g0232 , G0233 , G0300 , G0301 , G0302 , G0303 ,
     &                 g0310 , G0311 , G0312 , G0313 , g0320 , g0321 ,
     &                 G0322 , G0323
      double precision g0330 , g0331 , g0332 , G0333 , G1000 , G1001 ,
     &                 G1002 , G1003 , G1011 , G1012 , G1013 , G1022 ,
     &                 G1023 , G1033 , G1100 , G1101 , G1102 , G1103 ,
     &                 G1111 , G1112
      double precision G1113 , G1122 , G1123 , G1133 , G1200 , G1201 ,
     &                 G1202 , G1203 , G1211 , G1212 , G1213 , G1222 ,
     &                 G1223 , G1233 , G1300 , G1301 , G1302 , G1303 ,
     &                 G1311 , G1312
      double precision G1313 , G1322 , G1323 , G1333 , G2000 , G2001 ,
     &                 G2002 , G2003 , G2011 , G2012 , G2013 , G2022 ,
     &                 G2023 , G2033 , G2100 , G2101 , G2102 , G2103 ,
     &                 G2111 , G2112
      double precision G2113 , G2122 , G2123 , G2133 , G2200 , G2201 ,
     &                 G2202 , G2203 , G2211 , G2212 , G2213 , G2222 ,
     &                 G2223 , G2233 , G2300 , G2301 , G2302 , G2303 ,
     &                 G2311 , G2312
      double precision G2313 , G2322 , G2323 , G2333 , G3000 , G3001 ,
     &                 G3002 , G3003 , G3011 , G3012 , G3013 , G3022 ,
     &                 G3023 , G3033 , G3100 , G3101 , G3102 , G3103 ,
     &                 G3111 , G3112
      double precision G3113 , G3122 , G3123 , G3133 , G3200 , G3201 ,
     &                 G3202 , G3203 , G3211 , G3212 , G3213 , G3222 ,
     &                 G3223 , G3233 , G3300 , G3301 , G3302 , G3303 ,
     &                 G3311 , G3312
      double precision G3313 , G3322 , G3323 , G3333 , Gout , P11 ,
     &                 P12 , P13 , P21 , P22 , P23 , P31 , P32 , P33 ,
     &                 Pq1 , Pq2 , Pq3 , Px , Py , Pz
      double precision Q11 , Q12 , Q13 , Q21 , Q22 , Q23 , Q31 , Q32 ,
     &                 Q33 , Qperp , Qperp2 , Qx , Qy , Qz , r13 , r14 ,
     &                 r33 , r34 , Rab , Rabsq
      double precision Rcd , Rcdsq , Rpq , Rpqsq , Sing
      common /g     / G0000 , G0001 , G0002 , G0003 , G0011 , G0012 ,
     &                G0013 , G0022 , G0023 , G0033 , G0100 , G0101 ,
     &                G0102 , G0103 , G0111 , G0112 , G0113 , G0122 ,
     &                G0123 , G0133 , G0200 , G0201 , G0202 , G0203 ,
     &                G0211 , G0212 , G0213 , G0222 , G0223 , G0233 ,
     &                G0300 , G0301 , G0302 , G0303 , G0311 , G0312 ,
     &                G0313 , G0322 , G0323 , G0333 , G1000 , G1001 ,
     &                G1002 , G1003 , G1011 , G1012 , G1013 , G1022 ,
     &                G1023 , G1033 , G1100 , G1101 , G1102 , G1103 ,
     &                G1111 , G1112 , G1113 , G1122 , G1123 , G1133 ,
     &                G1200 , G1201 , G1202 , G1203 , G1211 , G1212 ,
     &                G1213 , G1222 , G1223 , G1233 , G1300 , G1301 ,
     &                G1302 , G1303 , G1311 , G1312 , G1313 , G1322 ,
     &                G1323 , G1333 , G2000 , G2001 , G2002 , G2003 ,
     &                G2011 , G2012 , G2013 , G2022 , G2023 , G2033 ,
     &                G2100 , G2101 , G2102 , G2103 , G2111 , G2112 ,
     &                G2113 , G2122 , G2123 , G2133 , G2200 , G2201 ,
     &                G2202 , G2203 , G2211 , G2212 , G2213 , G2222 ,
     &                G2223 , G2233 , G2300 , G2301 , G2302 , G2303 ,
     &                G2311 , G2312 , G2313 , G2322 , G2323 , G2333 ,
     &                G3000 , G3001 , G3002 , G3003 , G3011 , G3012 ,
     &                G3013 , G3022 , G3023 , G3033 , G3100 , G3101 ,
     &                G3102 , G3103 , G3111 , G3112 , G3113 , G3122 ,
     &                G3123 , G3133 , G3200 , G3201 , G3202 , G3203 ,
     &                G3211 , G3212 , G3213 , G3222 , G3223 , G3233 ,
     &                G3300 , G3301 , G3302 , G3303 , G3311 , G3312 ,
     &                G3313 , G3322 , G3323 , G3333
      common /cgeom / Ax , Ay , Az , Bx , By , Bz , Cx , Cy , Cz , Dx ,
     &                Dy , Dz , Rab , Rabsq , Rcd , Rcdsq , P11 , P12 ,
     &                P13 , P21 , P22 , P23 , P31 , P32 , P33 , Q11 ,
     &                Q12 , Q13 , Q21 , Q22 , Q23 , Q31 , Q32 , Q33
      common /pqgeom/ Ap , Bp , Cq , Dq , Px , Py , Pz , Qx , Qy , Qz ,
     &                Rpq , Rpqsq , Pq1 , Pq2 , Pq3 , C11 , C12 , C13 ,
     &                C21 , C22 , C23 , C31 , C32 , C33
      common /qgeom / Acx , Acy , Acz , Acy2 , Cosg , Sing , Aqx , Aqz ,
     &                Qperp , Qperp2
      common /dpq   / Dp00 , Dp01 , Dp10 , Dp11 , Dq00 , Dq01 , Dq10 ,
     &                Dq11
      common /gout  / Gout(256)
C
C
C     translates up to 160 integrals on a b and q to up to 256 integrals
C     on a b c and d
C
C
C
C     r13=component of cq along penultimate x-axis
C     r33=component of cq along penultimate z-axis
C     r14=component of dq along penultimate x-axis
C     r34=component of dq along penultimate z-axis
C
      r13 = Cq*Sing
      r33 = Cq*Cosg
      r14 = Dq*Sing
      r34 = Dq*Cosg
      g0010 = G0001
      g0020 = G0002
      g0021 = G0012
      g0030 = G0003
      g0031 = G0013
      g0032 = G0023
      g0110 = G0101
      g0120 = G0102
      g0121 = G0112
      g0130 = G0103
      g0131 = G0113
      g0132 = G0123
      g0210 = G0201
      g0220 = G0202
      g0221 = G0212
      g0230 = G0203
      g0231 = G0213
      g0232 = G0223
      g0310 = G0301
      g0320 = G0302
      g0321 = G0312
      g0330 = G0303
      g0331 = G0313
      g0332 = G0323
      if ( Rcdsq.gt.0 ) then
         g0010 = g0010 + r13*G0000
         G0011 = G0011 + r13*G0001
         G0012 = G0012 + r13*G0002
         G0013 = G0013 + r13*G0003
         g0110 = g0110 + r13*G0100
         G0111 = G0111 + r13*G0101
         G0112 = G0112 + r13*G0102
         G0113 = G0113 + r13*G0103
         g0210 = g0210 + r13*G0200
         G0211 = G0211 + r13*G0201
         G0212 = G0212 + r13*G0202
         G0213 = G0213 + r13*G0203
         g0310 = g0310 + r13*G0300
         G0311 = G0311 + r13*G0301
         G0312 = G0312 + r13*G0302
         G0313 = G0313 + r13*G0303
         g0030 = g0030 + r33*G0000
         g0031 = g0031 + r33*G0001
         g0032 = g0032 + r33*G0002
         G0033 = G0033 + r33*G0003
         g0130 = g0130 + r33*G0100
         g0131 = g0131 + r33*G0101
         g0132 = g0132 + r33*G0102
         G0133 = G0133 + r33*G0103
         g0230 = g0230 + r33*G0200
         g0231 = g0231 + r33*G0201
         g0232 = g0232 + r33*G0202
         G0233 = G0233 + r33*G0203
         g0330 = g0330 + r33*G0300
         g0331 = g0331 + r33*G0301
         g0332 = g0332 + r33*G0302
         G0333 = G0333 + r33*G0303
         G0001 = G0001 + r14*G0000
         G0011 = G0011 + r14*g0010
         g0021 = g0021 + r14*g0020
         g0031 = g0031 + r14*g0030
         G0101 = G0101 + r14*G0100
         G0111 = G0111 + r14*g0110
         g0121 = g0121 + r14*g0120
         g0131 = g0131 + r14*g0130
         G0201 = G0201 + r14*G0200
         G0211 = G0211 + r14*g0210
         g0221 = g0221 + r14*g0220
         g0231 = g0231 + r14*g0230
         G0301 = G0301 + r14*G0300
         G0311 = G0311 + r14*g0310
         g0321 = g0321 + r14*g0320
         g0331 = g0331 + r14*g0330
         G0003 = G0003 + r34*G0000
         G0013 = G0013 + r34*g0010
         G0023 = G0023 + r34*g0020
         G0033 = G0033 + r34*g0030
         G0103 = G0103 + r34*G0100
         G0113 = G0113 + r34*g0110
         G0123 = G0123 + r34*g0120
         G0133 = G0133 + r34*g0130
         G0203 = G0203 + r34*G0200
         G0213 = G0213 + r34*g0210
         G0223 = G0223 + r34*g0220
         G0233 = G0233 + r34*g0230
         G0303 = G0303 + r34*G0300
         G0313 = G0313 + r34*g0310
         G0323 = G0323 + r34*g0320
         G0333 = G0333 + r34*g0330
      endif
      Gout(1) = Gout(1) + G0000*Dq00
      Gout(2) = Gout(2) + G0001*Dq01
      Gout(3) = Gout(3) + G0002*Dq01
      Gout(4) = Gout(4) + G0003*Dq01
      Gout(5) = Gout(5) + g0010*Dq10
      Gout(6) = Gout(6) + G0011*Dq11
      Gout(7) = Gout(7) + G0012*Dq11
      Gout(8) = Gout(8) + G0013*Dq11
      Gout(9) = Gout(9) + g0020*Dq10
      Gout(10) = Gout(10) + g0021*Dq11
      Gout(11) = Gout(11) + G0022*Dq11
      Gout(12) = Gout(12) + G0023*Dq11
      Gout(13) = Gout(13) + g0030*Dq10
      Gout(14) = Gout(14) + g0031*Dq11
      Gout(15) = Gout(15) + g0032*Dq11
      Gout(16) = Gout(16) + G0033*Dq11
      Gout(17) = Gout(17) + G0100*Dq00
      Gout(18) = Gout(18) + G0101*Dq01
      Gout(19) = Gout(19) + G0102*Dq01
      Gout(20) = Gout(20) + G0103*Dq01
      Gout(21) = Gout(21) + g0110*Dq10
      Gout(22) = Gout(22) + G0111*Dq11
      Gout(23) = Gout(23) + G0112*Dq11
      Gout(24) = Gout(24) + G0113*Dq11
      Gout(25) = Gout(25) + g0120*Dq10
      Gout(26) = Gout(26) + g0121*Dq11
      Gout(27) = Gout(27) + G0122*Dq11
      Gout(28) = Gout(28) + G0123*Dq11
      Gout(29) = Gout(29) + g0130*Dq10
      Gout(30) = Gout(30) + g0131*Dq11
      Gout(31) = Gout(31) + g0132*Dq11
      Gout(32) = Gout(32) + G0133*Dq11
      Gout(33) = Gout(33) + G0200*Dq00
      Gout(34) = Gout(34) + G0201*Dq01
      Gout(35) = Gout(35) + G0202*Dq01
      Gout(36) = Gout(36) + G0203*Dq01
      Gout(37) = Gout(37) + g0210*Dq10
      Gout(38) = Gout(38) + G0211*Dq11
      Gout(39) = Gout(39) + G0212*Dq11
      Gout(40) = Gout(40) + G0213*Dq11
      Gout(41) = Gout(41) + g0220*Dq10
      Gout(42) = Gout(42) + g0221*Dq11
      Gout(43) = Gout(43) + G0222*Dq11
      Gout(44) = Gout(44) + G0223*Dq11
      Gout(45) = Gout(45) + g0230*Dq10
      Gout(46) = Gout(46) + g0231*Dq11
      Gout(47) = Gout(47) + g0232*Dq11
      Gout(48) = Gout(48) + G0233*Dq11
      Gout(49) = Gout(49) + G0300*Dq00
      Gout(50) = Gout(50) + G0301*Dq01
      Gout(51) = Gout(51) + G0302*Dq01
      Gout(52) = Gout(52) + G0303*Dq01
      Gout(53) = Gout(53) + g0310*Dq10
      Gout(54) = Gout(54) + G0311*Dq11
      Gout(55) = Gout(55) + G0312*Dq11
      Gout(56) = Gout(56) + G0313*Dq11
      Gout(57) = Gout(57) + g0320*Dq10
      Gout(58) = Gout(58) + g0321*Dq11
      Gout(59) = Gout(59) + G0322*Dq11
      Gout(60) = Gout(60) + G0323*Dq11
      Gout(61) = Gout(61) + g0330*Dq10
      Gout(62) = Gout(62) + g0331*Dq11
      Gout(63) = Gout(63) + g0332*Dq11
      Gout(64) = Gout(64) + G0333*Dq11
      return
C
      end

