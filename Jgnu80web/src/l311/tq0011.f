#if(0)
  FTANGLE v1.61,
 created with UNIX on "Friday, September 25, 1998 at 8:02." 
  COMMAND LINE: "ftangle -ybs15000 tq0011"
  RUN TIME:     "Friday, June 5, 2009 at 15:05."
  WEB FILE:     "tq0011.web"
  CHANGE FILE:  (none) 
#endif


      subroutine tq0011
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
      double precision G0111 , G0112 , G0113 , G0122 , G0123 , G0133 ,
     &                 G0200 , G0201 , G0202 , G0203 , G0211 , G0212 ,
     &                 G0213 , G0222 , G0223 , G0233 , G0300 , G0301 ,
     &                 G0302 , G0303
      double precision G0311 , G0312 , G0313 , G0322 , G0323 , G0333 ,
     &                 G1000 , G1001 , G1002 , G1003 , G1011 , G1012 ,
     &                 G1013 , G1022 , G1023 , G1033 , G1100 , G1101 ,
     &                 G1102 , G1103
      double precision G1111 , G1112 , G1113 , G1122 , G1123 , G1133 ,
     &                 G1200 , G1201 , G1202 , G1203 , G1211 , G1212 ,
     &                 G1213 , G1222 , G1223 , G1233 , G1300 , G1301 ,
     &                 G1302 , G1303
      double precision G1311 , G1312 , G1313 , G1322 , G1323 , G1333 ,
     &                 G2000 , G2001 , G2002 , G2003 , G2011 , G2012 ,
     &                 G2013 , G2022 , G2023 , G2033 , G2100 , G2101 ,
     &                 G2102 , G2103
      double precision G2111 , G2112 , G2113 , G2122 , G2123 , G2133 ,
     &                 G2200 , G2201 , G2202 , G2203 , G2211 , G2212 ,
     &                 G2213 , G2222 , G2223 , G2233 , G2300 , G2301 ,
     &                 G2302 , G2303
      double precision G2311 , G2312 , G2313 , G2322 , G2323 , G2333 ,
     &                 G3000 , G3001 , G3002 , G3003 , G3011 , G3012 ,
     &                 G3013 , G3022 , G3023 , G3033 , G3100 , G3101 ,
     &                 G3102 , G3103
      double precision G3111 , G3112 , G3113 , G3122 , G3123 , G3133 ,
     &                 G3200 , G3201 , G3202 , G3203 , G3211 , G3212 ,
     &                 G3213 , G3222 , G3223 , G3233 , G3300 , G3301 ,
     &                 G3302 , G3303
      double precision G3311 , G3312 , G3313 , G3322 , G3323 , G3333 ,
     &                 Gout , P11 , P12 , P13 , P21 , P22 , P23 , P31 ,
     &                 P32 , P33 , Pq1 , Pq2 , Pq3 , Px
      double precision Py , Pz , Q11 , Q12 , Q13 , Q21 , Q22 , Q23 ,
     &                 Q31 , Q32 , Q33 , Qperp , Qperp2 , Qx , Qy , Qz ,
     &                 r13 , r14 , r33 , r34
      double precision Rab , Rabsq , Rcd , Rcdsq , Rpq , Rpqsq , Sing
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
      if ( Rcdsq.gt.0 ) then
         g0010 = g0010 + r13*G0000
         G0011 = G0011 + r13*G0001
         G0012 = G0012 + r13*G0002
         G0013 = G0013 + r13*G0003
         g0030 = g0030 + r33*G0000
         g0031 = g0031 + r33*G0001
         g0032 = g0032 + r33*G0002
         G0033 = G0033 + r33*G0003
         G0001 = G0001 + r14*G0000
         G0011 = G0011 + r14*g0010
         g0021 = g0021 + r14*g0020
         g0031 = g0031 + r14*g0030
         G0003 = G0003 + r34*G0000
         G0013 = G0013 + r34*g0010
         G0023 = G0023 + r34*g0020
         G0033 = G0033 + r34*g0030
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
      return
C
      end

