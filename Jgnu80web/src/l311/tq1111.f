#if(0)
  FTANGLE v1.61,
 created with UNIX on "Friday, September 25, 1998 at 8:02." 
  COMMAND LINE: "ftangle -ybs15000 tq1111"
  RUN TIME:     "Friday, June 5, 2009 at 15:05."
  WEB FILE:     "tq1111.web"
  CHANGE FILE:  (none) 
#endif


      subroutine tq1111
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
     &                 G1002 , G1003 , g1010 , G1011 , G1012 , G1013 ,
     &                 g1020 , g1021 , G1022 , G1023 , g1030 , g1031 ,
     &                 g1032 , G1033
      double precision G1100 , G1101 , G1102 , G1103 , g1110 , G1111 ,
     &                 G1112 , G1113 , g1120 , g1121 , G1122 , G1123 ,
     &                 g1130 , g1131 , g1132 , G1133 , G1200 , G1201 ,
     &                 G1202 , G1203
      double precision g1210 , G1211 , G1212 , G1213 , g1220 , g1221 ,
     &                 G1222 , G1223 , g1230 , g1231 , g1232 , G1233 ,
     &                 G1300 , G1301 , G1302 , G1303 , g1310 , G1311 ,
     &                 G1312 , G1313
      double precision g1320 , g1321 , G1322 , G1323 , g1330 , g1331 ,
     &                 g1332 , G1333 , G2000 , G2001 , G2002 , G2003 ,
     &                 g2010 , G2011 , G2012 , G2013 , g2020 , g2021 ,
     &                 G2022 , G2023
      double precision g2030 , g2031 , g2032 , G2033 , G2100 , G2101 ,
     &                 G2102 , G2103 , g2110 , G2111 , G2112 , G2113 ,
     &                 g2120 , g2121 , G2122 , G2123 , g2130 , g2131 ,
     &                 g2132 , G2133
      double precision G2200 , G2201 , G2202 , G2203 , g2210 , G2211 ,
     &                 G2212 , G2213 , g2220 , g2221 , G2222 , G2223 ,
     &                 g2230 , g2231 , g2232 , G2233 , G2300 , G2301 ,
     &                 G2302 , G2303
      double precision g2310 , G2311 , G2312 , G2313 , g2320 , g2321 ,
     &                 G2322 , G2323 , g2330 , g2331 , g2332 , G2333 ,
     &                 G3000 , G3001 , G3002 , G3003 , g3010 , G3011 ,
     &                 G3012 , G3013
      double precision g3020 , g3021 , G3022 , G3023 , g3030 , g3031 ,
     &                 g3032 , G3033 , G3100 , G3101 , G3102 , G3103 ,
     &                 g3110 , G3111 , G3112 , G3113 , g3120 , g3121 ,
     &                 G3122 , G3123
      double precision g3130 , g3131 , g3132 , G3133 , G3200 , G3201 ,
     &                 G3202 , G3203 , g3210 , G3211 , G3212 , G3213 ,
     &                 g3220 , g3221 , G3222 , G3223 , g3230 , g3231 ,
     &                 g3232 , G3233
      double precision G3300 , G3301 , G3302 , G3303 , g3310 , G3311 ,
     &                 G3312 , G3313 , g3320 , g3321 , G3322 , G3323 ,
     &                 g3330 , g3331 , g3332 , G3333 , Gout , P11 ,
     &                 P12 , P13
      double precision P21 , P22 , P23 , P31 , P32 , P33 , Pq1 , Pq2 ,
     &                 Pq3 , Px , Py , Pz , Q11 , Q12 , Q13 , Q21 ,
     &                 Q22 , Q23 , Q31 , Q32
      double precision Q33 , Qperp , Qperp2 , Qx , Qy , Qz , r13 , r14 ,
     &                 r33 , r34 , Rab , Rabsq , Rcd , Rcdsq , Rpq ,
     &                 Rpqsq , Sing
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
      g1010 = G1001
      g1020 = G1002
      g1021 = G1012
      g1030 = G1003
      g1031 = G1013
      g1032 = G1023
      g1110 = G1101
      g1120 = G1102
      g1121 = G1112
      g1130 = G1103
      g1131 = G1113
      g1132 = G1123
      g1210 = G1201
      g1220 = G1202
      g1221 = G1212
      g1230 = G1203
      g1231 = G1213
      g1232 = G1223
      g1310 = G1301
      g1320 = G1302
      g1321 = G1312
      g1330 = G1303
      g1331 = G1313
      g1332 = G1323
      g2010 = G2001
      g2020 = G2002
      g2021 = G2012
      g2030 = G2003
      g2031 = G2013
      g2032 = G2023
      g2110 = G2101
      g2120 = G2102
      g2121 = G2112
      g2130 = G2103
      g2131 = G2113
      g2132 = G2123
      g2210 = G2201
      g2220 = G2202
      g2221 = G2212
      g2230 = G2203
      g2231 = G2213
      g2232 = G2223
      g2310 = G2301
      g2320 = G2302
      g2321 = G2312
      g2330 = G2303
      g2331 = G2313
      g2332 = G2323
      g3010 = G3001
      g3020 = G3002
      g3021 = G3012
      g3030 = G3003
      g3031 = G3013
      g3032 = G3023
      g3110 = G3101
      g3120 = G3102
      g3121 = G3112
      g3130 = G3103
      g3131 = G3113
      g3132 = G3123
      g3210 = G3201
      g3220 = G3202
      g3221 = G3212
      g3230 = G3203
      g3231 = G3213
      g3232 = G3223
      g3310 = G3301
      g3320 = G3302
      g3321 = G3312
      g3330 = G3303
      g3331 = G3313
      g3332 = G3323
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
         g1010 = g1010 + r13*G1000
         G1011 = G1011 + r13*G1001
         G1012 = G1012 + r13*G1002
         G1013 = G1013 + r13*G1003
         g1110 = g1110 + r13*G1100
         G1111 = G1111 + r13*G1101
         G1112 = G1112 + r13*G1102
         G1113 = G1113 + r13*G1103
         g1210 = g1210 + r13*G1200
         G1211 = G1211 + r13*G1201
         G1212 = G1212 + r13*G1202
         G1213 = G1213 + r13*G1203
         g1310 = g1310 + r13*G1300
         G1311 = G1311 + r13*G1301
         G1312 = G1312 + r13*G1302
         G1313 = G1313 + r13*G1303
         g2010 = g2010 + r13*G2000
         G2011 = G2011 + r13*G2001
         G2012 = G2012 + r13*G2002
         G2013 = G2013 + r13*G2003
         g2110 = g2110 + r13*G2100
         G2111 = G2111 + r13*G2101
         G2112 = G2112 + r13*G2102
         G2113 = G2113 + r13*G2103
         g2210 = g2210 + r13*G2200
         G2211 = G2211 + r13*G2201
         G2212 = G2212 + r13*G2202
         G2213 = G2213 + r13*G2203
         g2310 = g2310 + r13*G2300
         G2311 = G2311 + r13*G2301
         G2312 = G2312 + r13*G2302
         G2313 = G2313 + r13*G2303
         g3010 = g3010 + r13*G3000
         G3011 = G3011 + r13*G3001
         G3012 = G3012 + r13*G3002
         G3013 = G3013 + r13*G3003
         g3110 = g3110 + r13*G3100
         G3111 = G3111 + r13*G3101
         G3112 = G3112 + r13*G3102
         G3113 = G3113 + r13*G3103
         g3210 = g3210 + r13*G3200
         G3211 = G3211 + r13*G3201
         G3212 = G3212 + r13*G3202
         G3213 = G3213 + r13*G3203
         g3310 = g3310 + r13*G3300
         G3311 = G3311 + r13*G3301
         G3312 = G3312 + r13*G3302
         G3313 = G3313 + r13*G3303
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
         g1030 = g1030 + r33*G1000
         g1031 = g1031 + r33*G1001
         g1032 = g1032 + r33*G1002
         G1033 = G1033 + r33*G1003
         g1130 = g1130 + r33*G1100
         g1131 = g1131 + r33*G1101
         g1132 = g1132 + r33*G1102
         G1133 = G1133 + r33*G1103
         g1230 = g1230 + r33*G1200
         g1231 = g1231 + r33*G1201
         g1232 = g1232 + r33*G1202
         G1233 = G1233 + r33*G1203
         g1330 = g1330 + r33*G1300
         g1331 = g1331 + r33*G1301
         g1332 = g1332 + r33*G1302
         G1333 = G1333 + r33*G1303
         g2030 = g2030 + r33*G2000
         g2031 = g2031 + r33*G2001
         g2032 = g2032 + r33*G2002
         G2033 = G2033 + r33*G2003
         g2130 = g2130 + r33*G2100
         g2131 = g2131 + r33*G2101
         g2132 = g2132 + r33*G2102
         G2133 = G2133 + r33*G2103
         g2230 = g2230 + r33*G2200
         g2231 = g2231 + r33*G2201
         g2232 = g2232 + r33*G2202
         G2233 = G2233 + r33*G2203
         g2330 = g2330 + r33*G2300
         g2331 = g2331 + r33*G2301
         g2332 = g2332 + r33*G2302
         G2333 = G2333 + r33*G2303
         g3030 = g3030 + r33*G3000
         g3031 = g3031 + r33*G3001
         g3032 = g3032 + r33*G3002
         G3033 = G3033 + r33*G3003
         g3130 = g3130 + r33*G3100
         g3131 = g3131 + r33*G3101
         g3132 = g3132 + r33*G3102
         G3133 = G3133 + r33*G3103
         g3230 = g3230 + r33*G3200
         g3231 = g3231 + r33*G3201
         g3232 = g3232 + r33*G3202
         G3233 = G3233 + r33*G3203
         g3330 = g3330 + r33*G3300
         g3331 = g3331 + r33*G3301
         g3332 = g3332 + r33*G3302
         G3333 = G3333 + r33*G3303
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
         G1001 = G1001 + r14*G1000
         G1011 = G1011 + r14*g1010
         g1021 = g1021 + r14*g1020
         g1031 = g1031 + r14*g1030
         G1101 = G1101 + r14*G1100
         G1111 = G1111 + r14*g1110
         g1121 = g1121 + r14*g1120
         g1131 = g1131 + r14*g1130
         G1201 = G1201 + r14*G1200
         G1211 = G1211 + r14*g1210
         g1221 = g1221 + r14*g1220
         g1231 = g1231 + r14*g1230
         G1301 = G1301 + r14*G1300
         G1311 = G1311 + r14*g1310
         g1321 = g1321 + r14*g1320
         g1331 = g1331 + r14*g1330
         G2001 = G2001 + r14*G2000
         G2011 = G2011 + r14*g2010
         g2021 = g2021 + r14*g2020
         g2031 = g2031 + r14*g2030
         G2101 = G2101 + r14*G2100
         G2111 = G2111 + r14*g2110
         g2121 = g2121 + r14*g2120
         g2131 = g2131 + r14*g2130
         G2201 = G2201 + r14*G2200
         G2211 = G2211 + r14*g2210
         g2221 = g2221 + r14*g2220
         g2231 = g2231 + r14*g2230
         G2301 = G2301 + r14*G2300
         G2311 = G2311 + r14*g2310
         g2321 = g2321 + r14*g2320
         g2331 = g2331 + r14*g2330
         G3001 = G3001 + r14*G3000
         G3011 = G3011 + r14*g3010
         g3021 = g3021 + r14*g3020
         g3031 = g3031 + r14*g3030
         G3101 = G3101 + r14*G3100
         G3111 = G3111 + r14*g3110
         g3121 = g3121 + r14*g3120
         g3131 = g3131 + r14*g3130
         G3201 = G3201 + r14*G3200
         G3211 = G3211 + r14*g3210
         g3221 = g3221 + r14*g3220
         g3231 = g3231 + r14*g3230
         G3301 = G3301 + r14*G3300
         G3311 = G3311 + r14*g3310
         g3321 = g3321 + r14*g3320
         g3331 = g3331 + r14*g3330
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
         G1003 = G1003 + r34*G1000
         G1013 = G1013 + r34*g1010
         G1023 = G1023 + r34*g1020
         G1033 = G1033 + r34*g1030
         G1103 = G1103 + r34*G1100
         G1113 = G1113 + r34*g1110
         G1123 = G1123 + r34*g1120
         G1133 = G1133 + r34*g1130
         G1203 = G1203 + r34*G1200
         G1213 = G1213 + r34*g1210
         G1223 = G1223 + r34*g1220
         G1233 = G1233 + r34*g1230
         G1303 = G1303 + r34*G1300
         G1313 = G1313 + r34*g1310
         G1323 = G1323 + r34*g1320
         G1333 = G1333 + r34*g1330
         G2003 = G2003 + r34*G2000
         G2013 = G2013 + r34*g2010
         G2023 = G2023 + r34*g2020
         G2033 = G2033 + r34*g2030
         G2103 = G2103 + r34*G2100
         G2113 = G2113 + r34*g2110
         G2123 = G2123 + r34*g2120
         G2133 = G2133 + r34*g2130
         G2203 = G2203 + r34*G2200
         G2213 = G2213 + r34*g2210
         G2223 = G2223 + r34*g2220
         G2233 = G2233 + r34*g2230
         G2303 = G2303 + r34*G2300
         G2313 = G2313 + r34*g2310
         G2323 = G2323 + r34*g2320
         G2333 = G2333 + r34*g2330
         G3003 = G3003 + r34*G3000
         G3013 = G3013 + r34*g3010
         G3023 = G3023 + r34*g3020
         G3033 = G3033 + r34*g3030
         G3103 = G3103 + r34*G3100
         G3113 = G3113 + r34*g3110
         G3123 = G3123 + r34*g3120
         G3133 = G3133 + r34*g3130
         G3203 = G3203 + r34*G3200
         G3213 = G3213 + r34*g3210
         G3223 = G3223 + r34*g3220
         G3233 = G3233 + r34*g3230
         G3303 = G3303 + r34*G3300
         G3313 = G3313 + r34*g3310
         G3323 = G3323 + r34*g3320
         G3333 = G3333 + r34*g3330
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
      Gout(65) = Gout(65) + G1000*Dq00
      Gout(66) = Gout(66) + G1001*Dq01
      Gout(67) = Gout(67) + G1002*Dq01
      Gout(68) = Gout(68) + G1003*Dq01
      Gout(69) = Gout(69) + g1010*Dq10
      Gout(70) = Gout(70) + G1011*Dq11
      Gout(71) = Gout(71) + G1012*Dq11
      Gout(72) = Gout(72) + G1013*Dq11
      Gout(73) = Gout(73) + g1020*Dq10
      Gout(74) = Gout(74) + g1021*Dq11
      Gout(75) = Gout(75) + G1022*Dq11
      Gout(76) = Gout(76) + G1023*Dq11
      Gout(77) = Gout(77) + g1030*Dq10
      Gout(78) = Gout(78) + g1031*Dq11
      Gout(79) = Gout(79) + g1032*Dq11
      Gout(80) = Gout(80) + G1033*Dq11
      Gout(81) = Gout(81) + G1100*Dq00
      Gout(82) = Gout(82) + G1101*Dq01
      Gout(83) = Gout(83) + G1102*Dq01
      Gout(84) = Gout(84) + G1103*Dq01
      Gout(85) = Gout(85) + g1110*Dq10
      Gout(86) = Gout(86) + G1111*Dq11
      Gout(87) = Gout(87) + G1112*Dq11
      Gout(88) = Gout(88) + G1113*Dq11
      Gout(89) = Gout(89) + g1120*Dq10
      Gout(90) = Gout(90) + g1121*Dq11
      Gout(91) = Gout(91) + G1122*Dq11
      Gout(92) = Gout(92) + G1123*Dq11
      Gout(93) = Gout(93) + g1130*Dq10
      Gout(94) = Gout(94) + g1131*Dq11
      Gout(95) = Gout(95) + g1132*Dq11
      Gout(96) = Gout(96) + G1133*Dq11
      Gout(97) = Gout(97) + G1200*Dq00
      Gout(98) = Gout(98) + G1201*Dq01
      Gout(99) = Gout(99) + G1202*Dq01
      Gout(100) = Gout(100) + G1203*Dq01
      Gout(101) = Gout(101) + g1210*Dq10
      Gout(102) = Gout(102) + G1211*Dq11
      Gout(103) = Gout(103) + G1212*Dq11
      Gout(104) = Gout(104) + G1213*Dq11
      Gout(105) = Gout(105) + g1220*Dq10
      Gout(106) = Gout(106) + g1221*Dq11
      Gout(107) = Gout(107) + G1222*Dq11
      Gout(108) = Gout(108) + G1223*Dq11
      Gout(109) = Gout(109) + g1230*Dq10
      Gout(110) = Gout(110) + g1231*Dq11
      Gout(111) = Gout(111) + g1232*Dq11
      Gout(112) = Gout(112) + G1233*Dq11
      Gout(113) = Gout(113) + G1300*Dq00
      Gout(114) = Gout(114) + G1301*Dq01
      Gout(115) = Gout(115) + G1302*Dq01
      Gout(116) = Gout(116) + G1303*Dq01
      Gout(117) = Gout(117) + g1310*Dq10
      Gout(118) = Gout(118) + G1311*Dq11
      Gout(119) = Gout(119) + G1312*Dq11
      Gout(120) = Gout(120) + G1313*Dq11
      Gout(121) = Gout(121) + g1320*Dq10
      Gout(122) = Gout(122) + g1321*Dq11
      Gout(123) = Gout(123) + G1322*Dq11
      Gout(124) = Gout(124) + G1323*Dq11
      Gout(125) = Gout(125) + g1330*Dq10
      Gout(126) = Gout(126) + g1331*Dq11
      Gout(127) = Gout(127) + g1332*Dq11
      Gout(128) = Gout(128) + G1333*Dq11
      Gout(129) = Gout(129) + G2000*Dq00
      Gout(130) = Gout(130) + G2001*Dq01
      Gout(131) = Gout(131) + G2002*Dq01
      Gout(132) = Gout(132) + G2003*Dq01
      Gout(133) = Gout(133) + g2010*Dq10
      Gout(134) = Gout(134) + G2011*Dq11
      Gout(135) = Gout(135) + G2012*Dq11
      Gout(136) = Gout(136) + G2013*Dq11
      Gout(137) = Gout(137) + g2020*Dq10
      Gout(138) = Gout(138) + g2021*Dq11
      Gout(139) = Gout(139) + G2022*Dq11
      Gout(140) = Gout(140) + G2023*Dq11
      Gout(141) = Gout(141) + g2030*Dq10
      Gout(142) = Gout(142) + g2031*Dq11
      Gout(143) = Gout(143) + g2032*Dq11
      Gout(144) = Gout(144) + G2033*Dq11
      Gout(145) = Gout(145) + G2100*Dq00
      Gout(146) = Gout(146) + G2101*Dq01
      Gout(147) = Gout(147) + G2102*Dq01
      Gout(148) = Gout(148) + G2103*Dq01
      Gout(149) = Gout(149) + g2110*Dq10
      Gout(150) = Gout(150) + G2111*Dq11
      Gout(151) = Gout(151) + G2112*Dq11
      Gout(152) = Gout(152) + G2113*Dq11
      Gout(153) = Gout(153) + g2120*Dq10
      Gout(154) = Gout(154) + g2121*Dq11
      Gout(155) = Gout(155) + G2122*Dq11
      Gout(156) = Gout(156) + G2123*Dq11
      Gout(157) = Gout(157) + g2130*Dq10
      Gout(158) = Gout(158) + g2131*Dq11
      Gout(159) = Gout(159) + g2132*Dq11
      Gout(160) = Gout(160) + G2133*Dq11
      Gout(161) = Gout(161) + G2200*Dq00
      Gout(162) = Gout(162) + G2201*Dq01
      Gout(163) = Gout(163) + G2202*Dq01
      Gout(164) = Gout(164) + G2203*Dq01
      Gout(165) = Gout(165) + g2210*Dq10
      Gout(166) = Gout(166) + G2211*Dq11
      Gout(167) = Gout(167) + G2212*Dq11
      Gout(168) = Gout(168) + G2213*Dq11
      Gout(169) = Gout(169) + g2220*Dq10
      Gout(170) = Gout(170) + g2221*Dq11
      Gout(171) = Gout(171) + G2222*Dq11
      Gout(172) = Gout(172) + G2223*Dq11
      Gout(173) = Gout(173) + g2230*Dq10
      Gout(174) = Gout(174) + g2231*Dq11
      Gout(175) = Gout(175) + g2232*Dq11
      Gout(176) = Gout(176) + G2233*Dq11
      Gout(177) = Gout(177) + G2300*Dq00
      Gout(178) = Gout(178) + G2301*Dq01
      Gout(179) = Gout(179) + G2302*Dq01
      Gout(180) = Gout(180) + G2303*Dq01
      Gout(181) = Gout(181) + g2310*Dq10
      Gout(182) = Gout(182) + G2311*Dq11
      Gout(183) = Gout(183) + G2312*Dq11
      Gout(184) = Gout(184) + G2313*Dq11
      Gout(185) = Gout(185) + g2320*Dq10
      Gout(186) = Gout(186) + g2321*Dq11
      Gout(187) = Gout(187) + G2322*Dq11
      Gout(188) = Gout(188) + G2323*Dq11
      Gout(189) = Gout(189) + g2330*Dq10
      Gout(190) = Gout(190) + g2331*Dq11
      Gout(191) = Gout(191) + g2332*Dq11
      Gout(192) = Gout(192) + G2333*Dq11
      Gout(193) = Gout(193) + G3000*Dq00
      Gout(194) = Gout(194) + G3001*Dq01
      Gout(195) = Gout(195) + G3002*Dq01
      Gout(196) = Gout(196) + G3003*Dq01
      Gout(197) = Gout(197) + g3010*Dq10
      Gout(198) = Gout(198) + G3011*Dq11
      Gout(199) = Gout(199) + G3012*Dq11
      Gout(200) = Gout(200) + G3013*Dq11
      Gout(201) = Gout(201) + g3020*Dq10
      Gout(202) = Gout(202) + g3021*Dq11
      Gout(203) = Gout(203) + G3022*Dq11
      Gout(204) = Gout(204) + G3023*Dq11
      Gout(205) = Gout(205) + g3030*Dq10
      Gout(206) = Gout(206) + g3031*Dq11
      Gout(207) = Gout(207) + g3032*Dq11
      Gout(208) = Gout(208) + G3033*Dq11
      Gout(209) = Gout(209) + G3100*Dq00
      Gout(210) = Gout(210) + G3101*Dq01
      Gout(211) = Gout(211) + G3102*Dq01
      Gout(212) = Gout(212) + G3103*Dq01
      Gout(213) = Gout(213) + g3110*Dq10
      Gout(214) = Gout(214) + G3111*Dq11
      Gout(215) = Gout(215) + G3112*Dq11
      Gout(216) = Gout(216) + G3113*Dq11
      Gout(217) = Gout(217) + g3120*Dq10
      Gout(218) = Gout(218) + g3121*Dq11
      Gout(219) = Gout(219) + G3122*Dq11
      Gout(220) = Gout(220) + G3123*Dq11
      Gout(221) = Gout(221) + g3130*Dq10
      Gout(222) = Gout(222) + g3131*Dq11
      Gout(223) = Gout(223) + g3132*Dq11
      Gout(224) = Gout(224) + G3133*Dq11
      Gout(225) = Gout(225) + G3200*Dq00
      Gout(226) = Gout(226) + G3201*Dq01
      Gout(227) = Gout(227) + G3202*Dq01
      Gout(228) = Gout(228) + G3203*Dq01
      Gout(229) = Gout(229) + g3210*Dq10
      Gout(230) = Gout(230) + G3211*Dq11
      Gout(231) = Gout(231) + G3212*Dq11
      Gout(232) = Gout(232) + G3213*Dq11
      Gout(233) = Gout(233) + g3220*Dq10
      Gout(234) = Gout(234) + g3221*Dq11
      Gout(235) = Gout(235) + G3222*Dq11
      Gout(236) = Gout(236) + G3223*Dq11
      Gout(237) = Gout(237) + g3230*Dq10
      Gout(238) = Gout(238) + g3231*Dq11
      Gout(239) = Gout(239) + g3232*Dq11
      Gout(240) = Gout(240) + G3233*Dq11
      Gout(241) = Gout(241) + G3300*Dq00
      Gout(242) = Gout(242) + G3301*Dq01
      Gout(243) = Gout(243) + G3302*Dq01
      Gout(244) = Gout(244) + G3303*Dq01
      Gout(245) = Gout(245) + g3310*Dq10
      Gout(246) = Gout(246) + G3311*Dq11
      Gout(247) = Gout(247) + G3312*Dq11
      Gout(248) = Gout(248) + G3313*Dq11
      Gout(249) = Gout(249) + g3320*Dq10
      Gout(250) = Gout(250) + g3321*Dq11
      Gout(251) = Gout(251) + G3322*Dq11
      Gout(252) = Gout(252) + G3323*Dq11
      Gout(253) = Gout(253) + g3330*Dq10
      Gout(254) = Gout(254) + g3331*Dq11
      Gout(255) = Gout(255) + g3332*Dq11
      Gout(256) = Gout(256) + G3333*Dq11
      return
C
      end

