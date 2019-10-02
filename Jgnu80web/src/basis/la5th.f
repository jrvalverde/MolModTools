
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 la5th"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "la5th.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "la5th.web"
      
      subroutine la5th(ITYPE,E,CS,CP,CD,IA,NCONT,NGAUSS,ISHT,ISHC)
      implicit none
      real*8 CD,CP,CS,E
      integer IA,ia1,ISHC,ISHT,ITYPE,NCONT,NGAUSS
      
      dimension E(*),CS(*),CP(*),CD(*),NGAUSS(*),ISHT(*),ISHC(*)
      
      ia1=IA-54
      if(ia1.EQ.2)then
      
      
      E(1)=0.1297
      E(2)=0.0823
      E(3)=0.0231
      E(4)=0.1804
      E(5)=0.0476
      E(6)=0.0192
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=2
      ISHT(2)=1
      ISHC(2)=1
      NGAUSS(1)=3
      NGAUSS(2)=3
      CS(1)=-0.9330926
      CS(2)=1.0001676
      CS(3)=0.7928327
      CP(4)=-0.1406549
      CP(5)=0.6039805
      CP(6)=0.5175636
      else
      NCONT=4
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      CS(1)=-3.4063657
      CS(2)=3.6512309
      CS(3)=1.0000000
      CP(4)=-0.2642537
      CP(5)=1.1347212
      CP(6)=1.0000000
      endif
      return
      elseif(ia1.EQ.3)then
      
      
      E(1)=0.1413
      E(2)=0.0792
      E(3)=0.0239
      E(4)=0.2125
      E(5)=0.0483
      E(6)=0.0179
      E(7)=0.4524
      E(8)=0.1602
      E(9)=0.0531
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=3
      ISHT(2)=1
      ISHC(2)=1
      ISHT(3)=2
      ISHC(3)=2
      NGAUSS(1)=3
      NGAUSS(2)=3
      NGAUSS(3)=3
      CS(1)=-0.8171303
      CS(2)=0.9661994
      CS(3)=0.7098560
      CP(4)=-0.1082407
      CP(5)=0.5996720
      CP(6)=0.5131390
      CD(7)=0.3682427
      CD(8)=0.5335878
      CD(9)=0.3051338
      else
      NCONT=6
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      NGAUSS(5)=2
      NGAUSS(6)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      ISHT(5)=2
      ISHC(5)=2
      ISHT(6)=2
      ISHC(6)=2
      CS(1)=-2.3877035
      CS(2)=2.8232923
      CS(3)=1.0000
      CP(4)=-0.1968810
      CP(5)=1.0907542
      CP(6)=1.0000
      CD(7)=0.4497269
      CD(8)=0.6516593
      CD(9)=1.0000
      endif
      return
      elseif(ia1.EQ.4)then
      
      
      call lnk1e
      
      
      call lnk1e
      
      
      call lnk1e
      
      
      call lnk1e
      
      
      call lnk1e
      
      
      call lnk1e
      
      
      call lnk1e
      
      
      call lnk1e
      
      
      call lnk1e
      
      
      call lnk1e
      
      
      call lnk1e
      
      
      call lnk1e
      
      
      call lnk1e
      
      
      call lnk1e
      elseif(ia1.EQ.5)then
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      elseif(ia1.EQ.6)then
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      elseif(ia1.EQ.7)then
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      elseif(ia1.EQ.8)then
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      elseif(ia1.EQ.9)then
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      elseif(ia1.EQ.10)then
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      elseif(ia1.EQ.11)then
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      elseif(ia1.EQ.12)then
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      elseif(ia1.EQ.13)then
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      elseif(ia1.EQ.14)then
      call lnk1e
      call lnk1e
      call lnk1e
      call lnk1e
      elseif(ia1.EQ.15)then
      call lnk1e
      call lnk1e
      call lnk1e
      elseif(ia1.EQ.16)then
      call lnk1e
      call lnk1e
      elseif(ia1.EQ.17)then
      call lnk1e
      elseif(ia1.EQ.18)then
      elseif(ia1.EQ.19)then
      
      
      E(1)=0.3084
      E(2)=0.1671
      E(3)=0.0482
      E(4)=0.4360
      E(5)=0.0840
      E(6)=0.0280
      E(7)=0.8948
      E(8)=0.2989
      E(9)=0.0935
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=3
      ISHT(2)=1
      ISHC(2)=1
      ISHT(3)=2
      ISHC(3)=2
      NGAUSS(1)=3
      NGAUSS(2)=3
      NGAUSS(3)=3
      CS(1)=-0.6940372
      CS(2)=0.7824080
      CS(3)=0.7770293
      CP(4)=-0.0737334
      CP(5)=0.5172072
      CP(6)=0.5910988
      CD(7)=0.3884774
      CD(8)=0.5224911
      CD(9)=0.3190890
      else
      NCONT=6
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      NGAUSS(5)=2
      NGAUSS(6)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      ISHT(5)=2
      ISHC(5)=2
      ISHT(6)=2
      ISHC(6)=2
      CS(1)=-2.4452356
      CS(2)=2.7565841
      CS(3)=1.0000000
      CP(4)=-0.1513014
      CP(5)=1.0613123
      CP(6)=1.0000000
      CD(7)=0.4746917
      CD(8)=0.6384469
      CD(9)=1.0000000
      endif
      return
      elseif(ia1.EQ.20)then
      
      
      E(1)=0.3313
      E(2)=0.1883
      E(3)=0.0518
      E(4)=0.4010
      E(5)=0.0900
      E(6)=0.0280
      E(7)=0.9519
      E(8)=0.3270
      E(9)=0.1054
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=3
      ISHT(2)=1
      ISHC(2)=1
      ISHT(3)=2
      ISHC(3)=2
      NGAUSS(1)=3
      NGAUSS(2)=3
      NGAUSS(3)=3
      CS(1)=-0.7652966
      CS(2)=0.8588369
      CS(3)=0.7713327
      CP(4)=-0.0679181
      CP(5)=0.4856300
      CP(6)=0.6318430
      CD(7)=0.4225700
      CD(8)=0.5153273
      CD(9)=0.2788919
      else
      NCONT=6
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      NGAUSS(5)=2
      NGAUSS(6)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      ISHT(5)=2
      ISHC(5)=2
      ISHT(6)=2
      ISHC(6)=2
      CS(1)=-2.6381161
      CS(2)=2.9605665
      CS(3)=1.0000000
      CP(4)=-0.1497499
      CP(5)=1.0707463
      CP(6)=1.0000000
      CD(7)=0.4999776
      CD(8)=0.6097264
      CD(9)=1.0000000
      endif
      return
      elseif(ia1.EQ.21)then
      
      
      E(1)=0.3314
      E(2)=0.2314
      E(3)=0.0566
      E(4)=0.4960
      E(5)=0.0890
      E(6)=0.0280
      E(7)=1.1160
      E(8)=0.4267
      E(9)=0.1378
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=3
      ISHT(2)=1
      ISHC(2)=1
      ISHT(3)=2
      ISHC(3)=2
      NGAUSS(1)=3
      NGAUSS(2)=3
      NGAUSS(3)=3
      CS(1)=-1.1074561
      CS(2)=1.1522357
      CS(3)=0.8217259
      CP(4)=-0.0539986
      CP(5)=0.4325121
      CP(6)=0.6707919
      CD(7)=0.3725292
      CD(8)=0.5014277
      CD(9)=0.3341287
      else
      NCONT=6
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      NGAUSS(5)=2
      NGAUSS(6)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      ISHT(5)=2
      ISHC(5)=2
      ISHT(6)=2
      ISHC(6)=2
      CS(1)=-4.4235772
      CS(2)=4.6024430
      CS(3)=1.0000000
      CP(4)=-0.1311370
      CP(5)=1.0503671
      CP(6)=1.0000000
      CD(7)=0.4644937
      CD(8)=0.6252127
      CD(9)=1.0000000
      endif
      return
      elseif(ia1.EQ.22)then
      
      
      E(1)=0.3553
      E(2)=0.2437
      E(3)=0.0583
      E(4)=0.5100
      E(5)=0.0980
      E(6)=0.0290
      E(7)=1.1830
      E(8)=0.4492
      E(9)=0.1463
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=3
      ISHT(2)=1
      ISHC(2)=1
      ISHT(3)=2
      ISHC(3)=2
      NGAUSS(1)=3
      NGAUSS(2)=3
      NGAUSS(3)=3
      CS(1)=-1.0298132
      CS(2)=1.0766338
      CS(3)=0.8249345
      CP(4)=-0.0600261
      CP(5)=0.4270969
      CP(6)=0.6860066
      CD(7)=0.4003032
      CD(8)=0.5004758
      CD(9)=0.3036704
      else
      NCONT=6
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      NGAUSS(5)=2
      NGAUSS(6)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      ISHT(5)=2
      ISHC(5)=2
      ISHT(6)=2
      ISHC(6)=2
      CS(1)=-4.1980722
      CS(2)=4.3889382
      CS(3)=1.0000000
      CP(4)=-0.1490279
      CP(5)=1.0603616
      CP(6)=1.0000000
      CD(7)=0.4852204
      CD(8)=0.6066428
      CD(9)=1.0000000
      endif
      return
      elseif(ia1.EQ.23)then
      
      
      E(1)=0.3857
      E(2)=0.2500
      E(3)=0.0598
      E(4)=0.5100
      E(5)=0.0980
      E(6)=0.0290
      E(7)=1.2400
      E(8)=0.4647
      E(9)=0.1529
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=3
      ISHT(2)=1
      ISHC(2)=1
      ISHT(3)=2
      ISHC(3)=2
      NGAUSS(1)=3
      NGAUSS(2)=3
      NGAUSS(3)=3
      CS(1)=-0.8851354
      CS(2)=0.9276371
      CS(3)=0.8300342
      CP(4)=-0.0488611
      CP(5)=0.4377874
      CP(6)=0.6736952
      CD(7)=0.4309934
      CD(8)=0.4977205
      CD(9)=0.2715636
      else
      NCONT=6
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      NGAUSS(5)=2
      NGAUSS(6)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      ISHT(5)=2
      ISHC(5)=2
      ISHT(6)=2
      ISHC(6)=2
      CS(1)=-3.6672892
      CS(2)=3.8433821
      CS(3)=1.0000000
      CP(4)=-0.1170669
      CP(5)=1.0489002
      CP(6)=1.0000000
      CD(7)=0.5081144
      CD(8)=0.5867815
      CD(9)=1.0000000
      endif
      return
      elseif(ia1.EQ.24)then
      
      
      E(1)=0.3755
      E(2)=0.2651
      E(3)=0.0580
      E(4)=0.6048
      E(5)=0.0996
      E(6)=0.0290
      E(7)=1.2430
      E(8)=0.4271
      E(9)=0.1370
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=3
      ISHT(2)=1
      ISHC(2)=1
      ISHT(3)=2
      ISHC(3)=2
      NGAUSS(1)=3
      NGAUSS(2)=3
      NGAUSS(3)=3
      CS(1)=-1.1780900
      CS(2)=1.2683001
      CS(3)=0.7895579
      CP(4)=-0.0447327
      CP(5)=0.4375802
      CP(6)=0.6730424
      CD(7)=0.5038163
      CD(8)=0.4979002
      CD(9)=0.1976129
      else
      NCONT=6
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      NGAUSS(5)=2
      NGAUSS(6)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      ISHT(5)=2
      ISHC(5)=2
      ISHT(6)=2
      ISHC(6)=2
      CS(1)=-4.3030775
      CS(2)=4.6325779
      CS(3)=1.0000000
      CP(4)=-0.1061438
      CP(5)=1.0383102
      CP(6)=1.0000000
      CD(7)=0.5587443
      CD(8)=0.5521832
      CD(9)=1.0000000
      endif
      return
      elseif(ia1.EQ.25)then
      
      
      E(1)=0.3992
      E(2)=0.2826
      E(3)=0.0598
      E(4)=0.6838
      E(5)=0.0977
      E(6)=0.0279
      E(7)=1.2870
      E(8)=0.4335
      E(9)=0.1396
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=3
      ISHT(2)=1
      ISHC(2)=1
      ISHT(3)=2
      ISHC(3)=2
      NGAUSS(1)=3
      NGAUSS(2)=3
      NGAUSS(3)=3
      CS(1)=-1.1140055
      CS(2)=1.1838178
      CS(3)=0.8145308
      CP(4)=-0.0391462
      CP(5)=0.4234659
      CP(6)=0.6856038
      CD(7)=0.5380450
      CD(8)=0.4869991
      CD(9)=0.1682663
      else
      NCONT=6
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      NGAUSS(5)=2
      NGAUSS(6)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      ISHT(5)=2
      ISHC(5)=2
      ISHT(6)=2
      ISHC(6)=2
      CS(1)=-4.4402904
      CS(2)=4.7185538
      CS(3)=1.0000000
      CP(4)=-0.0952078
      CP(5)=1.0299147
      CP(6)=1.0000000
      CD(7)=0.5848601
      CD(8)=0.5293727
      CD(9)=1.0000000
      endif
      return
      elseif(ia1.EQ.26)then
      
      
      E(1)=0.5275
      E(2)=0.2334
      E(3)=0.06861
      E(4)=0.6503
      E(5)=0.1368
      E(6)=0.04256
      E(7)=1.4840
      E(8)=0.5605
      E(9)=0.1923
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=3
      ISHT(2)=1
      ISHC(2)=1
      ISHT(3)=2
      ISHC(3)=2
      NGAUSS(1)=3
      NGAUSS(2)=3
      NGAUSS(3)=3
      CS(1)=-0.4911676
      CS(2)=0.6044070
      CS(3)=0.7690260
      CP(4)=-0.0672271
      CP(5)=0.4979023
      CP(6)=0.6187761
      CD(7)=0.4976772
      CD(8)=0.5010071
      CD(9)=0.1943363
      else
      NCONT=6
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      NGAUSS(5)=2
      NGAUSS(6)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      ISHT(5)=2
      ISHC(5)=2
      ISHT(6)=2
      ISHC(6)=2
      CS(1)=-1.7292589
      CS(2)=2.1279420
      CS(3)=1.0000000
      CP(4)=-0.1436715
      CP(5)=1.0640703
      CP(6)=1.0000000
      CD(7)=0.5630223
      CD(8)=0.5667893
      CD(9)=1.0000000
      endif
      return
      elseif(ia1.EQ.27)then
      
      E(1)=0.5355
      E(2)=0.3082
      E(3)=0.08183
      E(4)=0.7977
      E(5)=0.1498
      E(6)=0.04435
      E(7)=8.655
      E(8)=1.415
      E(9)=0.4442
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=3
      ISHT(2)=1
      ISHC(2)=1
      ISHT(3)=2
      ISHC(3)=2
      NGAUSS(1)=3
      NGAUSS(2)=3
      NGAUSS(3)=3
      CS(1)=-0.8008350
      CS(2)=0.9220130
      CS(3)=0.7480820
      CP(4)=-0.0690671
      CP(5)=0.5109710
      CP(6)=0.6114320
      CD(7)=0.0156029
      CD(8)=0.6226510
      CD(9)=0.4994810
      else
      NCONT=6
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      NGAUSS(5)=2
      NGAUSS(6)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      ISHT(5)=2
      ISHC(5)=2
      ISHT(6)=2
      ISHC(6)=2
      CS(1)=-0.8008350
      CS(2)=0.9220130
      CS(3)=1.0000000
      CP(4)=-0.0690671
      CP(5)=0.5109710
      CP(6)=1.0000000
      CD(7)=0.0156029
      CD(8)=0.6226510
      CD(9)=1.0000000
      endif
      return
      elseif(ia1.EQ.28)then
      
      
      E(1)=0.5135
      E(2)=0.3756
      E(3)=0.0944
      E(4)=0.8748
      E(5)=0.1843
      E(6)=0.0598
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=2
      ISHT(2)=1
      ISHC(2)=1
      NGAUSS(1)=3
      NGAUSS(2)=3
      CS(1)=-1.6466069
      CS(2)=1.8286900
      CS(3)=0.6759934
      CP(4)=-0.0951399
      CP(5)=0.5717806
      CP(6)=0.5510027
      else
      NCONT=4
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      CS(1)=-4.3675036
      CS(2)=4.8504656
      CS(3)=1.0000000
      CP(4)=-0.1793128
      CP(5)=1.0776505
      CP(6)=1.0000000
      endif
      return
      elseif(ia1.EQ.29)then
      goto 100
      else
      
      
      E(1)=0.1206
      E(2)=0.0393
      E(3)=0.0151
      E(4)=0.1458
      E(5)=0.0279
      E(6)=0.0113
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=2
      ISHT(2)=1
      ISHC(2)=1
      NGAUSS(1)=3
      NGAUSS(2)=3
      CS(1)=-0.4033569
      CS(2)=0.5743688
      CS(3)=0.6938567
      CP(4)=-0.0868370
      CP(5)=0.5511162
      CP(6)=0.5376642
      else
      NCONT=4
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      CS(1)=-1.1536494
      CS(2)=1.6427642
      CS(3)=1.0000000
      CP(4)=-0.1680556
      CP(5)=1.0665746
      CP(6)=1.0000000
      endif
      return
      endif
      
      
      E(1)=0.2457
      E(2)=0.1656
      E(3)=0.0424
      E(4)=0.3427
      E(5)=0.0804
      E(6)=0.0274
      E(7)=0.8226
      E(8)=0.2585
      E(9)=0.0762
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=3
      ISHT(2)=1
      ISHC(2)=1
      ISHT(3)=2
      ISHC(3)=2
      NGAUSS(1)=3
      NGAUSS(2)=3
      NGAUSS(3)=3
      CS(1)=-1.0978540
      CS(2)=1.1873377
      CS(3)=0.7754017
      CP(4)=-0.0839349
      CP(5)=0.5232311
      CP(6)=0.5909315
      CD(7)=0.3619963
      CD(8)=0.5364635
      CD(9)=0.3502947
      else
      NCONT=6
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      NGAUSS(5)=2
      NGAUSS(6)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      ISHT(5)=2
      ISHC(5)=2
      ISHT(6)=2
      ISHC(6)=2
      CS(1)=-3.8217151
      CS(2)=4.1332149
      CS(3)=1.0000000
      CP(4)=-0.1739178
      CP(5)=1.0841644
      CP(6)=1.0000000
      CD(7)=0.4519513
      CD(8)=0.6697730
      CD(9)=1.0000000
      endif
      return
      
      
100   E(1)=0.5744
      E(2)=0.3851
      E(3)=0.1050
      E(4)=0.9105
      E(5)=0.2194
      E(6)=0.0745
      ISHT(1)=0
      ISHC(1)=0
      if((ITYPE.EQ.0).OR.(ITYPE.EQ.2))then
      NCONT=2
      ISHT(2)=1
      ISHC(2)=1
      NGAUSS(1)=3
      NGAUSS(2)=3
      CS(1)=-1.3604224
      CS(2)=1.5862744
      CS(3)=0.6266092
      CP(4)=-0.1188660
      CP(5)=0.6064640
      CP(6)=0.5241060
      else
      NCONT=4
      NGAUSS(1)=2
      NGAUSS(2)=1
      NGAUSS(3)=2
      NGAUSS(4)=1
      ISHT(2)=0
      ISHC(2)=0
      ISHT(3)=1
      ISHC(3)=1
      ISHT(4)=1
      ISHC(4)=1
      CS(1)=-3.2278875
      CS(2)=3.7637689
      CS(3)=1.0000000
      CP(4)=-0.2164189
      CP(5)=1.1041867
      CP(6)=1.0000000
      endif
      
      return
      end
C* :1 * 
      
