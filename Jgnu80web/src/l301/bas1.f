
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 bas1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "bas1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "bas1.web"
      subroutine bas1(NVAL,COEF,EXPON,KF,KL,MAX,LSKP,TYPE,ICOR,IATNO)
      implicit none
      double precision COEF,EXPON
      integer IATNO,ICOR,KF,KL,LSKP,MAX,NVAL
      integer hell
      character*4 TYPE,nameb1
      dimension nameb1(18)
      dimension NVAL(40),COEF(40),EXPON(40),KF(5),KL(5)
      data nameb1/'H-1 ','He-1','Li-1','Be-1','B-1 ','C-1 ','N-1 ','O-1 
     &','F-1 ','Ne-1','Na-1','Mg-1','Al-1','Si-1','P-1 ','S-1 ','Cl-1','
     &Ar-1'/
      
      
      hell=IATNO-2
      if(IATNO.LE.2)then
      LSKP=1
      ICOR=0
      return
      else
      
      LSKP=0
      if(IATNO.LE.10)then
      MAX=2
      ICOR=2
      if(hell.EQ.2)then
      
      
      TYPE=nameb1(4)
      KF(1)=1
      KL(1)=3
      NVAL(1)=1
      EXPON(1)=1.5162337400D0
      COEF(1)=-.4519073400D0
      NVAL(2)=1
      EXPON(2)=.1382609600D0
      COEF(2)=-.0181583900D0
      NVAL(3)=2
      EXPON(3)=1.6554877900D0
      COEF(3)=.4457972740D0
      KF(2)=4
      KL(2)=7
      NVAL(4)=0
      EXPON(4)=.0716817300D0
      COEF(4)=.0755608500D0
      NVAL(5)=0
      EXPON(5)=.8325392100D0
      COEF(5)=1.5141929300D0
      NVAL(6)=0
      EXPON(6)=6.4678118100D0
      COEF(6)=.8281252050D0
      NVAL(7)=0
      EXPON(7)=115.6210947700D0
      COEF(7)=.4850227770D0
      KF(3)=8
      KL(3)=8
      NVAL(8)=2
      EXPON(8)=6.7253564300D0
      COEF(8)=-2.1453293000D0
      return
      elseif(hell.EQ.3)then
      
      
      TYPE=nameb1(5)
      KF(1)=1
      KL(1)=1
      NVAL(1)=1
      EXPON(1)=.2737781800D0
      COEF(1)=-.0377155900D0
      KF(2)=2
      KL(2)=4
      NVAL(2)=0
      EXPON(2)=.3916060600D0
      COEF(2)=.4151007300D0
      NVAL(3)=0
      EXPON(3)=37.3428362200D0
      COEF(3)=5.8802960000D0
      NVAL(4)=2
      EXPON(4)=4.0472670100D0
      COEF(4)=9.3032089000D0
      KF(3)=5
      KL(3)=5
      NVAL(5)=2
      EXPON(5)=9.7710463500D0
      COEF(5)=-4.2566973000D0
      return
      elseif(hell.EQ.4)then
      
      
      TYPE=nameb1(6)
      KF(1)=1
      KL(1)=3
      NVAL(1)=1
      EXPON(1)=80.0000000000D0
      COEF(1)=-1.6000000000D0
      NVAL(2)=1
      EXPON(2)=30.0000000000D0
      COEF(2)=-.4000000000D0
      NVAL(3)=2
      EXPON(3)=.5498204800D0
      COEF(3)=-.0399021000D0
      KF(2)=4
      KL(2)=6
      NVAL(4)=0
      EXPON(4)=.7374760400D0
      COEF(4)=.6381083160D0
      NVAL(5)=0
      EXPON(5)=135.2354831700D0
      COEF(5)=11.0091623000D0
      NVAL(6)=2
      EXPON(6)=8.5605568900D0
      COEF(6)=20.1379702000D0
      KF(3)=7
      KL(3)=8
      NVAL(7)=2
      EXPON(7)=10.6863587000D0
      COEF(7)=-3.2468428000D0
      NVAL(8)=2
      EXPON(8)=23.4979896800D0
      COEF(8)=.7850576520D0
      return
      elseif(hell.EQ.5)then
      
      
      TYPE=nameb1(7)
      KF(1)=1
      KL(1)=3
      NVAL(1)=1
      EXPON(1)=80.0000000000D0
      COEF(1)=-1.6000000000D0
      NVAL(2)=1
      EXPON(2)=30.0000000000D0
      COEF(2)=-.4000000000D0
      NVAL(3)=2
      EXPON(3)=.7627819100D0
      COEF(3)=-.0499000000D0
      KF(2)=4
      KL(2)=6
      NVAL(4)=0
      EXPON(4)=.8722099600D0
      COEF(4)=.5279147280D0
      NVAL(5)=0
      EXPON(5)=48.1738582600D0
      COEF(5)=3.5150873600D0
      NVAL(6)=2
      EXPON(6)=9.3860860000D0
      COEF(6)=20.1297916000D0
      KF(3)=7
      KL(3)=8
      NVAL(7)=2
      EXPON(7)=19.4884353200D0
      COEF(7)=-11.1777950000D0
      NVAL(8)=2
      EXPON(8)=28.3756285700D0
      COEF(8)=12.1412959000D0
      return
      elseif(hell.EQ.6)then
      
      
      TYPE=nameb1(8)
      KF(1)=1
      KL(1)=3
      NVAL(1)=1
      EXPON(1)=80.0000000000D0
      COEF(1)=-1.6000000000D0
      NVAL(2)=1
      EXPON(2)=30.0000000000D0
      COEF(2)=-.4000000000D0
      NVAL(3)=2
      EXPON(3)=1.0953760000D0
      COEF(3)=-.0662381400D0
      KF(2)=4
      KL(2)=6
      NVAL(4)=0
      EXPON(4)=.9212951800D0
      COEF(4)=.3955217910D0
      NVAL(5)=0
      EXPON(5)=28.6481971300D0
      COEF(5)=2.5165484300D0
      NVAL(6)=2
      EXPON(6)=9.3033499700D0
      COEF(6)=17.0447850000D0
      KF(3)=7
      KL(3)=8
      NVAL(7)=2
      EXPON(7)=52.3427019100D0
      COEF(7)=27.9779077000D0
      NVAL(8)=2
      EXPON(8)=30.7220233200D0
      COEF(8)=-16.4963050000D0
      return
      elseif(hell.EQ.7)then
      
      
      TYPE=nameb1(9)
      KF(1)=1
      KL(1)=3
      NVAL(1)=1
      EXPON(1)=16.6952471600D0
      COEF(1)=-.7237557200D0
      NVAL(2)=1
      EXPON(2)=26.2072475400D0
      COEF(2)=.5889028680D0
      NVAL(3)=2
      EXPON(3)=1.3382694400D0
      COEF(3)=-.0647352400D0
      KF(2)=4
      KL(2)=6
      NVAL(4)=0
      EXPON(4)=.8798847500D0
      COEF(4)=.2712468720D0
      NVAL(5)=0
      EXPON(5)=22.2158751200D0
      COEF(5)=2.2413837500D0
      NVAL(6)=2
      EXPON(6)=9.1357569000D0
      COEF(6)=14.6223952000D0
      KF(3)=7
      KL(3)=8
      NVAL(7)=2
      EXPON(7)=140.9425396700D0
      COEF(7)=-30.1942360000D0
      NVAL(8)=2
      EXPON(8)=28.6393853800D0
      COEF(8)=-1.8092553000D0
      return
      elseif(hell.EQ.8)then
      
      
      TYPE=nameb1(10)
      KF(1)=1
      KL(1)=3
      NVAL(1)=1
      EXPON(1)=80.0000000000D0
      COEF(1)=-1.6000000000D0
      NVAL(2)=1
      EXPON(2)=30.0000000000D0
      COEF(2)=-.4000000000D0
      NVAL(3)=2
      EXPON(3)=12.5730000000D0
      COEF(3)=-.2965810000D0
      KF(2)=4
      KL(2)=6
      NVAL(4)=0
      EXPON(4)=.3568039900D0
      COEF(4)=.0930679170D0
      NVAL(5)=0
      EXPON(5)=23.7299635100D0
      COEF(5)=2.5069327000D0
      NVAL(6)=2
      EXPON(6)=9.0707003500D0
      COEF(6)=15.3323560000D0
      KF(3)=7
      KL(3)=9
      NVAL(7)=2
      EXPON(7)=27.3880773100D0
      COEF(7)=-3.7806984000D0
      NVAL(8)=2
      EXPON(8)=47.6677704200D0
      COEF(8)=7.6776799000D0
      NVAL(9)=2
      EXPON(9)=1.1011358300D0
      COEF(9)=-.0454218400D0
      return
      else
      
      
      TYPE=nameb1(3)
      KF(1)=1
      KL(1)=5
      NVAL(1)=1
      EXPON(1)=11.0025700000D0
      COEF(1)=.4296258000D0
      NVAL(2)=1
      EXPON(2)=1.5082800000D0
      COEF(2)=-2.4255638000D0
      NVAL(3)=2
      EXPON(3)=4.3868200000D0
      COEF(3)=2.0262029000D0
      NVAL(4)=2
      EXPON(4)=1.7530400000D0
      COEF(4)=2.5342068000D0
      NVAL(5)=2
      EXPON(5)=.7158800000D0
      COEF(5)=-.0338851900D0
      KF(2)=6
      KL(2)=9
      NVAL(6)=0
      EXPON(6)=1.9087900000D0
      COEF(6)=2.5378443000D0
      NVAL(7)=2
      EXPON(7)=.9469900000D0
      COEF(7)=1.1097324000D0
      NVAL(8)=2
      EXPON(8)=.4904800000D0
      COEF(8)=.3527400600D0
      NVAL(9)=2
      EXPON(9)=.0982500000D0
      COEF(9)=.0089425040D0
      KF(3)=10
      KL(3)=12
      NVAL(10)=2
      EXPON(10)=1.0147400000D0
      COEF(10)=-.1260803800D0
      NVAL(11)=2
      EXPON(11)=13.1142500000D0
      COEF(11)=-.1900978000D0
      NVAL(12)=2
      EXPON(12)=3.0809600000D0
      COEF(12)=-.2393116700D0
      return
      endif
      elseif(IATNO.LE.18)then
      MAX=2
      hell=IATNO-10
      ICOR=10
      if(hell.EQ.2)then
      
      
      TYPE=nameb1(12)
      KF(1)=1
      KL(1)=4
      NVAL(1)=1
      EXPON(1)=80.0000000000D0
      COEF(1)=-10.0000000000D0
      NVAL(2)=2
      EXPON(2)=.0287112400D0
      COEF(2)=-.0002048500D0
      NVAL(3)=2
      EXPON(3)=.2374654700D0
      COEF(3)=-.0461171600D0
      NVAL(4)=2
      EXPON(4)=2.9241486800D0
      COEF(4)=-10.5476380000D0
      KF(2)=5
      KL(2)=7
      NVAL(5)=0
      EXPON(5)=12.4658688700D0
      COEF(5)=3.7012143800D0
      NVAL(6)=0
      EXPON(6)=.3804253800D0
      COEF(6)=2.0846822200D0
      NVAL(7)=2
      EXPON(7)=3.8823093400D0
      COEF(7)=23.9766477000D0
      KF(3)=8
      KL(3)=10
      NVAL(8)=0
      EXPON(8)=.0945871900D0
      COEF(8)=.2410658510D0
      NVAL(9)=0
      EXPON(9)=.6547565000D0
      COEF(9)=3.2941508700D0
      NVAL(10)=2
      EXPON(10)=1.7098043800D0
      COEF(10)=-1.7338291000D0
      return
      elseif(hell.EQ.3)then
      
      
      TYPE=nameb1(13)
      KF(1)=1
      KL(1)=4
      NVAL(1)=1
      EXPON(1)=80.0000000000D0
      COEF(1)=-10.0000000000D0
      NVAL(2)=2
      EXPON(2)=.0540488400D0
      COEF(2)=-.0008188500D0
      NVAL(3)=2
      EXPON(3)=.3648448500D0
      COEF(3)=-.1065650300D0
      NVAL(4)=2
      EXPON(4)=3.8715283600D0
      COEF(4)=-11.9055510000D0
      KF(2)=5
      KL(2)=7
      NVAL(5)=0
      EXPON(5)=4.8120283200D0
      COEF(5)=.0218357700D0
      NVAL(6)=0
      EXPON(6)=.4701583000D0
      COEF(6)=2.3607473400D0
      NVAL(7)=2
      EXPON(7)=4.7611563800D0
      COEF(7)=23.4483545000D0
      KF(3)=8
      KL(3)=10
      NVAL(8)=0
      EXPON(8)=.2533365700D0
      COEF(8)=.7625515900D0
      NVAL(9)=0
      EXPON(9)=1.0955973900D0
      COEF(9)=2.7018550000D0
      NVAL(10)=2
      EXPON(10)=2.4466492700D0
      COEF(10)=-1.0097106000D0
      return
      elseif(hell.EQ.4)then
      
      
      TYPE=nameb1(14)
      KF(1)=1
      KL(1)=4
      NVAL(1)=1
      EXPON(1)=80.0000000000D0
      COEF(1)=-10.0000000000D0
      NVAL(2)=2
      EXPON(2)=.0300972400D0
      COEF(2)=-.0007095300D0
      NVAL(3)=2
      EXPON(3)=.3671052600D0
      COEF(3)=-.1086911600D0
      NVAL(4)=2
      EXPON(4)=4.7282601400D0
      COEF(4)=-12.7698520000D0
      KF(2)=5
      KL(2)=7
      NVAL(5)=0
      EXPON(5)=12.4971005500D0
      COEF(5)=-.4363288800D0
      NVAL(6)=0
      EXPON(6)=.5824388600D0
      COEF(6)=2.5438424200D0
      NVAL(7)=2
      EXPON(7)=5.8034267200D0
      COEF(7)=25.8297281000D0
      KF(3)=8
      KL(3)=10
      NVAL(8)=0
      EXPON(8)=.4023513600D0
      COEF(8)=.9776615960D0
      NVAL(9)=0
      EXPON(9)=1.9927811800D0
      COEF(9)=2.6344253000D0
      NVAL(10)=2
      EXPON(10)=.8155899800D0
      COEF(10)=.3533430720D0
      return
      elseif(hell.EQ.5)then
      
      
      TYPE=nameb1(15)
      KF(1)=1
      KL(1)=4
      NVAL(1)=1
      EXPON(1)=80.0000000000D0
      COEF(1)=-10.0000000000D0
      NVAL(2)=2
      EXPON(2)=.0492305600D0
      COEF(2)=-.0013391400D0
      NVAL(3)=2
      EXPON(3)=.3583713400D0
      COEF(3)=-.1140119300D0
      NVAL(4)=2
      EXPON(4)=5.7165889900D0
      COEF(4)=-13.3278980000D0
      KF(2)=5
      KL(2)=7
      NVAL(5)=0
      EXPON(5)=29.6983446100D0
      COEF(5)=.7923510880D0
      NVAL(6)=0
      EXPON(6)=.5735822200D0
      COEF(6)=2.0765747000D0
      NVAL(7)=2
      EXPON(7)=5.9474486500D0
      COEF(7)=31.7016297000D0
      KF(3)=8
      KL(3)=10
      NVAL(8)=0
      EXPON(8)=.5186026300D0
      COEF(8)=1.4801516900D0
      NVAL(9)=0
      EXPON(9)=3.2996790400D0
      COEF(9)=1.8007616300D0
      NVAL(10)=2
      EXPON(10)=4.7044188800D0
      COEF(10)=4.4478016300D0
      return
      elseif(hell.EQ.6)then
      
      
      TYPE=nameb1(16)
      KF(1)=1
      KL(1)=4
      NVAL(1)=1
      EXPON(1)=80.0000000000D0
      COEF(1)=-10.0000000000D0
      NVAL(2)=2
      EXPON(2)=.1473719300D0
      COEF(2)=-.0171717800D0
      NVAL(3)=2
      EXPON(3)=.6222090100D0
      COEF(3)=-.2157090400D0
      NVAL(4)=2
      EXPON(4)=8.6218027400D0
      COEF(4)=-19.0724190000D0
      KF(2)=5
      KL(2)=7
      NVAL(5)=0
      EXPON(5)=11.9133230100D0
      COEF(5)=.5451531720D0
      NVAL(6)=0
      EXPON(6)=.6489482700D0
      COEF(6)=2.1015827700D0
      NVAL(7)=2
      EXPON(7)=6.8516829800D0
      COEF(7)=32.6658621000D0
      KF(3)=8
      KL(3)=10
      NVAL(8)=0
      EXPON(8)=.5450196800D0
      COEF(8)=1.3083926300D0
      NVAL(9)=0
      EXPON(9)=9.1670093000D0
      COEF(9)=2.5457156000D0
      NVAL(10)=2
      EXPON(10)=5.0239046700D0
      COEF(10)=11.3925378000D0
      return
      elseif(hell.EQ.7)then
      
      
      TYPE=nameb1(17)
      KF(1)=1
      KL(1)=4
      NVAL(1)=1
      EXPON(1)=80.0000000000D0
      COEF(1)=-10.0000000000D0
      NVAL(2)=2
      EXPON(2)=.1744764000D0
      COEF(2)=-.0288175100D0
      NVAL(3)=2
      EXPON(3)=.9638199700D0
      COEF(3)=-.3800511900D0
      NVAL(4)=2
      EXPON(4)=11.1757401200D0
      COEF(4)=-22.5446910000D0
      KF(2)=5
      KL(2)=7
      NVAL(5)=0
      EXPON(5)=.7656954600D0
      COEF(5)=2.2287997800D0
      NVAL(6)=0
      EXPON(6)=7.9743209900D0
      COEF(6)=.2722369840D0
      NVAL(7)=2
      EXPON(7)=7.9876938700D0
      COEF(7)=34.7388088000D0
      KF(3)=8
      KL(3)=10
      NVAL(8)=0
      EXPON(8)=15.3902864400D0
      COEF(8)=2.6881188700D0
      NVAL(9)=0
      EXPON(9)=.7104291900D0
      COEF(9)=1.5152706100D0
      NVAL(10)=2
      EXPON(10)=6.9469470400D0
      COEF(10)=16.8102350000D0
      return
      elseif(hell.EQ.8)then
      
      
      TYPE=nameb1(18)
      KF(1)=1
      KL(1)=4
      NVAL(1)=1
      EXPON(1)=80.0000000000D0
      COEF(1)=-18.0000000000D0
      NVAL(2)=2
      EXPON(2)=.3007608100D0
      COEF(2)=-.0621304000D0
      NVAL(3)=2
      EXPON(3)=1.8246375200D0
      COEF(3)=-.7179810000D0
      NVAL(4)=2
      EXPON(4)=12.2691275400D0
      COEF(4)=-19.5629200000D0
      KF(2)=5
      KL(2)=7
      NVAL(5)=0
      EXPON(5)=.9870393700D0
      COEF(5)=2.4770597000D0
      NVAL(6)=0
      EXPON(6)=9.0293201000D0
      COEF(6)=.6509001800D0
      NVAL(7)=2
      EXPON(7)=9.0424492400D0
      COEF(7)=33.1199200000D0
      KF(3)=8
      KL(3)=10
      NVAL(8)=0
      EXPON(8)=.9711918300D0
      COEF(8)=1.7676079000D0
      NVAL(9)=0
      EXPON(9)=27.3931484800D0
      COEF(9)=3.2221497000D0
      NVAL(10)=2
      EXPON(10)=8.8158609800D0
      COEF(10)=20.8523430000D0
      return
      else
      
      
      TYPE=nameb1(11)
      KF(1)=1
      KL(1)=5
      NVAL(1)=2
      EXPON(1)=.2279900000D0
      COEF(1)=-.0219963700D0
      NVAL(2)=2
      EXPON(2)=1.7972200000D0
      COEF(2)=10.2369140000D0
      NVAL(3)=2
      EXPON(3)=4.2008500000D0
      COEF(3)=9.3093297000D0
      NVAL(4)=1
      EXPON(4)=1.3858700000D0
      COEF(4)=-11.0951430000D0
      NVAL(5)=1
      EXPON(5)=12.2244800000D0
      COEF(5)=1.1003870000D0
      KF(2)=6
      KL(2)=9
      NVAL(6)=2
      EXPON(6)=.0762111000D0
      COEF(6)=.0066726360D0
      NVAL(7)=2
      EXPON(7)=.6104800000D0
      COEF(7)=1.5541298000D0
      NVAL(8)=2
      EXPON(8)=28.4199620000D0
      COEF(8)=29.4418390000D0
      NVAL(9)=0
      EXPON(9)=1.3939702000D0
      COEF(9)=5.8539107000D0
      KF(3)=10
      KL(3)=14
      NVAL(10)=2
      EXPON(10)=.3030699900D0
      COEF(10)=.3581617900D0
      NVAL(11)=2
      EXPON(11)=1.5165800000D0
      COEF(11)=16.1439150000D0
      NVAL(12)=2
      EXPON(12)=2.8630499000D0
      COEF(12)=87.5678670000D0
      NVAL(13)=0
      EXPON(13)=1.7704300000D0
      COEF(13)=-46.9069080000D0
      NVAL(14)=0
      EXPON(14)=3.9263699000D0
      COEF(14)=51.5653140000D0
      return
      endif
      elseif(IATNO.LE.30)then
      if(hell.EQ.2)then
      
      stop
      elseif(hell.EQ.3)then
      
      stop
      elseif(hell.EQ.4)then
      
      stop
      elseif(hell.EQ.5)then
      
      stop
      elseif(hell.EQ.6)then
      
      stop
      elseif(hell.EQ.7)then
      
      stop
      elseif(hell.EQ.8)then
      
      stop
      elseif(hell.EQ.9)then
      
      stop
      elseif(hell.EQ.10)then
      
      stop
      elseif(hell.EQ.11)then
      
      stop
      elseif(hell.EQ.12)then
      
      stop
      else
      
      stop
      endif
      
      elseif(hell.EQ.2)then
      
      stop
      elseif(hell.EQ.3)then
      
      stop
      elseif(hell.EQ.4)then
      
      stop
      elseif(hell.EQ.5)then
      
      stop
      elseif(hell.NE.6)then
      
      stop
      endif
      endif
      
      stop
      
      end
C* :1 * 
      
