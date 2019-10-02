
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 twod4c"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "twod4c.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "twod4c.web"
      subroutine twod4c(CCP,VALIP)
      implicit none
      double precision CCP,G,Vali2p,Vali3p,VALIP
      integer Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lpqmax,Lqmax
      dimension CCP(*),VALIP(*)
      common/int2d/G(13),Vali2p(49),Vali3p(112)
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      
      
      if(Lamax.EQ.1)goto 8400
      if(Lamax.EQ.2)goto 5600
      if(Lamax.EQ.3)goto 2800
      
      if(Lbmax.EQ.1)goto 2100
      if(Lbmax.EQ.2)goto 1400
      if(Lbmax.EQ.3)goto 700
      
      if(Lcmax.EQ.1)goto 500
      if(Lcmax.EQ.2)goto 300
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(253)=CCP(43)*Vali3p(13)+CCP(44)*Vali3p(29)+CCP(45)*Vali3p(45
     &)+CCP(46)*Vali3p(61)+CCP(47)*Vali3p(77)+CCP(48)*Vali3p(93)+Vali3p(
     &109)
      goto 100
      elseif(Ldmax.EQ.2)then
      goto 50
      elseif(Ldmax.NE.3)then
      
      VALIP(256)=CCP(43)*Vali3p(16)+CCP(44)*Vali3p(32)+CCP(45)*Vali3p(48
     &)+CCP(46)*Vali3p(64)+CCP(47)*Vali3p(80)+CCP(48)*Vali3p(96)+Vali3p(
     &112)
      endif
      VALIP(255)=CCP(43)*Vali3p(15)+CCP(44)*Vali3p(31)+CCP(45)*Vali3p(47
     &)+CCP(46)*Vali3p(63)+CCP(47)*Vali3p(79)+CCP(48)*Vali3p(95)+Vali3p(
     &111)
50    VALIP(254)=CCP(43)*Vali3p(14)+CCP(44)*Vali3p(30)+CCP(45)*Vali3p(46
     &)+CCP(46)*Vali3p(62)+CCP(47)*Vali3p(78)+CCP(48)*Vali3p(94)+Vali3p(
     &110)
      VALIP(253)=CCP(43)*Vali3p(13)+CCP(44)*Vali3p(29)+CCP(45)*Vali3p(45
     &)+CCP(46)*Vali3p(61)+CCP(47)*Vali3p(77)+CCP(48)*Vali3p(93)+Vali3p(
     &109)
      endif
100   if(Ldmax.EQ.1)then
      VALIP(249)=CCP(43)*Vali3p(9)+CCP(44)*Vali3p(25)+CCP(45)*Vali3p(41)
     &+CCP(46)*Vali3p(57)+CCP(47)*Vali3p(73)+CCP(48)*Vali3p(89)+Vali3p(1
     &05)
      goto 300
      elseif(Ldmax.EQ.2)then
      goto 200
      elseif(Ldmax.NE.3)then
      
      VALIP(252)=CCP(43)*Vali3p(12)+CCP(44)*Vali3p(28)+CCP(45)*Vali3p(44
     &)+CCP(46)*Vali3p(60)+CCP(47)*Vali3p(76)+CCP(48)*Vali3p(92)+Vali3p(
     &108)
      endif
      VALIP(251)=CCP(43)*Vali3p(11)+CCP(44)*Vali3p(27)+CCP(45)*Vali3p(43
     &)+CCP(46)*Vali3p(59)+CCP(47)*Vali3p(75)+CCP(48)*Vali3p(91)+Vali3p(
     &107)
200   VALIP(250)=CCP(43)*Vali3p(10)+CCP(44)*Vali3p(26)+CCP(45)*Vali3p(42
     &)+CCP(46)*Vali3p(58)+CCP(47)*Vali3p(74)+CCP(48)*Vali3p(90)+Vali3p(
     &106)
      VALIP(249)=CCP(43)*Vali3p(9)+CCP(44)*Vali3p(25)+CCP(45)*Vali3p(41)
     &+CCP(46)*Vali3p(57)+CCP(47)*Vali3p(73)+CCP(48)*Vali3p(89)+Vali3p(1
     &05)
300   if(Ldmax.EQ.1)then
      VALIP(245)=CCP(43)*Vali3p(5)+CCP(44)*Vali3p(21)+CCP(45)*Vali3p(37)
     &+CCP(46)*Vali3p(53)+CCP(47)*Vali3p(69)+CCP(48)*Vali3p(85)+Vali3p(1
     &01)
      goto 500
      elseif(Ldmax.EQ.2)then
      goto 400
      elseif(Ldmax.NE.3)then
      
      VALIP(248)=CCP(43)*Vali3p(8)+CCP(44)*Vali3p(24)+CCP(45)*Vali3p(40)
     &+CCP(46)*Vali3p(56)+CCP(47)*Vali3p(72)+CCP(48)*Vali3p(88)+Vali3p(1
     &04)
      endif
      VALIP(247)=CCP(43)*Vali3p(7)+CCP(44)*Vali3p(23)+CCP(45)*Vali3p(39)
     &+CCP(46)*Vali3p(55)+CCP(47)*Vali3p(71)+CCP(48)*Vali3p(87)+Vali3p(1
     &03)
400   VALIP(246)=CCP(43)*Vali3p(6)+CCP(44)*Vali3p(22)+CCP(45)*Vali3p(38)
     &+CCP(46)*Vali3p(54)+CCP(47)*Vali3p(70)+CCP(48)*Vali3p(86)+Vali3p(1
     &02)
      VALIP(245)=CCP(43)*Vali3p(5)+CCP(44)*Vali3p(21)+CCP(45)*Vali3p(37)
     &+CCP(46)*Vali3p(53)+CCP(47)*Vali3p(69)+CCP(48)*Vali3p(85)+Vali3p(1
     &01)
500   if(Ldmax.EQ.1)then
      VALIP(241)=CCP(43)*Vali3p(1)+CCP(44)*Vali3p(17)+CCP(45)*Vali3p(33)
     &+CCP(46)*Vali3p(49)+CCP(47)*Vali3p(65)+CCP(48)*Vali3p(81)+Vali3p(9
     &7)
      goto 700
      elseif(Ldmax.EQ.2)then
      goto 600
      elseif(Ldmax.NE.3)then
      
      VALIP(244)=CCP(43)*Vali3p(4)+CCP(44)*Vali3p(20)+CCP(45)*Vali3p(36)
     &+CCP(46)*Vali3p(52)+CCP(47)*Vali3p(68)+CCP(48)*Vali3p(84)+Vali3p(1
     &00)
      endif
      VALIP(243)=CCP(43)*Vali3p(3)+CCP(44)*Vali3p(19)+CCP(45)*Vali3p(35)
     &+CCP(46)*Vali3p(51)+CCP(47)*Vali3p(67)+CCP(48)*Vali3p(83)+Vali3p(9
     &9)
600   VALIP(242)=CCP(43)*Vali3p(2)+CCP(44)*Vali3p(18)+CCP(45)*Vali3p(34)
     &+CCP(46)*Vali3p(50)+CCP(47)*Vali3p(66)+CCP(48)*Vali3p(82)+Vali3p(9
     &8)
      VALIP(241)=CCP(43)*Vali3p(1)+CCP(44)*Vali3p(17)+CCP(45)*Vali3p(33)
     &+CCP(46)*Vali3p(49)+CCP(47)*Vali3p(65)+CCP(48)*Vali3p(81)+Vali3p(9
     &7)
700   if(Lcmax.EQ.1)goto 1200
      if(Lcmax.EQ.2)goto 1000
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(237)=CCP(38)*Vali3p(13)+CCP(39)*Vali3p(29)+CCP(40)*Vali3p(45
     &)+CCP(41)*Vali3p(61)+CCP(42)*Vali3p(77)+Vali3p(93)
      goto 800
      elseif(Ldmax.EQ.2)then
      goto 750
      elseif(Ldmax.NE.3)then
      
      VALIP(240)=CCP(38)*Vali3p(16)+CCP(39)*Vali3p(32)+CCP(40)*Vali3p(48
     &)+CCP(41)*Vali3p(64)+CCP(42)*Vali3p(80)+Vali3p(96)
      endif
      VALIP(239)=CCP(38)*Vali3p(15)+CCP(39)*Vali3p(31)+CCP(40)*Vali3p(47
     &)+CCP(41)*Vali3p(63)+CCP(42)*Vali3p(79)+Vali3p(95)
750   VALIP(238)=CCP(38)*Vali3p(14)+CCP(39)*Vali3p(30)+CCP(40)*Vali3p(46
     &)+CCP(41)*Vali3p(62)+CCP(42)*Vali3p(78)+Vali3p(94)
      VALIP(237)=CCP(38)*Vali3p(13)+CCP(39)*Vali3p(29)+CCP(40)*Vali3p(45
     &)+CCP(41)*Vali3p(61)+CCP(42)*Vali3p(77)+Vali3p(93)
      endif
800   if(Ldmax.EQ.1)then
      VALIP(233)=CCP(38)*Vali3p(9)+CCP(39)*Vali3p(25)+CCP(40)*Vali3p(41)
     &+CCP(41)*Vali3p(57)+CCP(42)*Vali3p(73)+Vali3p(89)
      goto 1000
      elseif(Ldmax.EQ.2)then
      goto 900
      elseif(Ldmax.NE.3)then
      
      VALIP(236)=CCP(38)*Vali3p(12)+CCP(39)*Vali3p(28)+CCP(40)*Vali3p(44
     &)+CCP(41)*Vali3p(60)+CCP(42)*Vali3p(76)+Vali3p(92)
      endif
      VALIP(235)=CCP(38)*Vali3p(11)+CCP(39)*Vali3p(27)+CCP(40)*Vali3p(43
     &)+CCP(41)*Vali3p(59)+CCP(42)*Vali3p(75)+Vali3p(91)
900   VALIP(234)=CCP(38)*Vali3p(10)+CCP(39)*Vali3p(26)+CCP(40)*Vali3p(42
     &)+CCP(41)*Vali3p(58)+CCP(42)*Vali3p(74)+Vali3p(90)
      VALIP(233)=CCP(38)*Vali3p(9)+CCP(39)*Vali3p(25)+CCP(40)*Vali3p(41)
     &+CCP(41)*Vali3p(57)+CCP(42)*Vali3p(73)+Vali3p(89)
1000  if(Ldmax.EQ.1)then
      VALIP(229)=CCP(38)*Vali3p(5)+CCP(39)*Vali3p(21)+CCP(40)*Vali3p(37)
     &+CCP(41)*Vali3p(53)+CCP(42)*Vali3p(69)+Vali3p(85)
      goto 1200
      elseif(Ldmax.EQ.2)then
      goto 1100
      elseif(Ldmax.NE.3)then
      
      VALIP(232)=CCP(38)*Vali3p(8)+CCP(39)*Vali3p(24)+CCP(40)*Vali3p(40)
     &+CCP(41)*Vali3p(56)+CCP(42)*Vali3p(72)+Vali3p(88)
      endif
      VALIP(231)=CCP(38)*Vali3p(7)+CCP(39)*Vali3p(23)+CCP(40)*Vali3p(39)
     &+CCP(41)*Vali3p(55)+CCP(42)*Vali3p(71)+Vali3p(87)
1100  VALIP(230)=CCP(38)*Vali3p(6)+CCP(39)*Vali3p(22)+CCP(40)*Vali3p(38)
     &+CCP(41)*Vali3p(54)+CCP(42)*Vali3p(70)+Vali3p(86)
      VALIP(229)=CCP(38)*Vali3p(5)+CCP(39)*Vali3p(21)+CCP(40)*Vali3p(37)
     &+CCP(41)*Vali3p(53)+CCP(42)*Vali3p(69)+Vali3p(85)
1200  if(Ldmax.EQ.1)then
      VALIP(225)=CCP(38)*Vali3p(1)+CCP(39)*Vali3p(17)+CCP(40)*Vali3p(33)
     &+CCP(41)*Vali3p(49)+CCP(42)*Vali3p(65)+Vali3p(81)
      goto 1400
      elseif(Ldmax.EQ.2)then
      goto 1300
      elseif(Ldmax.NE.3)then
      
      VALIP(228)=CCP(38)*Vali3p(4)+CCP(39)*Vali3p(20)+CCP(40)*Vali3p(36)
     &+CCP(41)*Vali3p(52)+CCP(42)*Vali3p(68)+Vali3p(84)
      endif
      VALIP(227)=CCP(38)*Vali3p(3)+CCP(39)*Vali3p(19)+CCP(40)*Vali3p(35)
     &+CCP(41)*Vali3p(51)+CCP(42)*Vali3p(67)+Vali3p(83)
1300  VALIP(226)=CCP(38)*Vali3p(2)+CCP(39)*Vali3p(18)+CCP(40)*Vali3p(34)
     &+CCP(41)*Vali3p(50)+CCP(42)*Vali3p(66)+Vali3p(82)
      VALIP(225)=CCP(38)*Vali3p(1)+CCP(39)*Vali3p(17)+CCP(40)*Vali3p(33)
     &+CCP(41)*Vali3p(49)+CCP(42)*Vali3p(65)+Vali3p(81)
1400  if(Lcmax.EQ.1)goto 1900
      if(Lcmax.EQ.2)goto 1700
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(221)=CCP(34)*Vali3p(13)+CCP(35)*Vali3p(29)+CCP(36)*Vali3p(45
     &)+CCP(37)*Vali3p(61)+Vali3p(77)
      goto 1500
      elseif(Ldmax.EQ.2)then
      goto 1450
      elseif(Ldmax.NE.3)then
      
      VALIP(224)=CCP(34)*Vali3p(16)+CCP(35)*Vali3p(32)+CCP(36)*Vali3p(48
     &)+CCP(37)*Vali3p(64)+Vali3p(80)
      endif
      VALIP(223)=CCP(34)*Vali3p(15)+CCP(35)*Vali3p(31)+CCP(36)*Vali3p(47
     &)+CCP(37)*Vali3p(63)+Vali3p(79)
1450  VALIP(222)=CCP(34)*Vali3p(14)+CCP(35)*Vali3p(30)+CCP(36)*Vali3p(46
     &)+CCP(37)*Vali3p(62)+Vali3p(78)
      VALIP(221)=CCP(34)*Vali3p(13)+CCP(35)*Vali3p(29)+CCP(36)*Vali3p(45
     &)+CCP(37)*Vali3p(61)+Vali3p(77)
      endif
1500  if(Ldmax.EQ.1)then
      VALIP(217)=CCP(34)*Vali3p(9)+CCP(35)*Vali3p(25)+CCP(36)*Vali3p(41)
     &+CCP(37)*Vali3p(57)+Vali3p(73)
      goto 1700
      elseif(Ldmax.EQ.2)then
      goto 1600
      elseif(Ldmax.NE.3)then
      
      VALIP(220)=CCP(34)*Vali3p(12)+CCP(35)*Vali3p(28)+CCP(36)*Vali3p(44
     &)+CCP(37)*Vali3p(60)+Vali3p(76)
      endif
      VALIP(219)=CCP(34)*Vali3p(11)+CCP(35)*Vali3p(27)+CCP(36)*Vali3p(43
     &)+CCP(37)*Vali3p(59)+Vali3p(75)
1600  VALIP(218)=CCP(34)*Vali3p(10)+CCP(35)*Vali3p(26)+CCP(36)*Vali3p(42
     &)+CCP(37)*Vali3p(58)+Vali3p(74)
      VALIP(217)=CCP(34)*Vali3p(9)+CCP(35)*Vali3p(25)+CCP(36)*Vali3p(41)
     &+CCP(37)*Vali3p(57)+Vali3p(73)
1700  if(Ldmax.EQ.1)then
      VALIP(213)=CCP(34)*Vali3p(5)+CCP(35)*Vali3p(21)+CCP(36)*Vali3p(37)
     &+CCP(37)*Vali3p(53)+Vali3p(69)
      goto 1900
      elseif(Ldmax.EQ.2)then
      goto 1800
      elseif(Ldmax.NE.3)then
      
      VALIP(216)=CCP(34)*Vali3p(8)+CCP(35)*Vali3p(24)+CCP(36)*Vali3p(40)
     &+CCP(37)*Vali3p(56)+Vali3p(72)
      endif
      VALIP(215)=CCP(34)*Vali3p(7)+CCP(35)*Vali3p(23)+CCP(36)*Vali3p(39)
     &+CCP(37)*Vali3p(55)+Vali3p(71)
1800  VALIP(214)=CCP(34)*Vali3p(6)+CCP(35)*Vali3p(22)+CCP(36)*Vali3p(38)
     &+CCP(37)*Vali3p(54)+Vali3p(70)
      VALIP(213)=CCP(34)*Vali3p(5)+CCP(35)*Vali3p(21)+CCP(36)*Vali3p(37)
     &+CCP(37)*Vali3p(53)+Vali3p(69)
1900  if(Ldmax.EQ.1)then
      VALIP(209)=CCP(34)*Vali3p(1)+CCP(35)*Vali3p(17)+CCP(36)*Vali3p(33)
     &+CCP(37)*Vali3p(49)+Vali3p(65)
      goto 2100
      elseif(Ldmax.EQ.2)then
      goto 2000
      elseif(Ldmax.NE.3)then
      
      VALIP(212)=CCP(34)*Vali3p(4)+CCP(35)*Vali3p(20)+CCP(36)*Vali3p(36)
     &+CCP(37)*Vali3p(52)+Vali3p(68)
      endif
      VALIP(211)=CCP(34)*Vali3p(3)+CCP(35)*Vali3p(19)+CCP(36)*Vali3p(35)
     &+CCP(37)*Vali3p(51)+Vali3p(67)
2000  VALIP(210)=CCP(34)*Vali3p(2)+CCP(35)*Vali3p(18)+CCP(36)*Vali3p(34)
     &+CCP(37)*Vali3p(50)+Vali3p(66)
      VALIP(209)=CCP(34)*Vali3p(1)+CCP(35)*Vali3p(17)+CCP(36)*Vali3p(33)
     &+CCP(37)*Vali3p(49)+Vali3p(65)
2100  if(Lcmax.EQ.1)goto 2600
      if(Lcmax.EQ.2)goto 2400
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(205)=CCP(31)*Vali3p(13)+CCP(32)*Vali3p(29)+CCP(33)*Vali3p(45
     &)+Vali3p(61)
      goto 2200
      elseif(Ldmax.EQ.2)then
      goto 2150
      elseif(Ldmax.NE.3)then
      
      VALIP(208)=CCP(31)*Vali3p(16)+CCP(32)*Vali3p(32)+CCP(33)*Vali3p(48
     &)+Vali3p(64)
      endif
      VALIP(207)=CCP(31)*Vali3p(15)+CCP(32)*Vali3p(31)+CCP(33)*Vali3p(47
     &)+Vali3p(63)
2150  VALIP(206)=CCP(31)*Vali3p(14)+CCP(32)*Vali3p(30)+CCP(33)*Vali3p(46
     &)+Vali3p(62)
      VALIP(205)=CCP(31)*Vali3p(13)+CCP(32)*Vali3p(29)+CCP(33)*Vali3p(45
     &)+Vali3p(61)
      endif
2200  if(Ldmax.EQ.1)then
      VALIP(201)=CCP(31)*Vali3p(9)+CCP(32)*Vali3p(25)+CCP(33)*Vali3p(41)
     &+Vali3p(57)
      goto 2400
      elseif(Ldmax.EQ.2)then
      goto 2300
      elseif(Ldmax.NE.3)then
      
      VALIP(204)=CCP(31)*Vali3p(12)+CCP(32)*Vali3p(28)+CCP(33)*Vali3p(44
     &)+Vali3p(60)
      endif
      VALIP(203)=CCP(31)*Vali3p(11)+CCP(32)*Vali3p(27)+CCP(33)*Vali3p(43
     &)+Vali3p(59)
2300  VALIP(202)=CCP(31)*Vali3p(10)+CCP(32)*Vali3p(26)+CCP(33)*Vali3p(42
     &)+Vali3p(58)
      VALIP(201)=CCP(31)*Vali3p(9)+CCP(32)*Vali3p(25)+CCP(33)*Vali3p(41)
     &+Vali3p(57)
2400  if(Ldmax.EQ.1)then
      VALIP(197)=CCP(31)*Vali3p(5)+CCP(32)*Vali3p(21)+CCP(33)*Vali3p(37)
     &+Vali3p(53)
      goto 2600
      elseif(Ldmax.EQ.2)then
      goto 2500
      elseif(Ldmax.NE.3)then
      
      VALIP(200)=CCP(31)*Vali3p(8)+CCP(32)*Vali3p(24)+CCP(33)*Vali3p(40)
     &+Vali3p(56)
      endif
      VALIP(199)=CCP(31)*Vali3p(7)+CCP(32)*Vali3p(23)+CCP(33)*Vali3p(39)
     &+Vali3p(55)
2500  VALIP(198)=CCP(31)*Vali3p(6)+CCP(32)*Vali3p(22)+CCP(33)*Vali3p(38)
     &+Vali3p(54)
      VALIP(197)=CCP(31)*Vali3p(5)+CCP(32)*Vali3p(21)+CCP(33)*Vali3p(37)
     &+Vali3p(53)
2600  if(Ldmax.EQ.1)then
      VALIP(193)=CCP(31)*Vali3p(1)+CCP(32)*Vali3p(17)+CCP(33)*Vali3p(33)
     &+Vali3p(49)
      goto 2800
      elseif(Ldmax.EQ.2)then
      goto 2700
      elseif(Ldmax.NE.3)then
      
      VALIP(196)=CCP(31)*Vali3p(4)+CCP(32)*Vali3p(20)+CCP(33)*Vali3p(36)
     &+Vali3p(52)
      endif
      VALIP(195)=CCP(31)*Vali3p(3)+CCP(32)*Vali3p(19)+CCP(33)*Vali3p(35)
     &+Vali3p(51)
2700  VALIP(194)=CCP(31)*Vali3p(2)+CCP(32)*Vali3p(18)+CCP(33)*Vali3p(34)
     &+Vali3p(50)
      VALIP(193)=CCP(31)*Vali3p(1)+CCP(32)*Vali3p(17)+CCP(33)*Vali3p(33)
     &+Vali3p(49)
2800  if(Lbmax.EQ.1)goto 4900
      if(Lbmax.EQ.2)goto 4200
      if(Lbmax.EQ.3)goto 3500
      
      if(Lcmax.EQ.1)goto 3300
      if(Lcmax.EQ.2)goto 3100
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(189)=CCP(26)*Vali3p(13)+CCP(27)*Vali3p(29)+CCP(28)*Vali3p(45
     &)+CCP(29)*Vali3p(61)+CCP(30)*Vali3p(77)+Vali3p(93)
      goto 2900
      elseif(Ldmax.EQ.2)then
      goto 2850
      elseif(Ldmax.NE.3)then
      
      VALIP(192)=CCP(26)*Vali3p(16)+CCP(27)*Vali3p(32)+CCP(28)*Vali3p(48
     &)+CCP(29)*Vali3p(64)+CCP(30)*Vali3p(80)+Vali3p(96)
      endif
      VALIP(191)=CCP(26)*Vali3p(15)+CCP(27)*Vali3p(31)+CCP(28)*Vali3p(47
     &)+CCP(29)*Vali3p(63)+CCP(30)*Vali3p(79)+Vali3p(95)
2850  VALIP(190)=CCP(26)*Vali3p(14)+CCP(27)*Vali3p(30)+CCP(28)*Vali3p(46
     &)+CCP(29)*Vali3p(62)+CCP(30)*Vali3p(78)+Vali3p(94)
      VALIP(189)=CCP(26)*Vali3p(13)+CCP(27)*Vali3p(29)+CCP(28)*Vali3p(45
     &)+CCP(29)*Vali3p(61)+CCP(30)*Vali3p(77)+Vali3p(93)
      endif
2900  if(Ldmax.EQ.1)then
      VALIP(185)=CCP(26)*Vali3p(9)+CCP(27)*Vali3p(25)+CCP(28)*Vali3p(41)
     &+CCP(29)*Vali3p(57)+CCP(30)*Vali3p(73)+Vali3p(89)
      goto 3100
      elseif(Ldmax.EQ.2)then
      goto 3000
      elseif(Ldmax.NE.3)then
      
      VALIP(188)=CCP(26)*Vali3p(12)+CCP(27)*Vali3p(28)+CCP(28)*Vali3p(44
     &)+CCP(29)*Vali3p(60)+CCP(30)*Vali3p(76)+Vali3p(92)
      endif
      VALIP(187)=CCP(26)*Vali3p(11)+CCP(27)*Vali3p(27)+CCP(28)*Vali3p(43
     &)+CCP(29)*Vali3p(59)+CCP(30)*Vali3p(75)+Vali3p(91)
3000  VALIP(186)=CCP(26)*Vali3p(10)+CCP(27)*Vali3p(26)+CCP(28)*Vali3p(42
     &)+CCP(29)*Vali3p(58)+CCP(30)*Vali3p(74)+Vali3p(90)
      VALIP(185)=CCP(26)*Vali3p(9)+CCP(27)*Vali3p(25)+CCP(28)*Vali3p(41)
     &+CCP(29)*Vali3p(57)+CCP(30)*Vali3p(73)+Vali3p(89)
3100  if(Ldmax.EQ.1)then
      VALIP(181)=CCP(26)*Vali3p(5)+CCP(27)*Vali3p(21)+CCP(28)*Vali3p(37)
     &+CCP(29)*Vali3p(53)+CCP(30)*Vali3p(69)+Vali3p(85)
      goto 3300
      elseif(Ldmax.EQ.2)then
      goto 3200
      elseif(Ldmax.NE.3)then
      
      VALIP(184)=CCP(26)*Vali3p(8)+CCP(27)*Vali3p(24)+CCP(28)*Vali3p(40)
     &+CCP(29)*Vali3p(56)+CCP(30)*Vali3p(72)+Vali3p(88)
      endif
      VALIP(183)=CCP(26)*Vali3p(7)+CCP(27)*Vali3p(23)+CCP(28)*Vali3p(39)
     &+CCP(29)*Vali3p(55)+CCP(30)*Vali3p(71)+Vali3p(87)
3200  VALIP(182)=CCP(26)*Vali3p(6)+CCP(27)*Vali3p(22)+CCP(28)*Vali3p(38)
     &+CCP(29)*Vali3p(54)+CCP(30)*Vali3p(70)+Vali3p(86)
      VALIP(181)=CCP(26)*Vali3p(5)+CCP(27)*Vali3p(21)+CCP(28)*Vali3p(37)
     &+CCP(29)*Vali3p(53)+CCP(30)*Vali3p(69)+Vali3p(85)
3300  if(Ldmax.EQ.1)then
      VALIP(177)=CCP(26)*Vali3p(1)+CCP(27)*Vali3p(17)+CCP(28)*Vali3p(33)
     &+CCP(29)*Vali3p(49)+CCP(30)*Vali3p(65)+Vali3p(81)
      goto 3500
      elseif(Ldmax.EQ.2)then
      goto 3400
      elseif(Ldmax.NE.3)then
      
      VALIP(180)=CCP(26)*Vali3p(4)+CCP(27)*Vali3p(20)+CCP(28)*Vali3p(36)
     &+CCP(29)*Vali3p(52)+CCP(30)*Vali3p(68)+Vali3p(84)
      endif
      VALIP(179)=CCP(26)*Vali3p(3)+CCP(27)*Vali3p(19)+CCP(28)*Vali3p(35)
     &+CCP(29)*Vali3p(51)+CCP(30)*Vali3p(67)+Vali3p(83)
3400  VALIP(178)=CCP(26)*Vali3p(2)+CCP(27)*Vali3p(18)+CCP(28)*Vali3p(34)
     &+CCP(29)*Vali3p(50)+CCP(30)*Vali3p(66)+Vali3p(82)
      VALIP(177)=CCP(26)*Vali3p(1)+CCP(27)*Vali3p(17)+CCP(28)*Vali3p(33)
     &+CCP(29)*Vali3p(49)+CCP(30)*Vali3p(65)+Vali3p(81)
3500  if(Lcmax.EQ.1)goto 4000
      if(Lcmax.EQ.2)goto 3800
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(173)=CCP(22)*Vali3p(13)+CCP(23)*Vali3p(29)+CCP(24)*Vali3p(45
     &)+CCP(25)*Vali3p(61)+Vali3p(77)
      goto 3600
      elseif(Ldmax.EQ.2)then
      goto 3550
      elseif(Ldmax.NE.3)then
      
      VALIP(176)=CCP(22)*Vali3p(16)+CCP(23)*Vali3p(32)+CCP(24)*Vali3p(48
     &)+CCP(25)*Vali3p(64)+Vali3p(80)
      endif
      VALIP(175)=CCP(22)*Vali3p(15)+CCP(23)*Vali3p(31)+CCP(24)*Vali3p(47
     &)+CCP(25)*Vali3p(63)+Vali3p(79)
3550  VALIP(174)=CCP(22)*Vali3p(14)+CCP(23)*Vali3p(30)+CCP(24)*Vali3p(46
     &)+CCP(25)*Vali3p(62)+Vali3p(78)
      VALIP(173)=CCP(22)*Vali3p(13)+CCP(23)*Vali3p(29)+CCP(24)*Vali3p(45
     &)+CCP(25)*Vali3p(61)+Vali3p(77)
      endif
3600  if(Ldmax.EQ.1)then
      VALIP(169)=CCP(22)*Vali3p(9)+CCP(23)*Vali3p(25)+CCP(24)*Vali3p(41)
     &+CCP(25)*Vali3p(57)+Vali3p(73)
      goto 3800
      elseif(Ldmax.EQ.2)then
      goto 3700
      elseif(Ldmax.NE.3)then
      
      VALIP(172)=CCP(22)*Vali3p(12)+CCP(23)*Vali3p(28)+CCP(24)*Vali3p(44
     &)+CCP(25)*Vali3p(60)+Vali3p(76)
      endif
      VALIP(171)=CCP(22)*Vali3p(11)+CCP(23)*Vali3p(27)+CCP(24)*Vali3p(43
     &)+CCP(25)*Vali3p(59)+Vali3p(75)
3700  VALIP(170)=CCP(22)*Vali3p(10)+CCP(23)*Vali3p(26)+CCP(24)*Vali3p(42
     &)+CCP(25)*Vali3p(58)+Vali3p(74)
      VALIP(169)=CCP(22)*Vali3p(9)+CCP(23)*Vali3p(25)+CCP(24)*Vali3p(41)
     &+CCP(25)*Vali3p(57)+Vali3p(73)
3800  if(Ldmax.EQ.1)then
      VALIP(165)=CCP(22)*Vali3p(5)+CCP(23)*Vali3p(21)+CCP(24)*Vali3p(37)
     &+CCP(25)*Vali3p(53)+Vali3p(69)
      goto 4000
      elseif(Ldmax.EQ.2)then
      goto 3900
      elseif(Ldmax.NE.3)then
      
      VALIP(168)=CCP(22)*Vali3p(8)+CCP(23)*Vali3p(24)+CCP(24)*Vali3p(40)
     &+CCP(25)*Vali3p(56)+Vali3p(72)
      endif
      VALIP(167)=CCP(22)*Vali3p(7)+CCP(23)*Vali3p(23)+CCP(24)*Vali3p(39)
     &+CCP(25)*Vali3p(55)+Vali3p(71)
3900  VALIP(166)=CCP(22)*Vali3p(6)+CCP(23)*Vali3p(22)+CCP(24)*Vali3p(38)
     &+CCP(25)*Vali3p(54)+Vali3p(70)
      VALIP(165)=CCP(22)*Vali3p(5)+CCP(23)*Vali3p(21)+CCP(24)*Vali3p(37)
     &+CCP(25)*Vali3p(53)+Vali3p(69)
4000  if(Ldmax.EQ.1)then
      VALIP(161)=CCP(22)*Vali3p(1)+CCP(23)*Vali3p(17)+CCP(24)*Vali3p(33)
     &+CCP(25)*Vali3p(49)+Vali3p(65)
      goto 4200
      elseif(Ldmax.EQ.2)then
      goto 4100
      elseif(Ldmax.NE.3)then
      
      VALIP(164)=CCP(22)*Vali3p(4)+CCP(23)*Vali3p(20)+CCP(24)*Vali3p(36)
     &+CCP(25)*Vali3p(52)+Vali3p(68)
      endif
      VALIP(163)=CCP(22)*Vali3p(3)+CCP(23)*Vali3p(19)+CCP(24)*Vali3p(35)
     &+CCP(25)*Vali3p(51)+Vali3p(67)
4100  VALIP(162)=CCP(22)*Vali3p(2)+CCP(23)*Vali3p(18)+CCP(24)*Vali3p(34)
     &+CCP(25)*Vali3p(50)+Vali3p(66)
      VALIP(161)=CCP(22)*Vali3p(1)+CCP(23)*Vali3p(17)+CCP(24)*Vali3p(33)
     &+CCP(25)*Vali3p(49)+Vali3p(65)
4200  if(Lcmax.EQ.1)goto 4700
      if(Lcmax.EQ.2)goto 4500
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(157)=CCP(19)*Vali3p(13)+CCP(20)*Vali3p(29)+CCP(21)*Vali3p(45
     &)+Vali3p(61)
      goto 4300
      elseif(Ldmax.EQ.2)then
      goto 4250
      elseif(Ldmax.NE.3)then
      
      VALIP(160)=CCP(19)*Vali3p(16)+CCP(20)*Vali3p(32)+CCP(21)*Vali3p(48
     &)+Vali3p(64)
      endif
      VALIP(159)=CCP(19)*Vali3p(15)+CCP(20)*Vali3p(31)+CCP(21)*Vali3p(47
     &)+Vali3p(63)
4250  VALIP(158)=CCP(19)*Vali3p(14)+CCP(20)*Vali3p(30)+CCP(21)*Vali3p(46
     &)+Vali3p(62)
      VALIP(157)=CCP(19)*Vali3p(13)+CCP(20)*Vali3p(29)+CCP(21)*Vali3p(45
     &)+Vali3p(61)
      endif
4300  if(Ldmax.EQ.1)then
      VALIP(153)=CCP(19)*Vali3p(9)+CCP(20)*Vali3p(25)+CCP(21)*Vali3p(41)
     &+Vali3p(57)
      goto 4500
      elseif(Ldmax.EQ.2)then
      goto 4400
      elseif(Ldmax.NE.3)then
      
      VALIP(156)=CCP(19)*Vali3p(12)+CCP(20)*Vali3p(28)+CCP(21)*Vali3p(44
     &)+Vali3p(60)
      endif
      VALIP(155)=CCP(19)*Vali3p(11)+CCP(20)*Vali3p(27)+CCP(21)*Vali3p(43
     &)+Vali3p(59)
4400  VALIP(154)=CCP(19)*Vali3p(10)+CCP(20)*Vali3p(26)+CCP(21)*Vali3p(42
     &)+Vali3p(58)
      VALIP(153)=CCP(19)*Vali3p(9)+CCP(20)*Vali3p(25)+CCP(21)*Vali3p(41)
     &+Vali3p(57)
4500  if(Ldmax.EQ.1)then
      VALIP(149)=CCP(19)*Vali3p(5)+CCP(20)*Vali3p(21)+CCP(21)*Vali3p(37)
     &+Vali3p(53)
      goto 4700
      elseif(Ldmax.EQ.2)then
      goto 4600
      elseif(Ldmax.NE.3)then
      
      VALIP(152)=CCP(19)*Vali3p(8)+CCP(20)*Vali3p(24)+CCP(21)*Vali3p(40)
     &+Vali3p(56)
      endif
      VALIP(151)=CCP(19)*Vali3p(7)+CCP(20)*Vali3p(23)+CCP(21)*Vali3p(39)
     &+Vali3p(55)
4600  VALIP(150)=CCP(19)*Vali3p(6)+CCP(20)*Vali3p(22)+CCP(21)*Vali3p(38)
     &+Vali3p(54)
      VALIP(149)=CCP(19)*Vali3p(5)+CCP(20)*Vali3p(21)+CCP(21)*Vali3p(37)
     &+Vali3p(53)
4700  if(Ldmax.EQ.1)then
      VALIP(145)=CCP(19)*Vali3p(1)+CCP(20)*Vali3p(17)+CCP(21)*Vali3p(33)
     &+Vali3p(49)
      goto 4900
      elseif(Ldmax.EQ.2)then
      goto 4800
      elseif(Ldmax.NE.3)then
      
      VALIP(148)=CCP(19)*Vali3p(4)+CCP(20)*Vali3p(20)+CCP(21)*Vali3p(36)
     &+Vali3p(52)
      endif
      VALIP(147)=CCP(19)*Vali3p(3)+CCP(20)*Vali3p(19)+CCP(21)*Vali3p(35)
     &+Vali3p(51)
4800  VALIP(146)=CCP(19)*Vali3p(2)+CCP(20)*Vali3p(18)+CCP(21)*Vali3p(34)
     &+Vali3p(50)
      VALIP(145)=CCP(19)*Vali3p(1)+CCP(20)*Vali3p(17)+CCP(21)*Vali3p(33)
     &+Vali3p(49)
4900  if(Lcmax.EQ.1)goto 5400
      if(Lcmax.EQ.2)goto 5200
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(141)=CCP(17)*Vali3p(13)+CCP(18)*Vali3p(29)+Vali3p(45)
      goto 5000
      elseif(Ldmax.EQ.2)then
      goto 4950
      elseif(Ldmax.NE.3)then
      
      VALIP(144)=CCP(17)*Vali3p(16)+CCP(18)*Vali3p(32)+Vali3p(48)
      endif
      VALIP(143)=CCP(17)*Vali3p(15)+CCP(18)*Vali3p(31)+Vali3p(47)
4950  VALIP(142)=CCP(17)*Vali3p(14)+CCP(18)*Vali3p(30)+Vali3p(46)
      VALIP(141)=CCP(17)*Vali3p(13)+CCP(18)*Vali3p(29)+Vali3p(45)
      endif
5000  if(Ldmax.EQ.1)then
      VALIP(137)=CCP(17)*Vali3p(9)+CCP(18)*Vali3p(25)+Vali3p(41)
      goto 5200
      elseif(Ldmax.EQ.2)then
      goto 5100
      elseif(Ldmax.NE.3)then
      
      VALIP(140)=CCP(17)*Vali3p(12)+CCP(18)*Vali3p(28)+Vali3p(44)
      endif
      VALIP(139)=CCP(17)*Vali3p(11)+CCP(18)*Vali3p(27)+Vali3p(43)
5100  VALIP(138)=CCP(17)*Vali3p(10)+CCP(18)*Vali3p(26)+Vali3p(42)
      VALIP(137)=CCP(17)*Vali3p(9)+CCP(18)*Vali3p(25)+Vali3p(41)
5200  if(Ldmax.EQ.1)then
      VALIP(133)=CCP(17)*Vali3p(5)+CCP(18)*Vali3p(21)+Vali3p(37)
      goto 5400
      elseif(Ldmax.EQ.2)then
      goto 5300
      elseif(Ldmax.NE.3)then
      
      VALIP(136)=CCP(17)*Vali3p(8)+CCP(18)*Vali3p(24)+Vali3p(40)
      endif
      VALIP(135)=CCP(17)*Vali3p(7)+CCP(18)*Vali3p(23)+Vali3p(39)
5300  VALIP(134)=CCP(17)*Vali3p(6)+CCP(18)*Vali3p(22)+Vali3p(38)
      VALIP(133)=CCP(17)*Vali3p(5)+CCP(18)*Vali3p(21)+Vali3p(37)
5400  if(Ldmax.EQ.1)then
      VALIP(129)=CCP(17)*Vali3p(1)+CCP(18)*Vali3p(17)+Vali3p(33)
      goto 5600
      elseif(Ldmax.EQ.2)then
      goto 5500
      elseif(Ldmax.NE.3)then
      
      VALIP(132)=CCP(17)*Vali3p(4)+CCP(18)*Vali3p(20)+Vali3p(36)
      endif
      VALIP(131)=CCP(17)*Vali3p(3)+CCP(18)*Vali3p(19)+Vali3p(35)
5500  VALIP(130)=CCP(17)*Vali3p(2)+CCP(18)*Vali3p(18)+Vali3p(34)
      VALIP(129)=CCP(17)*Vali3p(1)+CCP(18)*Vali3p(17)+Vali3p(33)
5600  if(Lbmax.EQ.1)goto 7700
      if(Lbmax.EQ.2)goto 7000
      if(Lbmax.EQ.3)goto 6300
      
      if(Lcmax.EQ.1)goto 6100
      if(Lcmax.EQ.2)goto 5900
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(125)=CCP(13)*Vali3p(13)+CCP(14)*Vali3p(29)+CCP(15)*Vali3p(45
     &)+CCP(16)*Vali3p(61)+Vali3p(77)
      goto 5700
      elseif(Ldmax.EQ.2)then
      goto 5650
      elseif(Ldmax.NE.3)then
      
      VALIP(128)=CCP(13)*Vali3p(16)+CCP(14)*Vali3p(32)+CCP(15)*Vali3p(48
     &)+CCP(16)*Vali3p(64)+Vali3p(80)
      endif
      VALIP(127)=CCP(13)*Vali3p(15)+CCP(14)*Vali3p(31)+CCP(15)*Vali3p(47
     &)+CCP(16)*Vali3p(63)+Vali3p(79)
5650  VALIP(126)=CCP(13)*Vali3p(14)+CCP(14)*Vali3p(30)+CCP(15)*Vali3p(46
     &)+CCP(16)*Vali3p(62)+Vali3p(78)
      VALIP(125)=CCP(13)*Vali3p(13)+CCP(14)*Vali3p(29)+CCP(15)*Vali3p(45
     &)+CCP(16)*Vali3p(61)+Vali3p(77)
      endif
5700  if(Ldmax.EQ.1)then
      VALIP(121)=CCP(13)*Vali3p(9)+CCP(14)*Vali3p(25)+CCP(15)*Vali3p(41)
     &+CCP(16)*Vali3p(57)+Vali3p(73)
      goto 5900
      elseif(Ldmax.EQ.2)then
      goto 5800
      elseif(Ldmax.NE.3)then
      
      VALIP(124)=CCP(13)*Vali3p(12)+CCP(14)*Vali3p(28)+CCP(15)*Vali3p(44
     &)+CCP(16)*Vali3p(60)+Vali3p(76)
      endif
      VALIP(123)=CCP(13)*Vali3p(11)+CCP(14)*Vali3p(27)+CCP(15)*Vali3p(43
     &)+CCP(16)*Vali3p(59)+Vali3p(75)
5800  VALIP(122)=CCP(13)*Vali3p(10)+CCP(14)*Vali3p(26)+CCP(15)*Vali3p(42
     &)+CCP(16)*Vali3p(58)+Vali3p(74)
      VALIP(121)=CCP(13)*Vali3p(9)+CCP(14)*Vali3p(25)+CCP(15)*Vali3p(41)
     &+CCP(16)*Vali3p(57)+Vali3p(73)
5900  if(Ldmax.EQ.1)then
      VALIP(117)=CCP(13)*Vali3p(5)+CCP(14)*Vali3p(21)+CCP(15)*Vali3p(37)
     &+CCP(16)*Vali3p(53)+Vali3p(69)
      goto 6100
      elseif(Ldmax.EQ.2)then
      goto 6000
      elseif(Ldmax.NE.3)then
      
      VALIP(120)=CCP(13)*Vali3p(8)+CCP(14)*Vali3p(24)+CCP(15)*Vali3p(40)
     &+CCP(16)*Vali3p(56)+Vali3p(72)
      endif
      VALIP(119)=CCP(13)*Vali3p(7)+CCP(14)*Vali3p(23)+CCP(15)*Vali3p(39)
     &+CCP(16)*Vali3p(55)+Vali3p(71)
6000  VALIP(118)=CCP(13)*Vali3p(6)+CCP(14)*Vali3p(22)+CCP(15)*Vali3p(38)
     &+CCP(16)*Vali3p(54)+Vali3p(70)
      VALIP(117)=CCP(13)*Vali3p(5)+CCP(14)*Vali3p(21)+CCP(15)*Vali3p(37)
     &+CCP(16)*Vali3p(53)+Vali3p(69)
6100  if(Ldmax.EQ.1)then
      VALIP(113)=CCP(13)*Vali3p(1)+CCP(14)*Vali3p(17)+CCP(15)*Vali3p(33)
     &+CCP(16)*Vali3p(49)+Vali3p(65)
      goto 6300
      elseif(Ldmax.EQ.2)then
      goto 6200
      elseif(Ldmax.NE.3)then
      
      VALIP(116)=CCP(13)*Vali3p(4)+CCP(14)*Vali3p(20)+CCP(15)*Vali3p(36)
     &+CCP(16)*Vali3p(52)+Vali3p(68)
      endif
      VALIP(115)=CCP(13)*Vali3p(3)+CCP(14)*Vali3p(19)+CCP(15)*Vali3p(35)
     &+CCP(16)*Vali3p(51)+Vali3p(67)
6200  VALIP(114)=CCP(13)*Vali3p(2)+CCP(14)*Vali3p(18)+CCP(15)*Vali3p(34)
     &+CCP(16)*Vali3p(50)+Vali3p(66)
      VALIP(113)=CCP(13)*Vali3p(1)+CCP(14)*Vali3p(17)+CCP(15)*Vali3p(33)
     &+CCP(16)*Vali3p(49)+Vali3p(65)
6300  if(Lcmax.EQ.1)goto 6800
      if(Lcmax.EQ.2)goto 6600
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(109)=CCP(10)*Vali3p(13)+CCP(11)*Vali3p(29)+CCP(12)*Vali3p(45
     &)+Vali3p(61)
      goto 6400
      elseif(Ldmax.EQ.2)then
      goto 6350
      elseif(Ldmax.NE.3)then
      
      VALIP(112)=CCP(10)*Vali3p(16)+CCP(11)*Vali3p(32)+CCP(12)*Vali3p(48
     &)+Vali3p(64)
      endif
      VALIP(111)=CCP(10)*Vali3p(15)+CCP(11)*Vali3p(31)+CCP(12)*Vali3p(47
     &)+Vali3p(63)
6350  VALIP(110)=CCP(10)*Vali3p(14)+CCP(11)*Vali3p(30)+CCP(12)*Vali3p(46
     &)+Vali3p(62)
      VALIP(109)=CCP(10)*Vali3p(13)+CCP(11)*Vali3p(29)+CCP(12)*Vali3p(45
     &)+Vali3p(61)
      endif
6400  if(Ldmax.EQ.1)then
      VALIP(105)=CCP(10)*Vali3p(9)+CCP(11)*Vali3p(25)+CCP(12)*Vali3p(41)
     &+Vali3p(57)
      goto 6600
      elseif(Ldmax.EQ.2)then
      goto 6500
      elseif(Ldmax.NE.3)then
      
      VALIP(108)=CCP(10)*Vali3p(12)+CCP(11)*Vali3p(28)+CCP(12)*Vali3p(44
     &)+Vali3p(60)
      endif
      VALIP(107)=CCP(10)*Vali3p(11)+CCP(11)*Vali3p(27)+CCP(12)*Vali3p(43
     &)+Vali3p(59)
6500  VALIP(106)=CCP(10)*Vali3p(10)+CCP(11)*Vali3p(26)+CCP(12)*Vali3p(42
     &)+Vali3p(58)
      VALIP(105)=CCP(10)*Vali3p(9)+CCP(11)*Vali3p(25)+CCP(12)*Vali3p(41)
     &+Vali3p(57)
6600  if(Ldmax.EQ.1)then
      VALIP(101)=CCP(10)*Vali3p(5)+CCP(11)*Vali3p(21)+CCP(12)*Vali3p(37)
     &+Vali3p(53)
      goto 6800
      elseif(Ldmax.EQ.2)then
      goto 6700
      elseif(Ldmax.NE.3)then
      
      VALIP(104)=CCP(10)*Vali3p(8)+CCP(11)*Vali3p(24)+CCP(12)*Vali3p(40)
     &+Vali3p(56)
      endif
      VALIP(103)=CCP(10)*Vali3p(7)+CCP(11)*Vali3p(23)+CCP(12)*Vali3p(39)
     &+Vali3p(55)
6700  VALIP(102)=CCP(10)*Vali3p(6)+CCP(11)*Vali3p(22)+CCP(12)*Vali3p(38)
     &+Vali3p(54)
      VALIP(101)=CCP(10)*Vali3p(5)+CCP(11)*Vali3p(21)+CCP(12)*Vali3p(37)
     &+Vali3p(53)
6800  if(Ldmax.EQ.1)then
      VALIP(97)=CCP(10)*Vali3p(1)+CCP(11)*Vali3p(17)+CCP(12)*Vali3p(33)+
     &Vali3p(49)
      goto 7000
      elseif(Ldmax.EQ.2)then
      goto 6900
      elseif(Ldmax.NE.3)then
      
      VALIP(100)=CCP(10)*Vali3p(4)+CCP(11)*Vali3p(20)+CCP(12)*Vali3p(36)
     &+Vali3p(52)
      endif
      VALIP(99)=CCP(10)*Vali3p(3)+CCP(11)*Vali3p(19)+CCP(12)*Vali3p(35)+
     &Vali3p(51)
6900  VALIP(98)=CCP(10)*Vali3p(2)+CCP(11)*Vali3p(18)+CCP(12)*Vali3p(34)+
     &Vali3p(50)
      VALIP(97)=CCP(10)*Vali3p(1)+CCP(11)*Vali3p(17)+CCP(12)*Vali3p(33)+
     &Vali3p(49)
7000  if(Lcmax.EQ.1)goto 7500
      if(Lcmax.EQ.2)goto 7300
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(93)=CCP(8)*Vali3p(13)+CCP(9)*Vali3p(29)+Vali3p(45)
      goto 7100
      elseif(Ldmax.EQ.2)then
      goto 7050
      elseif(Ldmax.NE.3)then
      
      VALIP(96)=CCP(8)*Vali3p(16)+CCP(9)*Vali3p(32)+Vali3p(48)
      endif
      VALIP(95)=CCP(8)*Vali3p(15)+CCP(9)*Vali3p(31)+Vali3p(47)
7050  VALIP(94)=CCP(8)*Vali3p(14)+CCP(9)*Vali3p(30)+Vali3p(46)
      VALIP(93)=CCP(8)*Vali3p(13)+CCP(9)*Vali3p(29)+Vali3p(45)
      endif
7100  if(Ldmax.EQ.1)then
      VALIP(89)=CCP(8)*Vali3p(9)+CCP(9)*Vali3p(25)+Vali3p(41)
      goto 7300
      elseif(Ldmax.EQ.2)then
      goto 7200
      elseif(Ldmax.NE.3)then
      
      VALIP(92)=CCP(8)*Vali3p(12)+CCP(9)*Vali3p(28)+Vali3p(44)
      endif
      VALIP(91)=CCP(8)*Vali3p(11)+CCP(9)*Vali3p(27)+Vali3p(43)
7200  VALIP(90)=CCP(8)*Vali3p(10)+CCP(9)*Vali3p(26)+Vali3p(42)
      VALIP(89)=CCP(8)*Vali3p(9)+CCP(9)*Vali3p(25)+Vali3p(41)
7300  if(Ldmax.EQ.1)then
      VALIP(85)=CCP(8)*Vali3p(5)+CCP(9)*Vali3p(21)+Vali3p(37)
      goto 7500
      elseif(Ldmax.EQ.2)then
      goto 7400
      elseif(Ldmax.NE.3)then
      
      VALIP(88)=CCP(8)*Vali3p(8)+CCP(9)*Vali3p(24)+Vali3p(40)
      endif
      VALIP(87)=CCP(8)*Vali3p(7)+CCP(9)*Vali3p(23)+Vali3p(39)
7400  VALIP(86)=CCP(8)*Vali3p(6)+CCP(9)*Vali3p(22)+Vali3p(38)
      VALIP(85)=CCP(8)*Vali3p(5)+CCP(9)*Vali3p(21)+Vali3p(37)
7500  if(Ldmax.EQ.1)then
      VALIP(81)=CCP(8)*Vali3p(1)+CCP(9)*Vali3p(17)+Vali3p(33)
      goto 7700
      elseif(Ldmax.EQ.2)then
      goto 7600
      elseif(Ldmax.NE.3)then
      
      VALIP(84)=CCP(8)*Vali3p(4)+CCP(9)*Vali3p(20)+Vali3p(36)
      endif
      VALIP(83)=CCP(8)*Vali3p(3)+CCP(9)*Vali3p(19)+Vali3p(35)
7600  VALIP(82)=CCP(8)*Vali3p(2)+CCP(9)*Vali3p(18)+Vali3p(34)
      VALIP(81)=CCP(8)*Vali3p(1)+CCP(9)*Vali3p(17)+Vali3p(33)
7700  if(Lcmax.EQ.1)goto 8200
      if(Lcmax.EQ.2)goto 8000
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(77)=CCP(7)*Vali3p(13)+Vali3p(29)
      goto 7800
      elseif(Ldmax.EQ.2)then
      goto 7750
      elseif(Ldmax.NE.3)then
      
      VALIP(80)=CCP(7)*Vali3p(16)+Vali3p(32)
      endif
      VALIP(79)=CCP(7)*Vali3p(15)+Vali3p(31)
7750  VALIP(78)=CCP(7)*Vali3p(14)+Vali3p(30)
      VALIP(77)=CCP(7)*Vali3p(13)+Vali3p(29)
      endif
7800  if(Ldmax.EQ.1)then
      VALIP(73)=CCP(7)*Vali3p(9)+Vali3p(25)
      goto 8000
      elseif(Ldmax.EQ.2)then
      goto 7900
      elseif(Ldmax.NE.3)then
      
      VALIP(76)=CCP(7)*Vali3p(12)+Vali3p(28)
      endif
      VALIP(75)=CCP(7)*Vali3p(11)+Vali3p(27)
7900  VALIP(74)=CCP(7)*Vali3p(10)+Vali3p(26)
      VALIP(73)=CCP(7)*Vali3p(9)+Vali3p(25)
8000  if(Ldmax.EQ.1)then
      VALIP(69)=CCP(7)*Vali3p(5)+Vali3p(21)
      goto 8200
      elseif(Ldmax.EQ.2)then
      goto 8100
      elseif(Ldmax.NE.3)then
      
      VALIP(72)=CCP(7)*Vali3p(8)+Vali3p(24)
      endif
      VALIP(71)=CCP(7)*Vali3p(7)+Vali3p(23)
8100  VALIP(70)=CCP(7)*Vali3p(6)+Vali3p(22)
      VALIP(69)=CCP(7)*Vali3p(5)+Vali3p(21)
8200  if(Ldmax.EQ.1)then
      VALIP(65)=CCP(7)*Vali3p(1)+Vali3p(17)
      goto 8400
      elseif(Ldmax.EQ.2)then
      goto 8300
      elseif(Ldmax.NE.3)then
      
      VALIP(68)=CCP(7)*Vali3p(4)+Vali3p(20)
      endif
      VALIP(67)=CCP(7)*Vali3p(3)+Vali3p(19)
8300  VALIP(66)=CCP(7)*Vali3p(2)+Vali3p(18)
      VALIP(65)=CCP(7)*Vali3p(1)+Vali3p(17)
8400  if(Lbmax.EQ.1)goto 10500
      if(Lbmax.EQ.2)goto 9800
      if(Lbmax.EQ.3)goto 9100
      
      if(Lcmax.EQ.1)goto 8900
      if(Lcmax.EQ.2)goto 8700
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(61)=CCP(4)*Vali3p(13)+CCP(5)*Vali3p(29)+CCP(6)*Vali3p(45)+Va
     &li3p(61)
      goto 8500
      elseif(Ldmax.EQ.2)then
      goto 8450
      elseif(Ldmax.NE.3)then
      
      VALIP(64)=CCP(4)*Vali3p(16)+CCP(5)*Vali3p(32)+CCP(6)*Vali3p(48)+Va
     &li3p(64)
      endif
      VALIP(63)=CCP(4)*Vali3p(15)+CCP(5)*Vali3p(31)+CCP(6)*Vali3p(47)+Va
     &li3p(63)
8450  VALIP(62)=CCP(4)*Vali3p(14)+CCP(5)*Vali3p(30)+CCP(6)*Vali3p(46)+Va
     &li3p(62)
      VALIP(61)=CCP(4)*Vali3p(13)+CCP(5)*Vali3p(29)+CCP(6)*Vali3p(45)+Va
     &li3p(61)
      endif
8500  if(Ldmax.EQ.1)then
      VALIP(57)=CCP(4)*Vali3p(9)+CCP(5)*Vali3p(25)+CCP(6)*Vali3p(41)+Val
     &i3p(57)
      goto 8700
      elseif(Ldmax.EQ.2)then
      goto 8600
      elseif(Ldmax.NE.3)then
      
      VALIP(60)=CCP(4)*Vali3p(12)+CCP(5)*Vali3p(28)+CCP(6)*Vali3p(44)+Va
     &li3p(60)
      endif
      VALIP(59)=CCP(4)*Vali3p(11)+CCP(5)*Vali3p(27)+CCP(6)*Vali3p(43)+Va
     &li3p(59)
8600  VALIP(58)=CCP(4)*Vali3p(10)+CCP(5)*Vali3p(26)+CCP(6)*Vali3p(42)+Va
     &li3p(58)
      VALIP(57)=CCP(4)*Vali3p(9)+CCP(5)*Vali3p(25)+CCP(6)*Vali3p(41)+Val
     &i3p(57)
8700  if(Ldmax.EQ.1)then
      VALIP(53)=CCP(4)*Vali3p(5)+CCP(5)*Vali3p(21)+CCP(6)*Vali3p(37)+Val
     &i3p(53)
      goto 8900
      elseif(Ldmax.EQ.2)then
      goto 8800
      elseif(Ldmax.NE.3)then
      
      VALIP(56)=CCP(4)*Vali3p(8)+CCP(5)*Vali3p(24)+CCP(6)*Vali3p(40)+Val
     &i3p(56)
      endif
      VALIP(55)=CCP(4)*Vali3p(7)+CCP(5)*Vali3p(23)+CCP(6)*Vali3p(39)+Val
     &i3p(55)
8800  VALIP(54)=CCP(4)*Vali3p(6)+CCP(5)*Vali3p(22)+CCP(6)*Vali3p(38)+Val
     &i3p(54)
      VALIP(53)=CCP(4)*Vali3p(5)+CCP(5)*Vali3p(21)+CCP(6)*Vali3p(37)+Val
     &i3p(53)
8900  if(Ldmax.EQ.1)then
      VALIP(49)=CCP(4)*Vali3p(1)+CCP(5)*Vali3p(17)+CCP(6)*Vali3p(33)+Val
     &i3p(49)
      goto 9100
      elseif(Ldmax.EQ.2)then
      goto 9000
      elseif(Ldmax.NE.3)then
      
      VALIP(52)=CCP(4)*Vali3p(4)+CCP(5)*Vali3p(20)+CCP(6)*Vali3p(36)+Val
     &i3p(52)
      endif
      VALIP(51)=CCP(4)*Vali3p(3)+CCP(5)*Vali3p(19)+CCP(6)*Vali3p(35)+Val
     &i3p(51)
9000  VALIP(50)=CCP(4)*Vali3p(2)+CCP(5)*Vali3p(18)+CCP(6)*Vali3p(34)+Val
     &i3p(50)
      VALIP(49)=CCP(4)*Vali3p(1)+CCP(5)*Vali3p(17)+CCP(6)*Vali3p(33)+Val
     &i3p(49)
9100  if(Lcmax.EQ.1)goto 9600
      if(Lcmax.EQ.2)goto 9400
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(45)=CCP(2)*Vali3p(13)+CCP(3)*Vali3p(29)+Vali3p(45)
      goto 9200
      elseif(Ldmax.EQ.2)then
      goto 9150
      elseif(Ldmax.NE.3)then
      
      VALIP(48)=CCP(2)*Vali3p(16)+CCP(3)*Vali3p(32)+Vali3p(48)
      endif
      VALIP(47)=CCP(2)*Vali3p(15)+CCP(3)*Vali3p(31)+Vali3p(47)
9150  VALIP(46)=CCP(2)*Vali3p(14)+CCP(3)*Vali3p(30)+Vali3p(46)
      VALIP(45)=CCP(2)*Vali3p(13)+CCP(3)*Vali3p(29)+Vali3p(45)
      endif
9200  if(Ldmax.EQ.1)then
      VALIP(41)=CCP(2)*Vali3p(9)+CCP(3)*Vali3p(25)+Vali3p(41)
      goto 9400
      elseif(Ldmax.EQ.2)then
      goto 9300
      elseif(Ldmax.NE.3)then
      
      VALIP(44)=CCP(2)*Vali3p(12)+CCP(3)*Vali3p(28)+Vali3p(44)
      endif
      VALIP(43)=CCP(2)*Vali3p(11)+CCP(3)*Vali3p(27)+Vali3p(43)
9300  VALIP(42)=CCP(2)*Vali3p(10)+CCP(3)*Vali3p(26)+Vali3p(42)
      VALIP(41)=CCP(2)*Vali3p(9)+CCP(3)*Vali3p(25)+Vali3p(41)
9400  if(Ldmax.EQ.1)then
      VALIP(37)=CCP(2)*Vali3p(5)+CCP(3)*Vali3p(21)+Vali3p(37)
      goto 9600
      elseif(Ldmax.EQ.2)then
      goto 9500
      elseif(Ldmax.NE.3)then
      
      VALIP(40)=CCP(2)*Vali3p(8)+CCP(3)*Vali3p(24)+Vali3p(40)
      endif
      VALIP(39)=CCP(2)*Vali3p(7)+CCP(3)*Vali3p(23)+Vali3p(39)
9500  VALIP(38)=CCP(2)*Vali3p(6)+CCP(3)*Vali3p(22)+Vali3p(38)
      VALIP(37)=CCP(2)*Vali3p(5)+CCP(3)*Vali3p(21)+Vali3p(37)
9600  if(Ldmax.EQ.1)then
      VALIP(33)=CCP(2)*Vali3p(1)+CCP(3)*Vali3p(17)+Vali3p(33)
      goto 9800
      elseif(Ldmax.EQ.2)then
      goto 9700
      elseif(Ldmax.NE.3)then
      
      VALIP(36)=CCP(2)*Vali3p(4)+CCP(3)*Vali3p(20)+Vali3p(36)
      endif
      VALIP(35)=CCP(2)*Vali3p(3)+CCP(3)*Vali3p(19)+Vali3p(35)
9700  VALIP(34)=CCP(2)*Vali3p(2)+CCP(3)*Vali3p(18)+Vali3p(34)
      VALIP(33)=CCP(2)*Vali3p(1)+CCP(3)*Vali3p(17)+Vali3p(33)
9800  if(Lcmax.EQ.1)goto 10300
      if(Lcmax.EQ.2)goto 10100
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(29)=CCP(1)*Vali3p(13)+Vali3p(29)
      goto 9900
      elseif(Ldmax.EQ.2)then
      goto 9850
      elseif(Ldmax.NE.3)then
      
      VALIP(32)=CCP(1)*Vali3p(16)+Vali3p(32)
      endif
      VALIP(31)=CCP(1)*Vali3p(15)+Vali3p(31)
9850  VALIP(30)=CCP(1)*Vali3p(14)+Vali3p(30)
      VALIP(29)=CCP(1)*Vali3p(13)+Vali3p(29)
      endif
9900  if(Ldmax.EQ.1)then
      VALIP(25)=CCP(1)*Vali3p(9)+Vali3p(25)
      goto 10100
      elseif(Ldmax.EQ.2)then
      goto 10000
      elseif(Ldmax.NE.3)then
      
      VALIP(28)=CCP(1)*Vali3p(12)+Vali3p(28)
      endif
      VALIP(27)=CCP(1)*Vali3p(11)+Vali3p(27)
10000 VALIP(26)=CCP(1)*Vali3p(10)+Vali3p(26)
      VALIP(25)=CCP(1)*Vali3p(9)+Vali3p(25)
10100 if(Ldmax.EQ.1)then
      VALIP(21)=CCP(1)*Vali3p(5)+Vali3p(21)
      goto 10300
      elseif(Ldmax.EQ.2)then
      goto 10200
      elseif(Ldmax.NE.3)then
      
      VALIP(24)=CCP(1)*Vali3p(8)+Vali3p(24)
      endif
      VALIP(23)=CCP(1)*Vali3p(7)+Vali3p(23)
10200 VALIP(22)=CCP(1)*Vali3p(6)+Vali3p(22)
      VALIP(21)=CCP(1)*Vali3p(5)+Vali3p(21)
10300 if(Ldmax.EQ.1)then
      VALIP(17)=CCP(1)*Vali3p(1)+Vali3p(17)
      goto 10500
      elseif(Ldmax.EQ.2)then
      goto 10400
      elseif(Ldmax.NE.3)then
      
      VALIP(20)=CCP(1)*Vali3p(4)+Vali3p(20)
      endif
      VALIP(19)=CCP(1)*Vali3p(3)+Vali3p(19)
10400 VALIP(18)=CCP(1)*Vali3p(2)+Vali3p(18)
      VALIP(17)=CCP(1)*Vali3p(1)+Vali3p(17)
10500 if(Lcmax.EQ.1)goto 11000
      if(Lcmax.EQ.2)goto 10800
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      VALIP(13)=Vali3p(13)
      goto 10600
      elseif(Ldmax.EQ.2)then
      goto 10550
      elseif(Ldmax.NE.3)then
      
      VALIP(16)=Vali3p(16)
      endif
      VALIP(15)=Vali3p(15)
10550 VALIP(14)=Vali3p(14)
      VALIP(13)=Vali3p(13)
      endif
10600 if(Ldmax.EQ.1)then
      VALIP(9)=Vali3p(9)
      goto 10800
      elseif(Ldmax.EQ.2)then
      goto 10700
      elseif(Ldmax.NE.3)then
      
      VALIP(12)=Vali3p(12)
      endif
      VALIP(11)=Vali3p(11)
10700 VALIP(10)=Vali3p(10)
      VALIP(9)=Vali3p(9)
10800 if(Ldmax.EQ.1)then
      VALIP(5)=Vali3p(5)
      goto 11000
      elseif(Ldmax.EQ.2)then
      goto 10900
      elseif(Ldmax.NE.3)then
      
      VALIP(8)=Vali3p(8)
      endif
      VALIP(7)=Vali3p(7)
10900 VALIP(6)=Vali3p(6)
      VALIP(5)=Vali3p(5)
11000 if(Ldmax.EQ.1)goto 11200
      if(Ldmax.EQ.2)goto 11100
      
      if(Ldmax.NE.3)VALIP(4)=Vali3p(4)
      VALIP(3)=Vali3p(3)
11100 VALIP(2)=Vali3p(2)
11200 VALIP(1)=Vali3p(1)
      return
      
      end
C* :1 * 
      
