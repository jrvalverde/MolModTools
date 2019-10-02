
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 twod2c"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "twod2c.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "twod2c.web"
      subroutine twod2c
      implicit none
      double precision A,G,Vali2p,Vali3p
      integer Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lpqmax,Lqmax
      common/a/A(174)
      common/int2d/G(13),Vali2p(49),Vali3p(112)
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      
      
      if(Lpmax.EQ.1)goto 1100
      if(Lpmax.EQ.2)goto 900
      if(Lpmax.EQ.3)goto 700
      if(Lpmax.EQ.4)goto 500
      if(Lpmax.EQ.5)goto 300
      if(Lpmax.NE.6)then
      
      if(Lqmax.EQ.1)then
      Vali2p(43)=A(138)*G(1)+A(139)*G(3)+A(140)*G(5)+A(141)*G(7)
      goto 100
      elseif(Lqmax.EQ.2)then
      Vali2p(44)=A(142)*G(2)+A(143)*G(4)+A(144)*G(6)+A(145)*G(8)
      Vali2p(43)=A(138)*G(1)+A(139)*G(3)+A(140)*G(5)+A(141)*G(7)
      goto 100
      elseif(Lqmax.EQ.3)then
      Vali2p(45)=A(146)*G(1)+A(147)*G(3)+A(148)*G(5)+A(149)*G(7)+A(150)*
     &G(9)
      Vali2p(44)=A(142)*G(2)+A(143)*G(4)+A(144)*G(6)+A(145)*G(8)
      Vali2p(43)=A(138)*G(1)+A(139)*G(3)+A(140)*G(5)+A(141)*G(7)
      goto 100
      elseif(Lqmax.EQ.4)then
      Vali2p(46)=A(151)*G(2)+A(152)*G(4)+A(153)*G(6)+A(154)*G(8)+A(155)*
     &G(10)
      Vali2p(45)=A(146)*G(1)+A(147)*G(3)+A(148)*G(5)+A(149)*G(7)+A(150)*
     &G(9)
      Vali2p(44)=A(142)*G(2)+A(143)*G(4)+A(144)*G(6)+A(145)*G(8)
      Vali2p(43)=A(138)*G(1)+A(139)*G(3)+A(140)*G(5)+A(141)*G(7)
      goto 100
      elseif(Lqmax.EQ.5)then
      goto 50
      elseif(Lqmax.NE.6)then
      
      Vali2p(49)=A(168)*G(1)+A(169)*G(3)+A(170)*G(5)+A(171)*G(7)+A(172)*
     &G(9)+A(173)*G(11)+A(174)*G(13)
      endif
      Vali2p(48)=A(162)*G(2)+A(163)*G(4)+A(164)*G(6)+A(165)*G(8)+A(166)*
     &G(10)+A(167)*G(12)
50    Vali2p(47)=A(156)*G(1)+A(157)*G(3)+A(158)*G(5)+A(159)*G(7)+A(160)*
     &G(9)+A(161)*G(11)
      Vali2p(46)=A(151)*G(2)+A(152)*G(4)+A(153)*G(6)+A(154)*G(8)+A(155)*
     &G(10)
      Vali2p(45)=A(146)*G(1)+A(147)*G(3)+A(148)*G(5)+A(149)*G(7)+A(150)*
     &G(9)
      Vali2p(44)=A(142)*G(2)+A(143)*G(4)+A(144)*G(6)+A(145)*G(8)
      Vali2p(43)=A(138)*G(1)+A(139)*G(3)+A(140)*G(5)+A(141)*G(7)
      endif
100   if(Lqmax.EQ.1)then
      Vali2p(36)=A(108)*G(2)+A(109)*G(4)+A(110)*G(6)
      goto 300
      elseif(Lqmax.EQ.2)then
      Vali2p(37)=A(111)*G(3)+A(112)*G(5)+A(113)*G(7)
      Vali2p(36)=A(108)*G(2)+A(109)*G(4)+A(110)*G(6)
      goto 300
      elseif(Lqmax.EQ.3)then
      Vali2p(38)=A(114)*G(2)+A(115)*G(4)+A(116)*G(6)+A(117)*G(8)
      Vali2p(37)=A(111)*G(3)+A(112)*G(5)+A(113)*G(7)
      Vali2p(36)=A(108)*G(2)+A(109)*G(4)+A(110)*G(6)
      goto 300
      elseif(Lqmax.EQ.4)then
      Vali2p(39)=A(118)*G(3)+A(119)*G(5)+A(120)*G(7)+A(121)*G(9)
      Vali2p(38)=A(114)*G(2)+A(115)*G(4)+A(116)*G(6)+A(117)*G(8)
      Vali2p(37)=A(111)*G(3)+A(112)*G(5)+A(113)*G(7)
      Vali2p(36)=A(108)*G(2)+A(109)*G(4)+A(110)*G(6)
      goto 300
      elseif(Lqmax.EQ.5)then
      goto 200
      elseif(Lqmax.NE.6)then
      
      Vali2p(42)=A(132)*G(2)+A(133)*G(4)+A(134)*G(6)+A(135)*G(8)+A(136)*
     &G(10)+A(137)*G(12)
      endif
      Vali2p(41)=A(127)*G(3)+A(128)*G(5)+A(129)*G(7)+A(130)*G(9)+A(131)*
     &G(11)
200   Vali2p(40)=A(122)*G(2)+A(123)*G(4)+A(124)*G(6)+A(125)*G(8)+A(126)*
     &G(10)
      Vali2p(39)=A(118)*G(3)+A(119)*G(5)+A(120)*G(7)+A(121)*G(9)
      Vali2p(38)=A(114)*G(2)+A(115)*G(4)+A(116)*G(6)+A(117)*G(8)
      Vali2p(37)=A(111)*G(3)+A(112)*G(5)+A(113)*G(7)
      Vali2p(36)=A(108)*G(2)+A(109)*G(4)+A(110)*G(6)
300   if(Lqmax.EQ.1)then
      Vali2p(29)=A(78)*G(1)+A(79)*G(3)+A(80)*G(5)
      goto 500
      elseif(Lqmax.EQ.2)then
      Vali2p(30)=A(81)*G(2)+A(82)*G(4)+A(83)*G(6)
      Vali2p(29)=A(78)*G(1)+A(79)*G(3)+A(80)*G(5)
      goto 500
      elseif(Lqmax.EQ.3)then
      Vali2p(31)=A(84)*G(1)+A(85)*G(3)+A(86)*G(5)+A(87)*G(7)
      Vali2p(30)=A(81)*G(2)+A(82)*G(4)+A(83)*G(6)
      Vali2p(29)=A(78)*G(1)+A(79)*G(3)+A(80)*G(5)
      goto 500
      elseif(Lqmax.EQ.4)then
      Vali2p(32)=A(88)*G(2)+A(89)*G(4)+A(90)*G(6)+A(91)*G(8)
      Vali2p(31)=A(84)*G(1)+A(85)*G(3)+A(86)*G(5)+A(87)*G(7)
      Vali2p(30)=A(81)*G(2)+A(82)*G(4)+A(83)*G(6)
      Vali2p(29)=A(78)*G(1)+A(79)*G(3)+A(80)*G(5)
      goto 500
      elseif(Lqmax.EQ.5)then
      goto 400
      elseif(Lqmax.NE.6)then
      
      Vali2p(35)=A(102)*G(1)+A(103)*G(3)+A(104)*G(5)+A(105)*G(7)+A(106)*
     &G(9)+A(107)*G(11)
      endif
      Vali2p(34)=A(97)*G(2)+A(98)*G(4)+A(99)*G(6)+A(100)*G(8)+A(101)*G(1
     &0)
400   Vali2p(33)=A(92)*G(1)+A(93)*G(3)+A(94)*G(5)+A(95)*G(7)+A(96)*G(9)
      Vali2p(32)=A(88)*G(2)+A(89)*G(4)+A(90)*G(6)+A(91)*G(8)
      Vali2p(31)=A(84)*G(1)+A(85)*G(3)+A(86)*G(5)+A(87)*G(7)
      Vali2p(30)=A(81)*G(2)+A(82)*G(4)+A(83)*G(6)
      Vali2p(29)=A(78)*G(1)+A(79)*G(3)+A(80)*G(5)
500   if(Lqmax.EQ.1)then
      Vali2p(22)=A(55)*G(2)+A(56)*G(4)
      goto 700
      elseif(Lqmax.EQ.2)then
      Vali2p(23)=A(57)*G(3)+A(58)*G(5)
      Vali2p(22)=A(55)*G(2)+A(56)*G(4)
      goto 700
      elseif(Lqmax.EQ.3)then
      Vali2p(24)=A(59)*G(2)+A(60)*G(4)+A(61)*G(6)
      Vali2p(23)=A(57)*G(3)+A(58)*G(5)
      Vali2p(22)=A(55)*G(2)+A(56)*G(4)
      goto 700
      elseif(Lqmax.EQ.4)then
      Vali2p(25)=A(62)*G(3)+A(63)*G(5)+A(64)*G(7)
      Vali2p(24)=A(59)*G(2)+A(60)*G(4)+A(61)*G(6)
      Vali2p(23)=A(57)*G(3)+A(58)*G(5)
      Vali2p(22)=A(55)*G(2)+A(56)*G(4)
      goto 700
      elseif(Lqmax.EQ.5)then
      goto 600
      elseif(Lqmax.NE.6)then
      
      Vali2p(28)=A(73)*G(2)+A(74)*G(4)+A(75)*G(6)+A(76)*G(8)+A(77)*G(10)
      endif
      Vali2p(27)=A(69)*G(3)+A(70)*G(5)+A(71)*G(7)+A(72)*G(9)
600   Vali2p(26)=A(65)*G(2)+A(66)*G(4)+A(67)*G(6)+A(68)*G(8)
      Vali2p(25)=A(62)*G(3)+A(63)*G(5)+A(64)*G(7)
      Vali2p(24)=A(59)*G(2)+A(60)*G(4)+A(61)*G(6)
      Vali2p(23)=A(57)*G(3)+A(58)*G(5)
      Vali2p(22)=A(55)*G(2)+A(56)*G(4)
700   if(Lqmax.EQ.1)then
      Vali2p(15)=A(32)*G(1)+A(33)*G(3)
      goto 900
      elseif(Lqmax.EQ.2)then
      Vali2p(16)=A(34)*G(2)+A(35)*G(4)
      Vali2p(15)=A(32)*G(1)+A(33)*G(3)
      goto 900
      elseif(Lqmax.EQ.3)then
      Vali2p(17)=A(36)*G(1)+A(37)*G(3)+A(38)*G(5)
      Vali2p(16)=A(34)*G(2)+A(35)*G(4)
      Vali2p(15)=A(32)*G(1)+A(33)*G(3)
      goto 900
      elseif(Lqmax.EQ.4)then
      Vali2p(18)=A(39)*G(2)+A(40)*G(4)+A(41)*G(6)
      Vali2p(17)=A(36)*G(1)+A(37)*G(3)+A(38)*G(5)
      Vali2p(16)=A(34)*G(2)+A(35)*G(4)
      Vali2p(15)=A(32)*G(1)+A(33)*G(3)
      goto 900
      elseif(Lqmax.EQ.5)then
      goto 800
      elseif(Lqmax.NE.6)then
      
      Vali2p(21)=A(50)*G(1)+A(51)*G(3)+A(52)*G(5)+A(53)*G(7)+A(54)*G(9)
      endif
      Vali2p(20)=A(46)*G(2)+A(47)*G(4)+A(48)*G(6)+A(49)*G(8)
800   Vali2p(19)=A(42)*G(1)+A(43)*G(3)+A(44)*G(5)+A(45)*G(7)
      Vali2p(18)=A(39)*G(2)+A(40)*G(4)+A(41)*G(6)
      Vali2p(17)=A(36)*G(1)+A(37)*G(3)+A(38)*G(5)
      Vali2p(16)=A(34)*G(2)+A(35)*G(4)
      Vali2p(15)=A(32)*G(1)+A(33)*G(3)
900   if(Lqmax.EQ.1)then
      Vali2p(8)=A(16)*G(2)
      goto 1100
      elseif(Lqmax.EQ.2)then
      Vali2p(9)=A(17)*G(3)
      Vali2p(8)=A(16)*G(2)
      goto 1100
      elseif(Lqmax.EQ.3)then
      Vali2p(10)=A(18)*G(2)+A(19)*G(4)
      Vali2p(9)=A(17)*G(3)
      Vali2p(8)=A(16)*G(2)
      goto 1100
      elseif(Lqmax.EQ.4)then
      Vali2p(11)=A(20)*G(3)+A(21)*G(5)
      Vali2p(10)=A(18)*G(2)+A(19)*G(4)
      Vali2p(9)=A(17)*G(3)
      Vali2p(8)=A(16)*G(2)
      goto 1100
      elseif(Lqmax.EQ.5)then
      goto 1000
      elseif(Lqmax.NE.6)then
      
      Vali2p(14)=A(28)*G(2)+A(29)*G(4)+A(30)*G(6)+A(31)*G(8)
      endif
      Vali2p(13)=A(25)*G(3)+A(26)*G(5)+A(27)*G(7)
1000  Vali2p(12)=A(22)*G(2)+A(23)*G(4)+A(24)*G(6)
      Vali2p(11)=A(20)*G(3)+A(21)*G(5)
      Vali2p(10)=A(18)*G(2)+A(19)*G(4)
      Vali2p(9)=A(17)*G(3)
      Vali2p(8)=A(16)*G(2)
1100  if(Lqmax.EQ.1)goto 1300
      if(Lqmax.EQ.2)then
      Vali2p(2)=A(1)*G(2)
      goto 1300
      elseif(Lqmax.EQ.3)then
      Vali2p(3)=A(2)*G(1)+A(3)*G(3)
      Vali2p(2)=A(1)*G(2)
      goto 1300
      elseif(Lqmax.EQ.4)then
      Vali2p(4)=A(4)*G(2)+A(5)*G(4)
      Vali2p(3)=A(2)*G(1)+A(3)*G(3)
      Vali2p(2)=A(1)*G(2)
      goto 1300
      elseif(Lqmax.EQ.5)then
      goto 1200
      elseif(Lqmax.NE.6)then
      
      Vali2p(7)=A(12)*G(1)+A(13)*G(3)+A(14)*G(5)+A(15)*G(7)
      endif
      Vali2p(6)=A(9)*G(2)+A(10)*G(4)+A(11)*G(6)
1200  Vali2p(5)=A(6)*G(1)+A(7)*G(3)+A(8)*G(5)
      Vali2p(4)=A(4)*G(2)+A(5)*G(4)
      Vali2p(3)=A(2)*G(1)+A(3)*G(3)
      Vali2p(2)=A(1)*G(2)
1300  Vali2p(1)=G(1)
      return
      
      end
C* :1 * 
      
