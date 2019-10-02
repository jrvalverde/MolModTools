
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 get2cs"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "get2cs.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "get2cs.web"
      subroutine get2cs(SS,F,C,INC)
      implicit none
      double precision C,F,SS
      integer Idummy,Idump,In,INC,Iout,Ipunch,Lamax,lanew,Lbmax,Lpmax,Ma
     &xdum
      dimension C(190),F(6),SS(32)
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/dump/Idump,Idummy
      common/io/In,Iout,Ipunch
      
      
      
      
      
      
      lanew=Lamax+INC
      
      if(lanew.EQ.1)goto 1300
      if(lanew.EQ.2)goto 1100
      if(lanew.EQ.3)goto 900
      if(lanew.EQ.4)goto 700
      if(lanew.EQ.5)goto 500
      if(lanew.EQ.6)goto 300
      if(lanew.NE.7)then
      
      if(Lbmax.EQ.1)then
      SS(29)=F(1)*C(155)+F(2)*C(157)+F(3)*C(159)+F(4)*C(161)
      goto 100
      elseif(Lbmax.EQ.2)then
      goto 50
      elseif(Lbmax.NE.3)then
      
      SS(32)=F(1)*C(182)+F(2)*C(184)+F(3)*C(186)+F(4)*C(188)+F(5)*C(190)
     &+F(6)
      endif
      SS(31)=F(1)*C(172)+F(2)*C(174)+F(3)*C(176)+F(4)*C(178)+F(5)*C(180)
50    SS(30)=F(1)*C(163)+F(2)*C(165)+F(3)*C(167)+F(4)*C(169)+F(5)
      SS(29)=F(1)*C(155)+F(2)*C(157)+F(3)*C(159)+F(4)*C(161)
      endif
      
100   if(Lbmax.EQ.1)then
      SS(25)=F(1)*C(121)+F(2)*C(123)+F(3)*C(125)+F(4)
      goto 300
      elseif(Lbmax.EQ.2)then
      goto 200
      elseif(Lbmax.NE.3)then
      
      SS(28)=F(1)*C(145)+F(2)*C(147)+F(3)*C(149)+F(4)*C(151)+F(5)*C(153)
      endif
      SS(27)=F(1)*C(136)+F(2)*C(138)+F(3)*C(140)+F(4)*C(142)+F(5)
200   SS(26)=F(1)*C(128)+F(2)*C(130)+F(3)*C(132)+F(4)*C(134)
      SS(25)=F(1)*C(121)+F(2)*C(123)+F(3)*C(125)+F(4)
      
300   if(Lbmax.EQ.1)then
      SS(21)=F(1)*C(91)+F(2)*C(93)+F(3)*C(95)
      goto 500
      elseif(Lbmax.EQ.2)then
      goto 400
      elseif(Lbmax.NE.3)then
      
      SS(24)=F(1)*C(112)+F(2)*C(114)+F(3)*C(116)+F(4)*C(118)+F(5)
      endif
      SS(23)=F(1)*C(104)+F(2)*C(106)+F(3)*C(108)+F(4)*C(110)
400   SS(22)=F(1)*C(97)+F(2)*C(99)+F(3)*C(101)+F(4)
      SS(21)=F(1)*C(91)+F(2)*C(93)+F(3)*C(95)
      
500   if(Lbmax.EQ.1)then
      SS(17)=F(1)*C(65)+F(2)*C(67)+F(3)
      goto 700
      elseif(Lbmax.EQ.2)then
      goto 600
      elseif(Lbmax.NE.3)then
      
      SS(20)=F(1)*C(83)+F(2)*C(85)+F(3)*C(87)+F(4)*C(89)
      endif
      SS(19)=F(1)*C(76)+F(2)*C(78)+F(3)*C(80)+F(4)
600   SS(18)=F(1)*C(70)+F(2)*C(72)+F(3)*C(74)
      SS(17)=F(1)*C(65)+F(2)*C(67)+F(3)
      
700   if(Lbmax.EQ.1)then
      SS(13)=F(1)*C(43)+F(2)*C(45)
      goto 900
      elseif(Lbmax.EQ.2)then
      goto 800
      elseif(Lbmax.NE.3)then
      
      SS(16)=F(1)*C(58)+F(2)*C(60)+F(3)*C(62)+F(4)
      endif
      SS(15)=F(1)*C(52)+F(2)*C(54)+F(3)*C(56)
800   SS(14)=F(1)*C(47)+F(2)*C(49)+F(3)
      SS(13)=F(1)*C(43)+F(2)*C(45)
      
900   if(Lbmax.EQ.1)then
      SS(9)=F(1)*C(25)+F(2)
      goto 1100
      elseif(Lbmax.EQ.2)then
      goto 1000
      elseif(Lbmax.NE.3)then
      
      SS(12)=F(1)*C(37)+F(2)*C(39)+F(3)*C(41)
      endif
      SS(11)=F(1)*C(32)+F(2)*C(34)+F(3)
1000  SS(10)=F(1)*C(28)+F(2)*C(30)
      SS(9)=F(1)*C(25)+F(2)
      
1100  if(Lbmax.EQ.1)then
      SS(5)=F(1)*C(11)
      goto 1300
      elseif(Lbmax.EQ.2)then
      goto 1200
      elseif(Lbmax.NE.3)then
      
      SS(8)=F(1)*C(20)+F(2)*C(22)+F(3)
      endif
      SS(7)=F(1)*C(16)+F(2)*C(18)
1200  SS(6)=F(1)*C(13)+F(2)
      SS(5)=F(1)*C(11)
      
1300  if(Lbmax.EQ.1)goto 1500
      if(Lbmax.EQ.2)goto 1400
      
      if(Lbmax.NE.3)SS(4)=F(1)*C(7)+F(2)*C(9)
      SS(3)=F(1)*C(4)+F(2)
1400  SS(2)=F(1)*C(2)
1500  SS(1)=F(1)
      if(Idump.LT.4)return
      write(Iout,99001)
      call dout(SS,Lbmax,lanew)
      
99001 format('  GET2CS.')
99002 format(2x,8F12.5)
      
      return
      
      end
C* :1 * 
      
