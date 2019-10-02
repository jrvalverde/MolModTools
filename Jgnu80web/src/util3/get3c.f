
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 get3c"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "get3c.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "get3c.web"
      subroutine get3c(V,F,C)
      implicit none
      double precision C,F,V
      integer Idummy,Idump,In,Iout,Ipunch,Lamax,Lbmax,Lpmax,Maxdum
      dimension V(16),F(8),C(70)
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/dump/Idump,Idummy
      common/io/In,Iout,Ipunch
      
      
      
      
      
      if(Lamax.EQ.1)goto 500
      if(Lamax.EQ.2)goto 300
      if(Lamax.NE.3)then
      
      if(Lbmax.EQ.1)then
      V(13)=F(1)*C(43)+F(2)*C(44)+F(3)*C(45)+F(4)
      goto 100
      elseif(Lbmax.EQ.2)then
      goto 50
      elseif(Lbmax.NE.3)then
      
      V(16)=F(1)*C(58)+F(2)*C(59)+F(3)*C(60)+F(4)*C(61)+F(5)*C(62)+F(6)*
     &C(63)+F(7)
      endif
      V(15)=F(1)*C(52)+F(2)*C(53)+F(3)*C(54)+F(4)*C(55)+F(5)*C(56)+F(6)
50    V(14)=F(1)*C(47)+F(2)*C(48)+F(3)*C(49)+F(4)*C(50)+F(5)
      V(13)=F(1)*C(43)+F(2)*C(44)+F(3)*C(45)+F(4)
      endif
      
100   if(Lbmax.EQ.1)then
      V(9)=F(1)*C(25)+F(2)*C(26)+F(3)
      goto 300
      elseif(Lbmax.EQ.2)then
      goto 200
      elseif(Lbmax.NE.3)then
      
      V(12)=F(1)*C(37)+F(2)*C(38)+F(3)*C(39)+F(4)*C(40)+F(5)*C(41)+F(6)
      endif
      V(11)=F(1)*C(32)+F(2)*C(33)+F(3)*C(34)+F(4)*C(35)+F(5)
200   V(10)=F(1)*C(28)+F(2)*C(29)+F(3)*C(30)+F(4)
      V(9)=F(1)*C(25)+F(2)*C(26)+F(3)
      
300   if(Lbmax.EQ.1)then
      V(5)=F(1)*C(11)+F(2)
      goto 500
      elseif(Lbmax.EQ.2)then
      goto 400
      elseif(Lbmax.NE.3)then
      
      V(8)=F(1)*C(20)+F(2)*C(21)+F(3)*C(22)+F(4)*C(23)+F(5)
      endif
      V(7)=F(1)*C(16)+F(2)*C(17)+F(3)*C(18)+F(4)
400   V(6)=F(1)*C(13)+F(2)*C(14)+F(3)
      V(5)=F(1)*C(11)+F(2)
      
500   if(Lbmax.EQ.1)goto 700
      if(Lbmax.EQ.2)goto 600
      
      if(Lbmax.NE.3)V(4)=F(1)*C(7)+F(2)*C(8)+F(3)*C(9)+F(4)
      V(3)=F(1)*C(4)+F(2)*C(5)+F(3)
600   V(2)=F(1)*C(2)+F(2)
700   V(1)=F(1)
      if(Idump.LE.1)return
      write(Iout,99001)
      
99001 format('  GET3C.  THREE-CENTER INTS.')
      
      call dout(V,Lbmax,Lamax)
      return
      
      end
C* :1 * 
      
