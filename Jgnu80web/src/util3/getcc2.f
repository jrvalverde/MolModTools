
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getcc2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getcc2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "getcc2.web"
      subroutine getcc2(CC,A,B,L1MAX,L2MAX)
      implicit none
      double precision A,B,CC
      integer L1MAX,L2MAX
      dimension CC(48)
      
      
      
      if(L2MAX.GT.1)then
      
      CC(1)=B
      
      if(L2MAX.GT.2)then
      
      CC(2)=B*CC(1)
      CC(3)=B+CC(1)
      
      if(L2MAX.GT.3)then
      
      CC(4)=B*CC(2)
      CC(5)=B*CC(3)+CC(2)
      CC(6)=B+CC(3)
      endif
      endif
      endif
      
      if(L1MAX.GT.1)then
      
      CC(7)=A
      
      if(L2MAX.GT.1)then
      
      CC(8)=A*CC(1)
      CC(9)=A+CC(1)
      
      if(L2MAX.GT.2)then
      
      CC(10)=A*CC(2)
      CC(11)=A*CC(3)+CC(2)
      CC(12)=A+CC(3)
      
      if(L2MAX.GT.3)then
      
      CC(13)=A*CC(4)
      CC(14)=A*CC(5)+CC(4)
      CC(15)=A*CC(6)+CC(5)
      CC(16)=A+CC(6)
      endif
      endif
      endif
      
      if(L1MAX.GT.2)then
      
      CC(17)=A*CC(7)
      CC(18)=A+CC(7)
      
      if(L2MAX.GT.1)then
      
      CC(19)=A*CC(8)
      CC(20)=A*CC(9)+CC(8)
      CC(21)=A+CC(9)
      
      if(L2MAX.GT.2)then
      
      CC(22)=A*CC(10)
      CC(23)=A*CC(11)+CC(10)
      CC(24)=A*CC(12)+CC(11)
      CC(25)=A+CC(12)
      
      if(L2MAX.GT.3)then
      
      CC(26)=A*CC(13)
      CC(27)=A*CC(14)+CC(13)
      CC(28)=A*CC(15)+CC(14)
      CC(29)=A*CC(16)+CC(15)
      CC(30)=A+CC(16)
      endif
      endif
      endif
      
      if(L1MAX.GT.3)then
      
      CC(31)=A*CC(17)
      CC(32)=A*CC(18)+CC(17)
      CC(33)=A+CC(18)
      
      if(L2MAX.GT.1)then
      
      CC(34)=A*CC(19)
      CC(35)=A*CC(20)+CC(19)
      CC(36)=A*CC(21)+CC(20)
      CC(37)=A+CC(21)
      
      if(L2MAX.GT.2)then
      
      CC(38)=A*CC(22)
      CC(39)=A*CC(23)+CC(22)
      CC(40)=A*CC(24)+CC(23)
      CC(41)=A*CC(25)+CC(24)
      CC(42)=A+CC(25)
      
      if(L2MAX.GT.3)then
      
      CC(43)=A*CC(26)
      CC(44)=A*CC(27)+CC(26)
      CC(45)=A*CC(28)+CC(27)
      CC(46)=A*CC(29)+CC(28)
      CC(47)=A*CC(30)+CC(29)
      CC(48)=A+CC(30)
      endif
      endif
      endif
      endif
      endif
      endif
      
      return
      
      end
C* :1 * 
      
