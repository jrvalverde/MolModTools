
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 s1s"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "s1s.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "s1s.web"
      subroutine s1s(EXX,CS,NGAUSS)
      implicit none
      double precision CS,EXX
      integer NGAUSS
      dimension EXX(6),CS(6)
      
      if(NGAUSS.EQ.2)then
      EXX(1)=8.518186635D-01
      CS(1)=4.301284983D-01
      EXX(2)=1.516232927D-01
      CS(2)=6.789135305D-01
      return
      elseif(NGAUSS.EQ.3)then
      EXX(1)=2.227660584D00
      CS(1)=1.543289673D-01
      EXX(2)=4.057711562D-01
      CS(2)=5.353281423D-01
      EXX(3)=1.098175104D-01
      CS(3)=4.446345422D-01
      return
      elseif(NGAUSS.EQ.4)then
      EXX(1)=5.216844534D00
      CS(1)=5.675242080D-02
      EXX(2)=9.546182760D-01
      CS(2)=2.601413550D-01
      EXX(3)=2.652034102D-01
      CS(3)=5.328461143D-01
      EXX(4)=8.801862774D-02
      CS(4)=2.916254405D-01
      return
      elseif(NGAUSS.EQ.5)then
      EXX(1)=1.130563696D01
      CS(1)=2.214055312D-02
      EXX(2)=2.071728178D00
      CS(2)=1.135411520D-01
      EXX(3)=5.786484833D-01
      CS(3)=3.318161484D-01
      EXX(4)=1.975724573D-01
      CS(4)=4.825700713D-01
      EXX(5)=7.445271746D-02
      CS(5)=1.935721966D-01
      return
      elseif(NGAUSS.NE.6)then
      EXX(1)=2.709498091D-01
      CS(1)=1.000000000D00
      return
      endif
      EXX(1)=2.310303149D01
      CS(1)=9.163596280D-03
      EXX(2)=4.235915534D00
      CS(2)=4.936149294D-02
      EXX(3)=1.185056519D00
      CS(3)=1.685383049D-01
      EXX(4)=4.070988982D-01
      CS(4)=3.705627997D-01
      EXX(5)=1.580884151D-01
      CS(5)=4.164915298D-01
      EXX(6)=6.510953954D-02
      CS(6)=1.303340841D-01
      return
      
      end
C* :1 * 
      
