
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 s3p"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "s3p.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "s3p.web"
      subroutine s3p(EXX,CS,NGAUSS)
      implicit none
      double precision CS,EXX
      integer NGAUSS
      dimension EXX(6),CS(6)
      
      if(NGAUSS.EQ.2)then
      EXX(1)=1.458620964D-01
      CS(1)=5.349653144D-01
      EXX(2)=5.664210742D-02
      CS(2)=5.299607212D-01
      return
      elseif(NGAUSS.EQ.3)then
      EXX(1)=2.692880368D00
      CS(1)=-1.061945788D-02
      EXX(2)=1.489359592D-01
      CS(2)=5.218564264D-01
      EXX(3)=5.739585040D-02
      CS(3)=5.450015143D-01
      return
      elseif(NGAUSS.EQ.4)then
      EXX(1)=1.853180239D00
      CS(1)=-1.434249391D-02
      EXX(2)=1.915075719D-01
      CS(2)=2.755177589D-01
      EXX(3)=8.655487938D-02
      CS(3)=5.846750879D-01
      EXX(4)=4.184253862D-02
      CS(4)=2.144986514D-01
      return
      elseif(NGAUSS.EQ.5)then
      EXX(1)=6.466803859D00
      CS(1)=-2.329023747D-03
      EXX(2)=1.555914802D00
      CS(2)=-1.357395221D-02
      EXX(3)=1.955925255D-01
      CS(3)=2.632185383D-01
      EXX(4)=8.809647701D-02
      CS(4)=5.880427024D-01
      EXX(5)=4.234835707D-02
      CS(5)=2.242794445D-01
      return
      elseif(NGAUSS.NE.6)then
      EXX(1)=9.113614253D-02
      CS(1)=1.000000000D00
      return
      endif
      EXX(1)=5.077973607D00
      CS(1)=-3.329929840D-03
      EXX(2)=1.340786940D00
      CS(2)=-1.419488340D-02
      EXX(3)=2.248434849D-01
      CS(3)=1.639395770D-01
      EXX(4)=1.131741848D-01
      CS(4)=4.485358256D-01
      EXX(5)=6.076408893D-02
      CS(5)=3.908813050D-01
      EXX(6)=3.315424265D-02
      CS(6)=7.411456232D-02
      return
      
      end
C* :1 * 
      
