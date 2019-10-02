
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 s3d"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "s3d.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "s3d.web"
      subroutine s3d(EXX,CS,NGAUSS)
      implicit none
      double precision CS,EXX
      integer NGAUSS
      dimension EXX(6),CS(6)
      
      if(NGAUSS.EQ.2)then
      EXX(1)=2.777427345D-01
      CS(1)=4.666137923D-01
      EXX(2)=8.336507714D-02
      CS(2)=6.644706516D-01
      return
      elseif(NGAUSS.EQ.3)then
      EXX(1)=5.229112225D-01
      CS(1)=1.686596060D-01
      EXX(2)=1.639595876D-01
      CS(2)=5.847984817D-01
      EXX(3)=6.386630021D-02
      CS(3)=4.056779523D-01
      return
      elseif(NGAUSS.EQ.4)then
      EXX(1)=9.185846715D-01
      CS(1)=5.799057705D-02
      EXX(2)=2.920461109D-01
      CS(2)=3.045581349D-01
      EXX(3)=1.187568890D-01
      CS(3)=5.601358038D-01
      EXX(4)=5.286755896D-02
      CS(4)=2.432423313D-01
      return
      elseif(NGAUSS.EQ.5)then
      EXX(1)=1.539033958D00
      CS(1)=2.020869128D-02
      EXX(2)=4.922090297D-01
      CS(2)=1.321157923D-01
      EXX(3)=2.029756928D-01
      CS(3)=3.911240346D-01
      EXX(4)=9.424112917D-02
      CS(4)=4.779609701D-01
      EXX(5)=4.569058269D-02
      CS(5)=1.463662294D-01
      return
      elseif(NGAUSS.NE.6)then
      EXX(1)=1.302270363D-01
      CS(1)=1.000000000D00
      return
      endif
      EXX(1)=2.488296923D00
      CS(1)=7.283828112D-03
      EXX(2)=7.981487853D-01
      CS(2)=5.386799363D-02
      EXX(3)=3.311327490D-01
      CS(3)=2.072139149D-01
      EXX(4)=1.559114463D-01
      CS(4)=4.266269092D-01
      EXX(5)=7.877734732D-02
      CS(5)=3.843100204D-01
      EXX(6)=4.058484363D-02
      CS(6)=8.902827546D-02
      return
      
      end
C* :1 * 
      
