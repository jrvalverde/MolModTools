
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 s2p"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "s2p.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "s2p.web"
      subroutine s2p(EXX,CS,NGAUSS)
      implicit none
      double precision CS,EXX
      integer NGAUSS
      dimension EXX(6),CS(6)
      
      if(NGAUSS.EQ.2)then
      EXX(1)=4.323908358D-01
      CS(1)=4.522627513D-01
      EXX(2)=1.069439065D-01
      CS(2)=6.713122642D-01
      return
      elseif(NGAUSS.EQ.3)then
      EXX(1)=9.192379002D-01
      CS(1)=1.623948553D-01
      EXX(2)=2.359194503D-01
      CS(2)=5.661708862D-01
      EXX(3)=8.009805746D-02
      CS(3)=4.223071752D-01
      return
      elseif(NGAUSS.EQ.4)then
      EXX(1)=1.798260992D00
      CS(1)=5.713170255D-02
      EXX(2)=4.662622228D-01
      CS(2)=2.857455515D-01
      EXX(3)=1.643718620D-01
      CS(3)=5.517873105D-01
      EXX(4)=6.543927065D-02
      CS(4)=2.632314924D-01
      return
      elseif(NGAUSS.EQ.5)then
      EXX(1)=3.320386533D00
      CS(1)=2.079051117D-02
      EXX(2)=8.643257633D-01
      CS(2)=1.235472099D-01
      EXX(3)=3.079819284D-01
      CS(3)=3.667738886D-01
      EXX(4)=1.273309895D-01
      CS(4)=4.834930290D-01
      EXX(5)=5.606243164D-02
      CS(5)=1.653444074D-01
      return
      elseif(NGAUSS.NE.6)then
      EXX(1)=1.759666885D-01
      CS(1)=1.000000000D00
      return
      endif
      EXX(1)=5.868285913D00
      CS(1)=7.924233646D-03
      EXX(2)=1.530329631D00
      CS(2)=5.144104825D-02
      EXX(3)=5.475665231D-01
      CS(3)=1.898400060D-01
      EXX(4)=2.288932733D-01
      CS(4)=4.049863191D-01
      EXX(5)=1.046655969D-01
      CS(5)=4.012362861D-01
      EXX(6)=4.948220127D-02
      CS(6)=1.051855189D-01
      return
      
      end
C* :1 * 
      
