
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 s4d"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "s4d.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "s4d.web"
      subroutine s4d(EXX,CS,NGAUSS)
      implicit none
      real*8 CS,EXX
      integer NGAUSS
      dimension EXX(6),CS(6)
      
      if(NGAUSS.EQ.2)then
      EXX(1)=1.330958892D-01
      CS(1)=4.932764167D-01
      EXX(2)=5.272119659D-02
      CS(2)=5.918727866D-01
      return
      elseif(NGAUSS.EQ.3)then
      EXX(1)=1.777717219D-01
      CS(1)=2.308552718D-01
      EXX(2)=8.040647350D-02
      CS(2)=6.042409177D-01
      EXX(3)=3.949855551D-02
      CS(3)=2.595768926D-01
      return
      elseif(NGAUSS.EQ.4)then
      EXX(1)=1.995825422D-00
      CS(1)=-2.816702620D-03
      EXX(2)=1.82346128D-01
      CS(2)=2.177095871D-01
      EXX(3)=8.197240896D-02
      CS(3)=6.058047348D-01
      EXX(4)=4.000634951D-02
      CS(4)=2.717811257D-01
      return
      elseif(NGAUSS.EQ.5)then
      EXX(1)=1.522122079D00
      CS(1)=-3.673711876D-03
      EXX(2)=2.173041823D-01
      CS(2)=1.167122499D-01
      EXX(3)=1.084876577D-01
      CS(3)=4.216476416D-01
      EXX(4)=5.836797641D-02
      CS(4)=4.547673415D-01
      EXX(5)=3.206682246D-02
      CS(5)=1.037803318D-01
      return
      elseif(NGAUSS.NE.6)then
      EXX(1)=7.941656339D-02
      CS(1)=1.000000000E00
      return
      endif
      EXX(1)=4.634239420D-00
      CS(1)=-4.749842876D-04
      EXX(2)=1.341648295D-00
      CS(2)=-3.566777891D-03
      EXX(3)=2.209593028D-01
      CS(3)=1.108670481D-01
      EXX(4)=1.101467943D-01
      CS(4)=4.159646930D-01
      EXX(5)=5.904190370D-02
      CS(5)=4.621672517D-01
      EXX(6)=3.232628887D-02
      CS(6)=1.081250196D-01
      return
      end
C* :1 * 
      
