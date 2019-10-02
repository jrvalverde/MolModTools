
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 s4spd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "s4spd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "s4spd.web"
      subroutine s4spd(EXX,CS,CP,CD,NGAUSS)
      implicit none
      real*8 CD,CP,CS,EXX
      integer NGAUSS
      dimension EXX(6),CS(6),CP(6),CD(6)
      
      if(NGAUSS.EQ.2)then
      elseif(NGAUSS.EQ.3)then
      EXX(1)=2.334859142D-01
      CS(1)=-3.306100626D-01
      CP(1)=-1.283927634D-01
      CD(1)=1.250662138D-01
      EXX(2)=9.091820476D-02
      CS(2)=5.761095337D-02
      CP(2)=5.852047641D-01
      CD(2)=6.686785577D-01
      EXX(3)=4.002241756D-02
      CS(3)=1.115578745D+00
      CP(3)=5.439442040D-01
      CD(3)=3.052468245D-01
      return
      elseif(NGAUSS.EQ.4)then
      EXX(1)=4.504253317D-01
      CS(1)=-5.846878904D-02
      CP(1)=-6.746634037D-02
      CD(1)=3.869942210D-03
      EXX(2)=1.607926601D-01
      CS(2)=-4.398036149D-01
      CP(2)=2.077927908D-02
      CD(2)=2.976123998D-01
      EXX(3)=7.163477033D-02
      CS(3)=5.319219322D-01
      CP(3)=6.884538916D-01
      CD(3)=6.203734570D-01
      EXX(4)=3.511480147D-02
      CS(4)=8.107467607D-01
      CP(4)=3.514817260D-01
      CD(4)=1.735534933D-01
      return
      elseif(NGAUSS.EQ.5)then
      EXX(1)=8.147330480D-01
      CS(1)=-1.625226415D-03
      CP(1)=-2.252723379D-02
      CD(1)=-7.366292015D-03
      EXX(2)=2.730428991D-01
      CS(2)=-2.021421832D-01
      CP(2)=-8.330806344D-02
      CD(2)=6.959758544D-02
      EXX(3)=1.196699932D-01
      CS(3)=-3.157024793D-01
      CP(3)=2.024706622D-01
      CD(3)=4.238047632D-01
      EXX(4)=5.981039858D-02
      CS(4)=8.082100531D-01
      CP(4)=6.764313752D-01
      CD(4)=5.073954645D-01
      EXX(5)=3.146521928D-02
      CS(5)=5.572172111D-01
      CP(5)=2.185970413D-01
      CD(5)=1.003613116D-01
      return
      elseif(NGAUSS.EQ.6)then
      goto 100
      else
      call lnk1e
      EXX(1)=7.163507065D-02
      CS(1)=1.000000000D00
      CP(1)=1.000000000D00
      endif
      EXX(1)=1.260113461D-01
      CS(1)=-6.561972117D-01
      CP(1)=1.270081424D-01
      CD(1)=5.432745552D-01
      EXX(2)=5.009011045D-02
      CS(2)=1.503538305D+00
      CP(2)=8.985325121D-01
      CD(2)=5.420870304D-01
      return
100   EXX(1)=1.374339986D+00
      CS(1)=3.713770150D-03
      CP(1)=-6.877310609D-03
      CD(1)=-4.346377641D-03
      EXX(2)=4.465159588D-01
      CS(2)=-5.212196567D-02
      CP(2)=-5.085106480D-02
      CD(2)=4.891225526D-03
      EXX(3)=1.924506869D-01
      CS(3)=-3.077625125D-01
      CP(3)=-4.206448162D-02
      CD(3)=1.634802602D-01
      EXX(4)=9.582818257D-02
      CS(4)=-6.451244293D-02
      CP(4)=3.652395431D-01
      CD(4)=4.807480840D-01
      EXX(5)=5.166421619D-02
      CS(5)=8.980784431D-01
      CP(5)=5.915908715D-01
      CD(5)=3.906920727D-01
      EXX(6)=2.858944822D-02
      CS(6)=3.690956849D-01
      CP(6)=1.344127135D-01
      CD(6)=5.786627339D-02
      return
      end
C* :1 * 
      
