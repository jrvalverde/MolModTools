
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 neon"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "neon.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "neon.web"
      subroutine neon(E1,E2,CS1,CS2,CP2,NGAUSS,NSPLIT)
      implicit none
      double precision CP2,CS1,CS2,E1,E2
      integer NGAUSS,NSPLIT
      dimension E1(6),E2(6),CS1(6),CS2(6),CP2(6)
      
      if(NGAUSS.EQ.4)then
      
      E1(1)=.139793208D+4
      CS1(1)=.174238054D-1
      E1(2)=.210769781D+3
      CS1(2)=.122272745D0
      E1(3)=.474672569D+2
      CS1(3)=.435014232D0
      E1(4)=.127226263D+2
      CS1(4)=.559714642D0
      E2(1)=.272130332D+2
      CS2(1)=-.109609439D0
      CP2(1)=.704403067D-1
      E2(2)=.629413435D+1
      CS2(2)=-.164124890D0
      CP2(2)=.343993047D0
      E2(3)=.176005125D+1
      CS2(3)=.114015159D+1
      CP2(3)=.724514960D0
      E2(4)=.461866992D0
      CS2(4)=1.0D0
      CP2(4)=1.0D0
      return
      elseif(NGAUSS.NE.6)then
      return
      endif
      
      E1(1)=8425.85153D0
      CS1(1)=0.00188434805D0
      E1(2)=1268.5194D0
      CS1(2)=0.0143368994D0
      E1(3)=289.621414D0
      CS1(3)=0.0701096233D0
      E1(4)=81.8590040D0
      CS1(4)=0.237373266D0
      E1(5)=26.2515079D0
      CS1(5)=0.473007126D0
      E1(6)=9.09472051D0
      CS1(6)=0.348401241D0
      E2(1)=26.532131D0
      CS2(1)=-0.107118287D0
      CP2(1)=0.0719095885D0
      E2(2)=6.10175501D0
      CS2(2)=-0.146163821D0
      CP2(2)=0.349513372D0
      E2(3)=1.69627153D0
      CS2(3)=1.12777350D0
      CP2(3)=0.719940512D0
      E2(4)=0.445818700D0
      CS2(4)=1.0D0
      CP2(4)=1.0D0
      return
      
      end
C* :1 * 
      
