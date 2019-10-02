
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gdot"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gdot.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "gdot.web"
      subroutine gdot(AR,AI,BR,BI,ANSR,ANSI,ICMP,NBASIS)
      implicit none
      double precision AI,ANSI,ANSR,AR,BI,BR,zero
      integer i,ICMP,NBASIS
      dimension AR(NBASIS),AI(NBASIS),BR(NBASIS),BI(NBASIS)
      data zero/0.0D0/
      
      
      
      ANSR=zero
      ANSI=zero
      
      do 100 i=1,NBASIS
      ANSR=ANSR+AR(i)*BR(i)
      if(ICMP.NE.0)then
      ANSR=ANSR+AI(i)*BI(i)
      ANSI=ANSI+AR(i)*BI(i)-BR(i)*AI(i)
      endif
100   continue
      return
      
      end
C* :1 * 
      
