
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fee0"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fee0.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "fee0.web"
      subroutine fee0(EDEL,ETOT)
      implicit none
      double precision EDEL,ETOT,x
      integer nfile
      dimension x(2)
      
      
      nfile=8
      call nbread(x,2,nfile)
      EDEL=x(1)
      ETOT=x(2)
      return
      end
C* :1 * 
      
