
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 l105"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "l105.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "l105.web"
      subroutine l105(NCHAIN)
      implicit none
      integer jump,NCHAIN,nextov
      
      call msopt(jump)
      NCHAIN=nextov(jump)
      return
      
      end
C* :1 * 
      
