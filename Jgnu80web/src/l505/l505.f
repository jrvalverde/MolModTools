
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 l505"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "l505.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "l505.web"
      subroutine l505(NCHAIN)
      implicit none
      integer jump,NCHAIN,nextov
      
      call rhfopn(jump)
      NCHAIN=nextov(jump)
      return
      
      end
C* :1 * 
      
