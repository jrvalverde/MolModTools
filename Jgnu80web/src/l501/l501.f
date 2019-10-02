
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 l501"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "l501.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "l501.web"
      subroutine l501(NCHAIN)
      implicit none
      integer jump,NCHAIN,nextov
      
      call rhfclo(jump)
      NCHAIN=nextov(jump)
      return
      
      end
C* :1 * 
      
