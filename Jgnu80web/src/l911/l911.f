
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 l911"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "l911.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "l911.web"
      subroutine l911(NCHAIN)
      implicit none
      integer jump,NCHAIN,nextov
      call cids3(jump)
      NCHAIN=nextov(jump)
      return
      
      end
C* :1 * 
      
