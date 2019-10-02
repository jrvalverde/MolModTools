
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 l913"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "l913.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "l913.web"
      subroutine l913(NCHAIN)
      implicit none
      integer jump,NCHAIN,nextov
      
      call cids5(jump)
      NCHAIN=nextov(jump)
      return
      
      end
C* :1 * 
      
