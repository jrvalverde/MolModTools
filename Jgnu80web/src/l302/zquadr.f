
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 zquadr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "zquadr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "zquadr.web"
      subroutine zquadr(AA,BB,CC,R1,R2,IRC,IER)
      implicit none
      real AA,BB,CC,R1,R2
      integer IER,IRC
      
      write(6,99001)
      
99001 format(' SORRY, ZQUADR IS NOT IMPLEMENTED YET.')
      
      call lnk1e
      call killer
      stop
      
      end
C* :1 * 
      
