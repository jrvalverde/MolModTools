
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 zpolyr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "zpolyr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "zpolyr.web"
      subroutine zpolyr(A,NDEG,Z,ZZ,IER)
      implicit none
      real A,Z,ZZ
      integer IER,NDEG
      
      write(6,99001)
      
99001 format(' SORRY, ZPOLYR IS NOT IMPLEMENTED YET.')
      
      call lnk1e
      call killer
      stop
      
      end
C* :1 * 
      
