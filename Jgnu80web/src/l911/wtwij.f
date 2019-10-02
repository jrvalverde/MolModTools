
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 wtwij"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "wtwij.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "wtwij.web"
      subroutine wtwij(CI,CJ,WTWIDJ,WTWIDI,WTILDA,NBASIS)
      implicit none
      real CI,CJ,WTILDA,WTWIDI,WTWIDJ
      integer In,Iout,Ipunch,NBASIS
      common/io/In,Iout,Ipunch
      
      
99001 format(' *** SINGLE-SUBSTITUTIONS NOT AVAILABLE ***')
      
      write(Iout,99001)
      call lnk1e
      return
      
      end
C* :1 * 
      
