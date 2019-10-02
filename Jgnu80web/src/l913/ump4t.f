
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ump4t"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ump4t.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "ump4t.web"
      subroutine ump4t(EMP4T)
      implicit none
      real EMP4T
      integer In,Iout,Ipunch
      common/io/In,Iout,Ipunch
      
      
99001 format(' *** SINGLE-SUBSTITUTIONS NOT AVAILABLE ***')
      
      write(Iout,99001)
      call lnk1e
      return
      
      end
C* :1 * 
      
