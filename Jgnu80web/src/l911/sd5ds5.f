
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sd5ds5"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sd5ds5.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "sd5ds5.web"
      subroutine sd5ds5(DAVAIL,SAVAIL)
      implicit none
      real DAVAIL,SAVAIL
      integer In,Iout,Ipunch
      common/io/In,Iout,Ipunch
      
      
99001 format(' *** SINGLE-SUBSTITUTIONS NOT AVAILABLE ***')
      
      write(Iout,99001)
      call lnk1e
      return
      
      end
C* :1 * 
      
