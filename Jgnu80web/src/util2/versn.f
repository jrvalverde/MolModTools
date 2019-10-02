
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 versn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "versn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "versn.web"
      subroutine versn
      implicit none
      integer In,Iout,Ipunch
      common/io/In,Iout,Ipunch
      
      
99001 format(1x,43(1H*)/1x,'    gnu80: PORTABLE GAUSSIAN SYSTEM',/,1x,43
     &(1H*))
      
      write(Iout,99001)
      return
      
      end
C* :1 * 
      
