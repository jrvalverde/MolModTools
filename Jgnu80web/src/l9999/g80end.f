
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 g80end"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "g80end.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "g80end.web"
      subroutine g80end
      implicit none
      integer*4 In,Iout,Ipunch
      common/io/In,Iout,Ipunch
      
      close(unit=3)
      write(Iout,99001)
      return
      
99001 format(/,' **** End of gnu80 Job Step *****')
      
      end
C* :1 * 
      
