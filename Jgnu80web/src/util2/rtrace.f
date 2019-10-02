
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rtrace"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rtrace.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "rtrace.web"
      subroutine rtrace(NAME,TRLEVL)
      implicit none
      integer In,Iop,Iout,Ipunch,TRLEVL
      character*6 NAME
      common/iop/Iop(50)
      common/io/In,Iout,Ipunch
      
99001 format(9H TRACE:  ,a6)
      
      if(Iop(34).GE.TRLEVL)write(Iout,99001)NAME
      return
      
      end
C* :1 * 
      
