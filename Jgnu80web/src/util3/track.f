
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 track"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "track.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "track.web"
      subroutine track(MSG)
      implicit none
      integer In,Iout,Iprint,Ipunch
      character*1 MSG(6)
      common/print/Iprint
      common/io/In,Iout,Ipunch
      
      
      
      
      
      
99001 format(/1x,6A1,':')
      
      if(Iprint.GE.2)write(Iout,99001)MSG
      
      return
      
      end
C* :1 * 
      
