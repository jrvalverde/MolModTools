
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 wia4a"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "wia4a.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "wia4a.web"
      subroutine wia4a(IBUCK,ISPIN,NO,NV,NPAIRS,LPAIR,NBASIS)
      implicit none
      integer IBUCK,In,Iout,Ipunch,ISPIN,LPAIR,NBASIS,NO,NPAIRS,NV
      common/io/In,Iout,Ipunch
      
      
99001 format(' *** SINGLE-SUBSTITUTIONS NOT AVAILABLE ***')
      
      write(Iout,99001)
      call lnk1e
      return
      
      end
C* :1 * 
      
