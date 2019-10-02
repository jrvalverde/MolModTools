
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 wia4b"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "wia4b.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "wia4b.web"
      subroutine wia4b(IBUCK,CA,CB,NPAIRS,LPAIR,NBASIS)
      implicit none
      real CA,CB
      integer IBUCK,In,Iout,Ipunch,LPAIR,NBASIS,NPAIRS
      common/io/In,Iout,Ipunch
      
      
99001 format(' *** SINGLE-SUBSTITUTIONS NOT AVAILABLE ***')
      
      write(Iout,99001)
      call lnk1e
      return
      
      end
C* :1 * 
      
