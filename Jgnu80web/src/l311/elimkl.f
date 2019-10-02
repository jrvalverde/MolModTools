
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 elimkl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "elimkl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "elimkl.web"
      subroutine elimkl(IC)
      implicit none
      integer i,IC
      dimension IC(*)
      
      do 100 i=1,16
      IC(16*i-14)=0
      IC(16*i-13)=0
      IC(16*i-12)=0
      IC(16*i-9)=0
      IC(16*i-8)=0
      IC(16*i-4)=0
100   continue
      return
      
      end
C* :1 * 
      
