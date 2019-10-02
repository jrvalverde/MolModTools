
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 iclear"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "iclear.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "iclear.web"
      subroutine iclear(N,IA)
      implicit none
      integer i,IA,N
      dimension IA(*)
      
      
      
      do 100 i=1,N
      IA(i)=0
100   continue
      return
      
      end
C* :1 * 
      
