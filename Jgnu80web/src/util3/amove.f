
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 amove"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "amove.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "amove.web"
      subroutine amove(N,A,B)
      implicit none
      double precision A,B
      integer i,N
      
      
      dimension A(*),B(*)
      
      do 100 i=1,N
      B(i)=A(i)
100   continue
      
      return
      
      end
C* :1 * 
      
