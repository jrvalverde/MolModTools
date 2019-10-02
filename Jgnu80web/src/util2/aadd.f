
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 aadd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "aadd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "aadd.web"
      subroutine aadd(N,A,B,C)
      implicit none
      double precision A,B,C
      integer i,N
      
      
      dimension A(*),B(*),C(*)
      
      do 100 i=1,N
      C(i)=A(i)+B(i)
100   continue
      
      return
      
      end
C* :1 * 
      
