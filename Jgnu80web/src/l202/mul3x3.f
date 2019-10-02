
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 mul3x3"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "mul3x3.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "mul3x3.web"
      subroutine mul3x3(A,B,C)
      implicit none
      integer i,j,k
      double precision A(3,3),B(3,3),C(3,3),zero
      data zero/0.0D0/
      
      do 100 i=1,3
      do 50 j=1,3
      C(i,j)=zero
      do 20 k=1,3
      C(i,j)=C(i,j)+A(i,k)*B(k,j)
20    continue
50    continue
100   continue
      return
      
      end
C* :1 * 
      
