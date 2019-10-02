
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 linear"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "linear.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "linear.web"
      subroutine linear(A,B,MAX,N)
      implicit none
      double precision A,B
      integer i,j,k,MAX,N
      dimension A(MAX,MAX),B(*)
      
      
      
      
      k=1
      do 100 j=1,N
      do 50 i=1,j
      B(k)=A(i,j)
      k=k+1
50    continue
100   continue
      return
      
      end
C* :1 * 
      
