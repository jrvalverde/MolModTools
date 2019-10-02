
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 copy"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "copy.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "copy.web"
      subroutine copy(A,B,NDIM,NR,NC)
      implicit none
      double precision A,B
      integer i,j,NC,NDIM,NR
      dimension A(NDIM,1),B(NDIM,1)
      
      
      do 100 j=1,NC
      do 50 i=1,NR
      B(i,j)=A(i,j)
50    continue
100   continue
      return
      end
C* :1 * 
      
