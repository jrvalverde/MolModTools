
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 trnspo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "trnspo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "trnspo.web"
      subroutine trnspo(A,NDIM,N)
      implicit none
      double precision A,temp
      integer i,j,N,NDIM
      dimension A(NDIM,NDIM)
      
      
      do 100 i=1,N
      do 50 j=1,i
      temp=A(i,j)
      A(i,j)=A(j,i)
      A(j,i)=temp
50    continue
100   continue
      return
      end
C* :1 * 
      
