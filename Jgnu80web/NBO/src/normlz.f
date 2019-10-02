
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 normlz"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "normlz.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "normlz.web"
      subroutine normlz(A,S,M,N)
      implicit none
      double precision A,factor,one,S,temp,zero
      integer i,j,k,M,N
      dimension A(M,M),S(M,M)
      
      data zero,one/0.0D0,1.0D0/
      
      
      do 200 i=1,N
      temp=zero
      do 50 j=1,N
      do 20 k=1,N
      temp=temp+A(j,i)*A(k,i)*S(j,k)
20    continue
50    continue
      factor=one/dsqrt(temp)
      do 100 j=1,N
      A(j,i)=factor*A(j,i)
100   continue
200   continue
      return
      end
C* :1 * 
      
