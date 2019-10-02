
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 square"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "square.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "square.web"
      subroutine square(A,B,MAX,N,KEY)
      implicit none
      double precision A,B
      integer i,ix,j,jx,k,KEY,MAX,N
      dimension A(*),B(MAX,MAX)
      
      
      
      
      if(KEY.NE.0)then
      
      
      k=N*N
      do 50 j=1,N
      jx=N-j+1
      do 20 i=1,N
      ix=N-i+1
      B(ix,jx)=A(k)
      k=k-1
20    continue
50    continue
      return
      endif
      
      
      k=N*(N+1)/2
      do 200 j=1,N
      jx=N-j+1
      do 100 i=1,jx
      ix=jx-i+1
      B(ix,jx)=A(k)
      k=k-1
100   continue
200   continue
      do 300 j=1,N
      do 250 i=1,j
      B(j,i)=B(i,j)
250   continue
300   continue
      return
      
      end
C* :1 * 
      
