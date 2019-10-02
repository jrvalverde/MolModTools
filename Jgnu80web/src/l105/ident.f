
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ident"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ident.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "ident.web"
      subroutine ident(A,NMAX,N)
      implicit none
      double precision A,one,zero
      integer i,ip1,j,N,nm1,NMAX
      dimension A(NMAX,NMAX)
      data zero/0.0D0/,one/1.0D0/
      
      
      
      
      
      
      
      nm1=N-1
      do 100 i=1,nm1
      A(i,i)=one
      ip1=i+1
      do 50 j=ip1,N
      A(i,j)=zero
50    continue
100   continue
      A(N,N)=one
      return
      
      end
C* :1 * 
      
