
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 atdiag"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "atdiag.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "atdiag.web"
      subroutine atdiag(N,A,B,EVAL,C)
      implicit none
      double precision A,B,C,EVAL,one,temp,zero
      integer i,j,k,N
      
      
      
      dimension A(N,N),B(N,N),EVAL(N),C(N,N)
      data zero,one/0.0D0,1.0D0/
      call jacobi(N,B,EVAL,C,N,N,0)
      do 100 i=1,N
      EVAL(i)=one/sqrt(EVAL(i))
100   continue
      do 200 i=1,N
      do 150 j=1,i
      temp=zero
      do 120 k=1,N
      temp=temp+EVAL(k)*C(i,k)*C(j,k)
120   continue
      B(i,j)=temp
      B(j,i)=temp
150   continue
200   continue
      call simtrs(A,B,EVAL,N,N)
      call jacobi(N,A,EVAL,C,N,N,1)
      do 300 i=1,N
      do 250 j=1,N
      temp=zero
      do 220 k=1,N
      temp=temp+B(i,k)*C(k,j)
220   continue
      A(i,j)=temp
250   continue
300   continue
      call copy(A,C,N,N,N)
      return
      end
C* :1 * 
      
