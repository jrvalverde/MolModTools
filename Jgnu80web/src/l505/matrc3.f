
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 matrc3"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "matrc3.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "matrc3.web"
      subroutine matrc3(A,B,C,L,M,N,MODE)
      implicit none
      double precision A,acc,B,C,zero
      integer i,j,k,L,M,MODE,N
      dimension A(70,70),B(70,70),C(70,70)
      data zero/0.0D0/
      
      
      
      
      
      
      
      
      
      if(MODE.LT.2)then
      
      do 50 i=1,L
      do 20 j=1,N
      acc=zero
      do 10 k=1,M
      acc=acc+(A(i,k)*B(k,j))
10    continue
      C(i,j)=acc
20    continue
50    continue
      elseif(MODE.EQ.2)then
      
      do 100 i=1,L
      do 80 j=1,N
      acc=zero
      do 60 k=1,M
      acc=acc+(A(k,i)*B(k,j))
60    continue
      C(i,j)=acc
80    continue
100   continue
      else
      
      do 150 i=1,L
      do 120 j=1,N
      acc=zero
      do 110 k=1,M
      acc=acc+(A(i,k)*B(j,k))
110   continue
      C(i,j)=acc
120   continue
150   continue
      endif
      
      return
      
      end
C* :1 * 
      
