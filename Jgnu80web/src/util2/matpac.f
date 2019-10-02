
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 matpac"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "matpac.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "matpac.web"
      subroutine matpac(A,B,C,MAX,NX,IQ)
      implicit none
      double precision A,B,C,zero
      integer i,IQ,j,m,MAX,NX
      dimension A(MAX,MAX),B(MAX,MAX),C(MAX,MAX)
      data zero/0.0D0/
      
      
      
      
      
      if(IQ.EQ.2)then
      
      
      do 50 i=1,NX
      do 20 j=1,NX
      C(i,j)=zero
      do 10 m=1,NX
      C(i,j)=C(i,j)+A(m,i)*B(m,j)
10    continue
20    continue
50    continue
      return
      elseif(IQ.NE.3)then
      
      
      do 100 i=1,NX
      do 80 j=1,NX
      C(i,j)=zero
      do 60 m=1,NX
      C(i,j)=C(i,j)+A(i,m)*B(m,j)
60    continue
80    continue
100   continue
      return
      endif
      
      
      do 200 i=1,NX
      do 150 j=1,NX
      C(i,j)=zero
      do 120 m=1,NX
      C(i,j)=C(i,j)+A(i,m)*B(j,m)
120   continue
150   continue
200   continue
      return
      
      end
C* :1 * 
      
