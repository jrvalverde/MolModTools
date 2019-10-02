
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 matrec"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "matrec.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "matrec.web"
      subroutine matrec(A,B,D,MAXDIM,L,M,N,MODE)
      implicit none
      double precision A,B,D,sum,swap,zero
      integer i,ip1,j,k,L,M,MAXDIM,mn,MODE,mx,N
      dimension A(MAXDIM,MAXDIM),B(MAXDIM,MAXDIM),D(MAXDIM)
      equivalence(swap,sum)
      data zero/0.0D0/
      
      mn=L
      mx=L
      if(L.LT.N)then
      mx=N
      elseif(L.NE.N)then
      
      mn=N
      endif
      if(MODE.EQ.2)then
      
      do 50 i=1,L
      do 20 j=1,N
      sum=zero
      do 10 k=1,M
      sum=sum+(A(k,i)*B(k,j))
10    continue
      D(j)=sum
20    continue
      do 40 j=1,N
      A(j,i)=D(j)
40    continue
50    continue
      do 100 i=1,mn
      ip1=i+1
      if(ip1.GT.mx)goto 600
      do 60 j=ip1,mx
      swap=A(i,j)
      A(i,j)=A(j,i)
      A(j,i)=swap
60    continue
100   continue
      elseif(MODE.EQ.3)then
      
      do 150 i=1,L
      do 120 j=1,N
      sum=zero
      do 110 k=1,M
      sum=sum+(A(i,k)*B(j,k))
110   continue
      D(j)=sum
120   continue
      do 140 j=1,N
      A(i,j)=D(j)
140   continue
150   continue
      elseif(MODE.EQ.4)then
      
      do 250 j=1,N
      do 180 i=1,L
      sum=zero
      do 160 k=1,M
      sum=sum+(A(i,k)*B(k,j))
160   continue
      D(i)=sum
180   continue
      do 200 i=1,L
      B(i,j)=D(i)
200   continue
250   continue
      elseif(MODE.EQ.5)then
      
      do 350 j=1,N
      do 280 i=1,L
      sum=zero
      do 260 k=1,M
      sum=sum+(A(k,i)*B(k,j))
260   continue
      D(i)=sum
280   continue
      do 300 i=1,L
      B(i,j)=D(i)
300   continue
350   continue
      elseif(MODE.EQ.6)then
      
      do 450 j=1,N
      do 380 i=1,L
      sum=zero
      do 360 k=1,M
      sum=sum+(A(i,k)*B(j,k))
360   continue
      D(i)=sum
380   continue
      do 400 i=1,L
      B(j,i)=D(i)
400   continue
450   continue
      do 500 i=1,mn
      ip1=i+1
      if(ip1.GT.mx)goto 600
      do 460 j=ip1,mx
      swap=B(i,j)
      B(i,j)=B(j,i)
      B(j,i)=swap
460   continue
500   continue
      else
      
      do 550 i=1,L
      do 520 j=1,N
      sum=zero
      do 510 k=1,M
      sum=sum+(A(i,k)*B(k,j))
510   continue
      D(j)=sum
520   continue
      do 540 j=1,N
      A(i,j)=D(j)
540   continue
550   continue
      endif
600   return
      
      end
C* :1 * 
      
