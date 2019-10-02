
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 subst"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "subst.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "subst.web"
      subroutine subst(X,W,B,IPIVOT,N,NDIM)
      implicit none
      double precision B,sum,W,X,zero
      integer i,ip,IPIVOT,j,N,NDIM
      dimension X(NDIM),W(NDIM,NDIM),B(NDIM),IPIVOT(NDIM)
      data zero/0.0D0/
      
      if(N.EQ.1)then
      X(1)=B(1)/W(1,1)
      return
      endif
      
      
      ip=IPIVOT(1)
      X(1)=B(ip)
      do 100 i=2,N
      sum=zero
      do 50 j=1,i-1
      sum=W(i,j)*X(j)+sum
50    continue
      ip=IPIVOT(i)
      X(i)=B(ip)-sum
100   continue
      X(N)=X(N)/W(N,N)
      do 200 i=N-1,1,-1
      sum=zero
      do 150 j=i+1,N
      sum=W(i,j)*X(j)+sum
150   continue
      X(i)=(X(i)-sum)/W(i,i)
200   continue
      return
      end
C* :1 * 
      
