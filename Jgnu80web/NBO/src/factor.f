
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 factor"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "factor.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "factor.web"
      subroutine dofactor(A,W,D,IPIVOT,N,NDIM,ZERTOL,IFLAG)
      implicit none
      double precision A,colmax,D,one,rowmax,temp,W,zero,ZERTOL
      integer i,IFLAG,IPIVOT,istar,itemp,j,k,N,NDIM
      dimension A(NDIM,NDIM),W(NDIM,NDIM),D(NDIM),IPIVOT(NDIM)
      data zero,one/0.0D0,1.0D0/
      
      
      IFLAG=1
      
      
      call copy(A,W,NDIM,N,N)
      
      
      do 100 i=1,N
      IPIVOT(i)=i
      rowmax=zero
      do 50 j=1,N
      if(abs(W(i,j)).GT.rowmax)rowmax=abs(W(i,j))
50    continue
      if(rowmax.LE.ZERTOL)then
      IFLAG=0
      rowmax=one
      endif
      D(i)=rowmax
100   continue
      if(N.EQ.1)return
      
      
      do 200 k=1,N-1
      
      
      colmax=abs(W(k,k))/D(k)
      istar=k
      do 150 i=k+1,N
      temp=abs(W(i,k))/D(k)
      if(temp.GT.colmax)then
      colmax=temp
      istar=i
      endif
150   continue
      if(colmax.EQ.zero)then
      IFLAG=0
      else
      if(istar.GT.k)then
      
      IFLAG=-IFLAG
      itemp=IPIVOT(istar)
      IPIVOT(istar)=IPIVOT(k)
      IPIVOT(k)=itemp
      temp=D(istar)
      D(istar)=D(k)
      D(k)=temp
      do 160 j=1,N
      temp=W(istar,j)
      W(istar,j)=W(k,j)
      W(k,j)=temp
160   continue
      endif
      
      
      do 180 i=k+1,N
      W(i,k)=W(i,k)/W(k,k)
      do 170 j=k+1,N
      W(i,j)=W(i,j)-W(i,k)*W(k,j)
170   continue
180   continue
      endif
200   continue
      if(abs(W(N,N)).LE.ZERTOL)IFLAG=0
      return
      end
C* :1 * 
      
