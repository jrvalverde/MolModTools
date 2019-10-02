
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fndsol"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fndsol.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "fndsol.web"
      subroutine fndsol(A,X,B,W,R,E,IPIVOT,N,NDIM,EPS,MAXIT,LFNPR,IERR)
      implicit none
      double precision A,B,E,elen,EPS,R,rellen,veclen,W,X,xlen,zero
      integer i,IERR,IPIVOT,iter,j,LFNPR,MAXIT,N,NDIM
      dimension A(NDIM,NDIM),X(NDIM),B(NDIM),W(NDIM,NDIM),R(NDIM),E(NDIM
     &),IPIVOT(NDIM)
      data zero/0.0D0/
      
      
      call copy(B,E,NDIM,N,1)
      call subst(X,W,E,IPIVOT,N,NDIM)
      if(MAXIT.EQ.0)return
      
      
      rellen=zero
      iter=0
100   if(rellen.GT.EPS)then
      iter=iter+1
      do 150 i=1,N
      R(i)=B(i)
      do 120 j=1,N
      R(i)=R(i)-A(i,j)*X(j)
120   continue
150   continue
      call subst(E,W,R,IPIVOT,N,NDIM)
      elen=veclen(E,N,NDIM)
      xlen=veclen(X,N,NDIM)
      rellen=elen/xlen
      do 200 i=1,N
      X(i)=X(i)+E(i)
200   continue
      
      
      if(LFNPR.GT.0)write(LFNPR,99001)iter,rellen
      
      
      if(iter.EQ.MAXIT)then
      if(rellen.GT.EPS)IERR=-1
      if(LFNPR.GT.0)then
      if(IERR.LT.0)then
      write(LFNPR,99002)
      else
      write(LFNPR,99003)
      endif
      endif
      EPS=rellen
      return
      endif
      
      
      else
      if(LFNPR.GT.0)write(LFNPR,99003)
      EPS=rellen
      MAXIT=iter
      return
      endif
      goto 100
      
99001 format(1x,'Iter = ',i3,'    relative length = ',f10.7)
99002 format(1x,'No convergence within the specified number of ','iterat
     &ions')
99003 format(1x,'The error vector is converged')
      end
C* :1 * 
      
