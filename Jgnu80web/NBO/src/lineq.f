
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lineq"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lineq.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "lineq.web"
      subroutine lineq(A,X,B,SCR,N,M,NDIM,MDIM,ZERTOL,EPS,MAXIT,LFNPR,IE
     &RR)
      
      
      
      implicit none
      double precision A,B,del,EPS,epsmax,SCR,X,zero,ZERTOL
      integer i1,i2,i3,i4,i5,i6,IERR,iflag,its,itsmax,jrow,kcol,LFNPR,M,
     &MAXIT,MDIM,N,NDIM
      dimension A(NDIM,NDIM),X(NDIM,MDIM),B(NDIM,MDIM),SCR(NDIM*(NDIM+5)
     &)
      data zero/0.0/
      
      if(N.LT.1)stop 'Dimension N is not positive'
      
      
      i1=1
      i2=i1+NDIM*NDIM
      i3=i2+NDIM
      i4=i3+NDIM
      i5=i4+NDIM
      i6=i5+NDIM
      
      
      call dofactor(A,SCR(i1),SCR(i2),SCR(i6),N,NDIM,ZERTOL,iflag)
      if(iflag.EQ.0)then
      IERR=1
      return
      else
      IERR=0
      endif
      
      
      epsmax=zero
      itsmax=0
      do 200 kcol=1,M
      do 50 jrow=1,N
      SCR(i4+jrow-1)=X(jrow,kcol)
      SCR(i5+jrow-1)=B(jrow,kcol)
50    continue
      its=MAXIT
      del=EPS
      
      
      call fndsol(A,SCR(i4),SCR(i5),SCR(i1),SCR(i2),SCR(i3),SCR(i6),N,ND
     &IM,del,its,LFNPR,IERR)
      if(IERR.NE.0)return
      
      
      do 100 jrow=1,N
      X(jrow,kcol)=SCR(i4+jrow-1)
100   continue
      if(del.GT.epsmax)epsmax=del
      if(its.GT.itsmax)itsmax=its
200   continue
      
      EPS=epsmax
      MAXIT=itsmax
      return
      end
C* :1 * 
      
