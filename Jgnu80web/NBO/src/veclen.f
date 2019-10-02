
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 veclen"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "veclen.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "veclen.web"
      function veclen(X,N,NDIM)
      implicit none
      integer i,N,NDIM
      double precision sum,veclen,X,zero
      dimension X(NDIM)
      data zero/0.0D0/
      
      sum=zero
      do 100 i=1,N
      sum=sum+X(i)*X(i)
100   continue
      veclen=sqrt(sum)
      return
      end
C* :1 * 
      
