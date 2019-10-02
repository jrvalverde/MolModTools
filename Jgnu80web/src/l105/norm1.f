
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 norm1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "norm1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "norm1.web"
      double precision function norm1(X,N)
      implicit none
      double precision gabs,X,zero
      integer i,N
      dimension X(N)
      data zero/0.0D0/
      
      
      
      
      
      norm1=zero
      do 100 i=1,N
      if(gabs(X(i)).GT.norm1)norm1=gabs(X(i))
100   continue
      return
      
      end
C* :1 * 
      
