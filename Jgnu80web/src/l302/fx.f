
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fx"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fx.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "fx.web"
      double precision function fx(N,X,A)
      implicit none
      double precision A,sum,X
      integer i,N
      dimension A(*)
      
      
      
      
      
      sum=A(1)
      if(N.GT.0)then
      
      do 50 i=1,N
      sum=sum*X+A(i+1)
50    continue
      endif
      
      fx=sum
      return
      
      end
C* :1 * 
      
