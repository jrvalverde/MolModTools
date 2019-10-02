
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 trasq"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "trasq.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "trasq.web"
      double precision function trasq(N,A)
      implicit none
      double precision A,sum,zero
      integer i,ind,N
      dimension A(*)
      data zero/0.0D0/
      ind=1
      sum=zero
      do 100 i=1,N
      sum=sum+A(ind)
      ind=ind+1+N
100   continue
      trasq=sum
      return
      
      end
C* :1 * 
      
