
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ascale"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ascale.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "ascale.web"
      subroutine ascale(N,S,A,B)
      implicit none
      double precision A,B,S
      integer i,N
      
      
      dimension A(*),B(*)
      
      do 100 i=1,N
      B(i)=S*A(i)
100   continue
      
      return
      
      end
C* :1 * 
      
