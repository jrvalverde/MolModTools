
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 move"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "move.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "move.web"
      subroutine move(MAXAP3,A,B,N)
      implicit none
      double precision A,B
      integer i,j,MAXAP3,N
      dimension A(MAXAP3,3),B(MAXAP3,3)
      
      
      
      
      
      do 100 i=1,N
      do 50 j=1,3
      B(i,j)=A(i,j)
50    continue
100   continue
      return
      
      end
C* :1 * 
      
