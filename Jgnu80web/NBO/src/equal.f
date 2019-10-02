
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 equal"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "equal.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "equal.web"
      function equal(IA,IB,L)
      implicit none
      integer i,IA,IB,L
      logical equal
      
      dimension IA(L),IB(L)
      
      
      equal=.FALSE.
      do 100 i=1,L
      if(IA(i).NE.IB(i))goto 200
100   continue
      equal=.TRUE.
200   return
      end
C* :1 * 
      
