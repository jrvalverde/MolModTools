
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 putstr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "putstr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "putstr.web"
      subroutine putstr(INSTR,LEN,STRING,CURSOR)
      implicit none
      real CURSOR
      integer i,LEN
      integer INSTR(*),STRING(*)
      
      
      do 100 i=1,LEN
      call puticr(INSTR(i),STRING,CURSOR)
100   continue
      return
      
      end
C* :1 * 
      
