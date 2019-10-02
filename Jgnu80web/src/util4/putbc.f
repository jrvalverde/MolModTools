
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 putbc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "putbc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "putbc.web"
      subroutine putbc(INSTR,LENIN,BB,NBB)
      implicit none
      integer BB,i,in,LENIN,NBB
      dimension BB(*)
      character*1 INSTR(*)
      character*1 chr
      character getch
      
      in=0
      do 100 i=1,LENIN
      chr=getch(INSTR,in)
      call putchr(chr,BB,NBB)
100   continue
      return
      
      end
C* :1 * 
      
