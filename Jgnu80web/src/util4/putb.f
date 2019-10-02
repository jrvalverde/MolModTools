
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 putb"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "putb.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "putb.web"
      subroutine putb(INSTR,LENIN,BB,NBB)
      implicit none
      integer BB,chr,getchr,i,in,INSTR,LENIN,NBB
      dimension INSTR(*),BB(*)
      
      in=0
      do 100 i=1,LENIN
      chr=getchr(INSTR,in)
      call puticr(chr,BB,NBB)
100   continue
      return
      
      end
C* :1 * 
      
