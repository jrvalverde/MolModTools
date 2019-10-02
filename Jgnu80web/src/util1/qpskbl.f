
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpskbl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpskbl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "qpskbl.web"
      subroutine qpskbl(LINE,LCURSR,LENGTH)
      implicit none
      integer getchr,iord,LCURSR,LENGTH
      integer LINE(*)
100   if(LCURSR.GE.LENGTH)return
      if(getchr(LINE,LCURSR).EQ.iord(' '))goto 100
      LCURSR=LCURSR-1
      return
      
      end
C* :1 * 
      
