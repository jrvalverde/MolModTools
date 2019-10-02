
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lfld"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lfld.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "lfld.web"
      subroutine lfld(STB,NBITS,TO,FROM)
      implicit none
      integer FROM,NBITS,STB,TO
      
      TO=TO+FROM*2**STB
      return
      
      end
C* :1 * 
      
