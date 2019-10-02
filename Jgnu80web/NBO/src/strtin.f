
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 strtin"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "strtin.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "strtin.web"
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine strtin(LFNIN)
      implicit none
      integer Icd,Ipt,Length,Lfn,LFNIN,Look,Nexp
      
      common/nbcrd1/Icd(80),Look(80),Length,Ipt,Lfn,Nexp
      common/nbcrd2/Point,End,Next,Exp
      logical Point,End,Next,Exp
      
      
      Lfn=LFNIN
      End=.FALSE.
      Next=.TRUE.
      call rdcard
      
      return
      end
C* :1 * 
      
