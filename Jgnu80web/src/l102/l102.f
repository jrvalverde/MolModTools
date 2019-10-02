
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 l102"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "l102.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "l102.web"
      subroutine l102(NCHAIN)
      implicit none
      integer Iret,Jump,NCHAIN,nextov
      common/j102/Jump,Iret
      
      Iret=0
      call fpmain
      NCHAIN=nextov(Jump)
      return
      
      end
C* :1 * 
      
