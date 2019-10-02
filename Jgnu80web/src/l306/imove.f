
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 imove"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "imove.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "imove.web"
      subroutine imove(N,INVEC,OUTVEC)
      implicit none
      integer i
      
      
      integer N,INVEC,OUTVEC
      dimension INVEC(*),OUTVEC(*)
      if(N.LT.1)return
      do 100 i=1,N
      OUTVEC(i)=INVEC(i)
100   continue
      return
      end
C* :1 * 
      
