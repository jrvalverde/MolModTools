
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 shift"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "shift.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "shift.web"
      integer function shift(I,J)
      implicit none
      integer I,J,jj,lshift
      real rshift
      
      if(J.GE.0)then
      shift=lshift(I,J)
      return
      endif
      
      jj=iabs(J)
      shift=rshift(I,jj)
      return
      
      end
C* :1 * 
      
