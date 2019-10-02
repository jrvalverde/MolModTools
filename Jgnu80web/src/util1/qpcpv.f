
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpcpv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpcpv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "qpcpv.web"
      integer function qpcpv(DUMMY)
      implicit none
      real DUMMY
      integer nchrpw
      qpcpv=nchrpw(DUMMY)
      return
      
      end
C* :1 * 
      
