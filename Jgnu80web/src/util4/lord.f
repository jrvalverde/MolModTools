
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lord"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lord.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "lord.web"
      integer function lord(CHR)
      implicit none
      character*1 CHR
      
      lord=ichar(CHR)
      return
      
      end
C* :1 * 
      
