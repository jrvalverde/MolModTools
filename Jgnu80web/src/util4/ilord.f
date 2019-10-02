
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ilord"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ilord.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "ilord.web"
      integer function ilord(ICR)
      integer ICR,jcr
      character*1 chr(4)
      equivalence(jcr,chr(1))
      
      jcr=ICR
      ilord=ichar(chr(1))
      return
      
      end
C* :1 * 
      
