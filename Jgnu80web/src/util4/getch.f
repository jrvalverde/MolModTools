
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getch"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getch.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "getch.web"
      character function getch(STRING,CURSOR)
      implicit none
      character*1 STRING(*)
      integer CURSOR
      CURSOR=CURSOR+1
      getch=STRING(CURSOR)
      return
      end
C* :1 * 
      
