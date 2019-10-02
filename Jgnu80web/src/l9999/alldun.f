
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 alldun"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "alldun.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "alldun.web"
      subroutine alldun
      implicit none
      integer fclose
      data fclose/10/
      call fileio(fclose,0,0,0,0)
      call g80end
      return
      
      end
C* :1 * 
      
