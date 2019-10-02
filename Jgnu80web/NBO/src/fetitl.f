
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fetitl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fetitl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "fetitl.web"
      subroutine fetitl(TITLE)
      implicit none
      integer nfile
      double precision TITLE
      dimension TITLE(10)
      
      
      nfile=2
      call nbread(TITLE,10,nfile)
      return
      end
C* :1 * 
      
