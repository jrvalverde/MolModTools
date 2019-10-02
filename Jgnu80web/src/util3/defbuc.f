
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 defbuc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "defbuc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "defbuc.web"
      subroutine defbuc(IBUCK,LENGTH)
      implicit none
      integer IBUCK,LENGTH
      
      
      
      
      
      call fileio(0,IBUCK,LENGTH,0,0)
      
      return
      
      end
C* :1 * 
      
