
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 puti"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "puti.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "puti.web"
      subroutine puti(I,BB,NBB)
      implicit none
      integer I,NBB
      integer BB(*)
      
      call decchr(I,BB,NBB)
      call putdel(1,BB,NBB)
      return
      
      end
C* :1 * 
      
