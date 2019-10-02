
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 itqry"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "itqry.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "itqry.web"
      integer function itqry(IFILE)
      implicit none
      integer idum,IFILE,jdum,len
      
      
      
      
      
      
      call fileio(11,IFILE,len,idum,jdum)
      itqry=len
      return
      
      end
C* :1 * 
      
