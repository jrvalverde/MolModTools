
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 pad"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "pad.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "pad.web"
      subroutine pad(STRING,OLDLEN,NEWLEN,PADCHR)
      implicit none
      integer i,istart,NEWLEN,OLDLEN,PADCHR,STRING
      dimension STRING(*)
      
      
      
      if(OLDLEN.GE.NEWLEN)return
      istart=OLDLEN+1
      do 100 i=istart,NEWLEN
      call puticr(PADCHR,STRING,OLDLEN)
100   continue
      
      return
      
      
      end
C* :1 * 
      
