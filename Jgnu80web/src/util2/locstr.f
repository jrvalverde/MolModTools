
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 locstr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "locstr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "locstr.web"
      subroutine locstr(SUBSTR,LENSUB,STRING,LENSTR,CURSOR)
      implicit none
      integer chrstr,chrsub,CURSOR,i,iend,ij,j,jj,LENSTR,LENSUB,STRING,S
     &UBSTR
      integer getchr
      iend=LENSTR-LENSUB+1
      if(CURSOR.LE.iend)then
      if(CURSOR.LE.0)CURSOR=1
      do 50 i=CURSOR,iend
      jj=0
      ij=i-2
      do 20 j=1,LENSUB
      chrsub=getchr(SUBSTR,jj)
      chrstr=getchr(STRING,ij)
      if(chrsub.NE.chrstr)goto 50
20    continue
      goto 100
      
50    continue
      endif
      i=0
100   CURSOR=i-1
      return
      
      end
C* :1 * 
      
