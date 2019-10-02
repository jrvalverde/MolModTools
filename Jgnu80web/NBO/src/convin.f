
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 convin"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "convin.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "convin.web"
      subroutine convin(IJ,LEN,IK,ERROR)
      implicit none
      integer i,IJ,IK,il,int,j,jj,LEN,mult
      dimension IJ(1)
      dimension int(10)
      logical ERROR
      
      data int/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/
      
      
      ERROR=.FALSE.
      if(LEN.LE.0)then
      ERROR=.TRUE.
      return
      endif
      
      
      il=0
      mult=1
      do 200 i=LEN,1,-1
      do 50 j=1,10
      jj=j-1
      if(IJ(i).EQ.int(j))goto 100
50    continue
      ERROR=.TRUE.
      return
      
100   il=il+jj*mult
      mult=mult*10
200   continue
      IK=il
      return
      end
C* :1 * 
      
