
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 digtst"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "digtst.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "digtst.web"
      subroutine digtst(FWG,IPOS,DIGIT,IVAL)
      implicit none
      integer i,IPOS,IVAL
      integer FWG(*),num(10)
      logical DIGIT
      data num/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/
      
      
      
      
      
      
      IVAL=0
      DIGIT=.FALSE.
      
100   do 200 i=1,10
      if(FWG(IPOS).EQ.num(i))then
      DIGIT=.TRUE.
      IPOS=IPOS+1
      IVAL=10*IVAL+i-1
      goto 100
      endif
      
200   continue
      return
      
      end
C* :1 * 
      
