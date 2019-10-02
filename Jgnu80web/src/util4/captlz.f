
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 captlz"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "captlz.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "captlz.web"
      subroutine captlz(IN,OUT,N)
      implicit none
      integer i,j,jj,kk,N
      integer IN(*),OUT(*)
      character*1 up(26),low(26)
      character chr,getlcu
      data up/'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O
     &','P','Q','R','S','T','U','V','W','X','Y','Z'/
      data low/'a','b','c','d','e','f','g','h','i','j','k','l','m','n','
     &o','p','q','r','s','t','u','v','w','x','y','z'/
      jj=0
      kk=0
      do 200 j=1,N
      chr=getlcu(IN,jj)
      do 50 i=1,26
      if(chr.EQ.low(i))then
      chr=up(i)
      goto 100
      endif
50    continue
100   call putchr(chr,OUT,kk)
200   continue
      return
      end
C* :1 * 
      
