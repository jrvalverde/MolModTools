
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 hexchr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "hexchr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "hexchr.web"
      integer function hexchr(N)
      implicit none
      integer blank,ichar,N
      dimension ichar(37)
      data blank/' '/
      data ichar/'0','1','2','3','4','5','6','7','8','9','A','B','C','D'
     &,'E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','
     &U','V','W','X','X','Y','Z'/
      
      
      
      
      
      
      
      
      hexchr=blank
      if(N.GT.36.OR.N.LT.0)return
      hexchr=ichar(N+1)
      return
      
      end
C* :1 * 
      
