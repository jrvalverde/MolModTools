
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 strout"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "strout.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "strout.web"
      subroutine strout(IOUT,ISTR,LEN,CRLF)
      implicit none
      integer blank,CRLF,i,ilen,IOUT,LEN,n,nold
      integer ISTR(*)
      integer jstr(33)
      character*1 str(132)
      equivalence(jstr(1),str(1))
      data blank/' '/,n/0/
      
99001 format(' ',132A1)
99002 format('+',132A1)
      
      ilen=LEN/4
      if(mod(LEN,4).GT.0)ilen=ilen+1
      do 100 i=1,ilen
      jstr(i)=ISTR(i)
100   continue
      if(CRLF.EQ.-1)n=nold
      if(n+LEN.GT.132)n=0
      if(n.EQ.0)write(IOUT,99001)(str(i),i=1,LEN)
      if(n.NE.0)write(IOUT,99002)(blank,i=1,n),(str(i),i=1,LEN)
      n=n+LEN
      nold=n
      if(CRLF.NE.0)n=0
      return
      
      end
C* :1 * 
      
