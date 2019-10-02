
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 decchr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "decchr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "decchr.web"
      subroutine decchr(NUM,STRING,CURSOR)
      implicit none
      integer base,CURSOR,digit,hexchr,i,j,n,NUM,STRING,test
      dimension STRING(*)
      
      
      
      
      
      if(NUM.LT.0)call putchr('-',STRING,CURSOR)
      if(NUM.EQ.0)call putchr('0',STRING,CURSOR)
      if(NUM.EQ.0)return
      n=iabs(NUM)
      
      
      digit=9
100   base=10**digit
      test=n/base
      if(test.NE.0)then
      
      
150   i=n/base
      j=hexchr(i)
      call puticr(j,STRING,CURSOR)
      n=n-base*i
      digit=digit-1
      if(digit.LT.0)return
      base=10**digit
      goto 150
      else
      digit=digit-1
      if(digit.LT.0)return
      goto 100
      endif
      
      end
C* :1 * 
      
