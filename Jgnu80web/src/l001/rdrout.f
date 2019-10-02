
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rdrout"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rdrout.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "rdrout.web"
      subroutine rdrout(ICARD,OUT,LEN)
      implicit none
      integer i,ibl,In,Iout,Ipunch,l,nold
      integer OUT(*),ICARD(*),LEN
      integer getchr
      common/io/In,Iout,Ipunch
      data ibl/'    '/
99001 format(20A4)
      
      l=LEN
      do 100 i=1,LEN
      OUT(i)=ibl
100   continue
      
      LEN=0
      do 200 i=1,80
      call puticr(ICARD(i),ICARD,LEN)
200   continue
      
      i=LEN
      LEN=0
      call pakstr(ICARD,i,OUT,LEN)
      call puticr(ibl,OUT,LEN)
      
300   read(In,99001)(ICARD(i),i=1,20)
      nold=LEN
      call pakstr(ICARD,80,OUT,LEN)
      LEN=LEN+1
      if(LEN.GT.nold+1)goto 300
      
      nold=LEN-1
      i=getchr(OUT,nold)
      if(i.EQ.ibl)LEN=LEN-1
      nold=LEN-1
      i=getchr(OUT,nold)
      if(i.EQ.ibl)LEN=LEN-1
      return
      
      end
C* :1 * 
      
