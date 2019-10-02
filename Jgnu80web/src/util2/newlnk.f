
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 newlnk"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "newlnk.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "newlnk.web"
      subroutine newlnk(LNK,NLINK,JUMP)
      implicit none
      integer i,icard,In,inc,Iout,Ipunch,jcard,jmpcrd,JUMP,LNK,NLINK
      dimension LNK(200)
      common/io/In,Iout,Ipunch
      
      if(iabs(JUMP).LT.100)then
      
50    NLINK=NLINK+1
      if(NLINK.GT.200)then
      
      write(Iout,99002)
      else
      if(LNK(NLINK).GT.10000)return
      
      if(JUMP.EQ.1)goto 50
      
      
      jmpcrd=iabs(LNK(NLINK))+1
      if(jmpcrd.EQ.0)then
      write(Iout,99002)
      else
      inc=+1
      if(LNK(NLINK).LT.0)inc=-1
      
      if(inc.EQ.-1)jmpcrd=jmpcrd+1
      icard=-99
      
60    NLINK=NLINK+inc
      if(NLINK.GT.200)then
      write(Iout,99002)
      else
      if(NLINK.NE.0)then
      
      if(LNK(NLINK).LT.10000)goto 60
      jcard=mod(LNK(NLINK),1000000)/10000
      else
      jcard=0
      if(icard.EQ.0)then
      
      write(Iout,99003)
      goto 100
      endif
      endif
      if(icard.NE.jcard)jmpcrd=jmpcrd-1
      if(jmpcrd.EQ.0)then
      
62    if(inc.EQ.-1)NLINK=NLINK+1
      if(LNK(NLINK).LT.10000)goto 62
      return
      else
      icard=jcard
      goto 60
      endif
      endif
      endif
      endif
100   write(Iout,99001)(LNK(i),i=1,200)
      
99001 format(200(' LNK: ',7I10,/))
      
      call lnk1e
      call killer
      stop
      
99002 format('  THE LIST OF LINKS IS WEDGED.')
99003 format('  A BOGUS JUMP IS IN THE ROUTE.')
      else
      NLINK=NLINK+mod(JUMP,100)
      return
      endif
      
      end
C* :1 * 
      
