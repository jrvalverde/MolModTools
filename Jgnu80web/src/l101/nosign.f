
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nosign"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nosign.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 35 "nosign.web"
      subroutine nosign(STR,LEN,ISGN)
      implicit none
      integer chr,cursor,getchr,iord,ISGN,LEN,ocursr,STR
      dimension STR(*)
      cursor=0
      ISGN=1
      chr=getchr(STR,cursor)
      if(chr.NE.iord('-').AND.chr.NE.iord('+'))return
      
      ISGN=-1
      if(chr.EQ.iord('+'))ISGN=1
      LEN=LEN-1
      ocursr=0
100   if(cursor.GT.LEN)return
      chr=getchr(STR,cursor)
      call puticr(chr,STR,ocursr)
      goto 100
      
      end
C* :1 * 
      
