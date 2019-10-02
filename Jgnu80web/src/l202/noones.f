
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 noones"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "noones.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 42 "noones.web"
      subroutine noones(OLDLEN,OLDSTR,NEWLEN,NEWSTR)
      implicit none
      integer blank,bra,i,i1,minus,NEWLEN,NEWSTR,next,nine,OLDLEN,OLDSTR
     &,one,plus,prev,zero
      logical fwg
      dimension OLDSTR(6),NEWSTR(6)
      data zero/'0'/,blank/' '/,bra/'<'/,one/'1'/
      data plus/1H+/,minus/1H-/,nine/1H9/
      
      
      fwg=OLDSTR(6).EQ.bra
      NEWLEN=0
      prev=blank
      i1=1
      if(fwg)then
      
      
      do 50 i=1,4
      if(.NOT.(OLDSTR(i).EQ.blank.OR.(OLDSTR(i).EQ.zero.AND.i.NE.3)))the
     &n
      NEWLEN=NEWLEN+1
      NEWSTR(NEWLEN)=OLDSTR(i)
      endif
50    continue
      i1=6
      endif
      
      
      do 100 i=i1,OLDLEN
      next=OLDSTR(i+1)
      if(i.EQ.OLDLEN)next=blank
      if(i.NE.1)prev=OLDSTR(i-1)
      if(.NOT.(OLDSTR(i).EQ.one.AND.(next.LT.zero.OR.next.GT.nine).AND.n
     &ext.NE.plus.AND.next.NE.minus.AND.(prev.LT.zero.OR.prev.GT.nine)))
     &then
      NEWLEN=NEWLEN+1
      NEWSTR(NEWLEN)=OLDSTR(i)
      endif
100   continue
      
      
      return
      
      end
C* :1 * 
      
