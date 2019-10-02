
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 putdel"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "putdel.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "putdel.web"
      subroutine putdel(IEO,BB,NBB)
      implicit none
      integer BB,c,eor,eost,eow,getchr,IEO,NBB
      dimension BB(*)
      data eow/','/,eost/'/'/,eor/10/
      
      
      
      if(IEO.EQ.2)then
      
      if(NBB.NE.0)then
      NBB=NBB-1
      c=getchr(BB,NBB)
      if(c.EQ.eost)return
      if(c.EQ.eow)NBB=NBB-1
      endif
      call puticr(eost,BB,NBB)
      return
      elseif(IEO.EQ.3)then
      
      if(NBB.NE.0)then
      
20    NBB=NBB-1
      c=getchr(BB,NBB)
      if(c.EQ.eow)NBB=NBB-1
      if(c.EQ.eow)goto 20
      else
      c=0
      endif
      if(c.NE.eost)call puticr(eost,BB,NBB)
      call puticr(eost,BB,NBB)
      return
      elseif(IEO.NE.4)then
      
      call puticr(eow,BB,NBB)
      return
      endif
      
      call puticr(eor,BB,NBB)
      return
      
      end
C* :1 * 
      
