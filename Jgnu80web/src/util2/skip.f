
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 skip"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "skip.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "skip.web"
      subroutine skip(IEO,BB,NBB)
      implicit none
      integer BB,c1,c1t,c2,deltyp,getchr,i,IEO,j,jt,leos,NBB
      dimension BB(*)
      
      if(IEO.EQ.2)then
      
      if(NBB.NE.0)then
      i=NBB-1
      c1=getchr(BB,i)
      c2=getchr(BB,i)
      c1t=deltyp(c1)
      if(c1.EQ.c2.AND.c1t.EQ.2)return
      endif
50    j=getchr(BB,NBB)
      jt=deltyp(j)
      if(jt.LT.3)then
      
      if(jt.EQ.2)return
      goto 50
      else
      NBB=NBB-1
      return
      endif
      elseif(IEO.EQ.3)then
      
      if(NBB.NE.0)NBB=NBB-1
      leos=0
100   c1=getchr(BB,NBB)
      c1t=deltyp(c1)
      if(c1t.LT.4)then
      
      if(c1t.NE.2)then
      leos=0
      else
      if(leos.EQ.1)return
      leos=1
      endif
      goto 100
      endif
      else
      
150   j=getchr(BB,NBB)
      jt=deltyp(j)
      if(jt.LT.2)then
      
      if(jt.EQ.1)return
      goto 150
      else
      NBB=NBB-1
      return
      endif
      endif
      NBB=NBB-1
      return
      
      end
C* :1 * 
      
