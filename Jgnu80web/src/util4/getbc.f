
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getbc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getbc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 46 "getbc.web"
      subroutine getbc(IEO,TO,LENTO,BB,NBB)
      implicit none
      integer deltyc,IEO,jt,LENTO,leost,n,NBB
      character*1 TO(*),BB(*)
      character*1 j
      character getch
      
      LENTO=0
      if(IEO.EQ.2)then
      
50    j=getch(BB,NBB)
      jt=deltyc(j)
      if(jt.GE.3)then
      NBB=NBB-1
      return
      
      elseif(jt.EQ.2)then
      
      if(LENTO.NE.0)return
      n=NBB-2
      j=getch(BB,n)
      jt=deltyc(j)
      if(jt.EQ.2)NBB=NBB-1
      return
      else
      call putchr(j,TO,LENTO)
      goto 50
      endif
      elseif(IEO.EQ.3)then
      
      leost=0
      if(NBB.GT.0)then
      NBB=NBB-1
      j=getch(BB,NBB)
      jt=deltyc(j)
      if(jt.EQ.2)leost=1
      endif
100   j=getch(BB,NBB)
      jt=deltyc(j)
      if(jt.GE.4)then
      NBB=NBB-1
      return
      
      elseif(jt.NE.2.OR.leost.NE.1)then
      leost=0
      if(jt.EQ.2)leost=1
      call putchr(j,TO,LENTO)
      goto 100
      endif
      else
      
150   j=getch(BB,NBB)
      jt=deltyc(j)
      if(jt.LT.2)then
      
      if(jt.EQ.1)return
      call putchr(j,TO,LENTO)
      goto 150
      else
      NBB=NBB-1
      return
      endif
      endif
      
      if(LENTO.GT.0)LENTO=LENTO-1
      return
      
      end
C* :1 * 
      
