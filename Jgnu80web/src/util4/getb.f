
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getb"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getb.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 48 "getb.web"
      subroutine getb(IEO,TO,LENTO,BB,NBB)
      implicit none
      integer BB,deltyp,getchr,IEO,j,jt,LENTO,leost,n,NBB,TO
      dimension TO(*),BB(*)
      
      LENTO=0
      if(IEO.EQ.2)then
      
50    j=getchr(BB,NBB)
      jt=deltyp(j)
      if(jt.GE.3)then
      NBB=NBB-1
      return
      
      elseif(jt.EQ.2)then
      
      if(LENTO.NE.0)return
      n=NBB-2
      j=getchr(BB,n)
      jt=deltyp(j)
      if(jt.EQ.2)NBB=NBB-1
      return
      else
      call puticr(j,TO,LENTO)
      goto 50
      endif
      elseif(IEO.EQ.3)then
      
      leost=0
      if(NBB.GT.0)then
      NBB=NBB-1
      j=getchr(BB,NBB)
      jt=deltyp(j)
      if(jt.EQ.2)leost=1
      endif
100   j=getchr(BB,NBB)
      jt=deltyp(j)
      if(jt.GE.4)then
      NBB=NBB-1
      return
      
      elseif(jt.NE.2.OR.leost.NE.1)then
      leost=0
      if(jt.EQ.2)leost=1
      call puticr(j,TO,LENTO)
      goto 100
      endif
      else
      
150   j=getchr(BB,NBB)
      jt=deltyp(j)
      if(jt.LT.2)then
      
      if(jt.EQ.1)return
      call puticr(j,TO,LENTO)
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
      
