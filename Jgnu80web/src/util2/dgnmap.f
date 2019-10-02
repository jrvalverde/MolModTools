
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dgnmap"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dgnmap.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "dgnmap.web"
      subroutine dgnmap(A,NB,NCOL,MAP,NDGN,NOSYM)
      implicit none
      double precision gabs
      integer i,imin,j,MAP,NB,NCOL,NDGN,NOSYM
      double precision A(NB,NCOL),thrsh
      dimension MAP(NCOL)
      data thrsh/1.0D-2/
      
      
      if(NCOL.LE.1)return
      do 100 i=2,NCOL
      imin=i-1
      do 50 j=1,imin
      
      if(gabs(A(i,j)).GE.thrsh)then
      
      if(MAP(i).EQ.0.AND.MAP(j).EQ.0)then
      
      NDGN=NDGN+1
      MAP(i)=NDGN
      MAP(j)=NDGN
      else
      
      if(MAP(i).EQ.0.AND.MAP(j).NE.0)MAP(i)=MAP(j)
      if(MAP(i).NE.0.AND.MAP(j).EQ.0)MAP(j)=MAP(i)
      
      if(MAP(i).NE.0.AND.MAP(j).NE.0.AND.MAP(i).NE.MAP(j))NOSYM=1
      endif
      endif
50    continue
100   continue
      return
      
      end
C* :1 * 
      
