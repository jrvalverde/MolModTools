
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rotmap"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rotmap.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "rotmap.web"
      subroutine rotmap(NSHELL,SHELLT,SHELLC,I56D,MAPROT,NROT)
      implicit none
      integer i,I56D,igo,MAPROT,NROT,NSHELL,SHELLC,SHELLT
      dimension SHELLT(*),SHELLC(*),MAPROT(*)
      
      NROT=0
      do 100 i=1,NSHELL
      NROT=NROT+1
      igo=SHELLT(i)+1
      if(igo.EQ.2)then
      if(SHELLC(i).NE.1)then
      MAPROT(NROT)=1
      NROT=NROT+1
      endif
      MAPROT(NROT)=3
      elseif(igo.EQ.3)then
      if(SHELLC(i).EQ.0)then
      MAPROT(NROT)=1
      MAPROT(NROT+1)=3
      NROT=NROT+2
      endif
      if(I56D.EQ.0)MAPROT(NROT)=5
      if(I56D.NE.0)MAPROT(NROT)=6
      elseif(igo.EQ.4)then
      MAPROT(NROT)=7
      else
      MAPROT(NROT)=1
      endif
100   continue
      return
      
      end
C* :1 * 
      
