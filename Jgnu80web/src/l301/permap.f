
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 permap"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "permap.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "permap.web"
      subroutine permap(NSHELL,SHELLT,SHELLC,COORD,I56D,MAPPER)
      implicit none
      integer i,I56D,j,MAXSHL,nao,natoms,nfnct,NSHELL
      parameter(MAXSHL=100)
      integer SHELLT(*),SHELLC(*),MAPPER(100)
      double precision COORD(MAXSHL,3),cloc(3),thrsh
      dimension nfnct(3,4)
      data thrsh/1.0D-4/
      data nfnct/1,1,1,4,3,4,9,5,5,7,7,7/
      
      
      natoms=1
      nao=0
      MAPPER(1)=1
      cloc(1)=COORD(1,1)
      cloc(2)=COORD(1,2)
      cloc(3)=COORD(1,3)
      
      do 100 i=1,NSHELL
      if(dabs(cloc(1)-COORD(i,1)).GE.thrsh.OR.dabs(cloc(2)-COORD(i,2)).G
     &E.thrsh.OR.dabs(cloc(3)-COORD(i,3)).GE.thrsh)then
      natoms=natoms+1
      MAPPER(natoms)=nao+1
      do 20 j=1,3
      cloc(j)=COORD(i,j)
20    continue
      endif
      nao=nao+nfnct(SHELLC(i)+1,SHELLT(i)+1)
      if(SHELLT(i).EQ.2.AND.I56D.EQ.1)nao=nao+1
100   continue
      return
      
      end
C* :1 * 
      
