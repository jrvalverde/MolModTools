
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 shlatm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "shlatm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "shlatm.web"
      subroutine shlatm(NATOMS,C,NSHELL,X,Y,Z,IATM)
      implicit none
      double precision C,ccut,gabs,X,Y,Z
      integer i,ia,iaind,IATM,NATOMS,NSHELL
      dimension C(*),X(*),Y(*),Z(*),IATM(*)
      data ccut/1.0D-06/
      
      
      
      
      
      
      ia=1
      iaind=0
      do 100 i=1,NSHELL
      if(gabs(X(i)-C(1+iaind)).LE.ccut)then
      if(gabs(Y(i)-C(2+iaind)).LE.ccut)then
      if(gabs(Z(i)-C(3+iaind)).LE.ccut)goto 50
      endif
      endif
      
      ia=ia+1
      iaind=iaind+3
50    IATM(i)=ia
100   continue
      
      return
      
      end
C* :1 * 
      
