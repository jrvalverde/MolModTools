
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 shldat"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "shldat.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "shldat.web"
      subroutine shldat(MYFSHL,MYNSHL,NSHELL,X,Y,Z)
      implicit none
      integer iatom,ishell,jshell,MYFSHL,MYNSHL,n,NSHELL
      double precision X,Y,Z
      logical newatm
      dimension MYFSHL(*),MYNSHL(*)
      dimension X(*),Y(*),Z(*)
      
      
      
      
      
      
      
      MYFSHL(1)=1
      iatom=1
      n=1
      do 100 ishell=2,NSHELL
      jshell=ishell-1
      newatm=(X(ishell).NE.X(jshell)).OR.(Y(ishell).NE.Y(jshell)).OR.(Z(
     &ishell).NE.Z(jshell))
      if(newatm)then
      
      MYNSHL(iatom)=n
      n=1
      iatom=iatom+1
      MYFSHL(iatom)=ishell
      else
      n=n+1
      endif
100   continue
      MYNSHL(iatom)=n
      
      return
      
      end
C* :1 * 
      
