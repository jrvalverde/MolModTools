
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 or3mom"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "or3mom.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "or3mom.web"
      double precision function or3mom(MAXAP3,A,ATMCHG,NATOMS,IXYZ)
      implicit none
      double precision A,ATMCHG,zero
      integer iat,IXYZ,MAXAP3,NATOMS
      dimension A(MAXAP3,3),ATMCHG(*)
      data zero/0.0D0/
      
      
      
      
      
      
      or3mom=zero
      do 100 iat=1,NATOMS
      or3mom=or3mom+ATMCHG(iat)*A(iat,IXYZ)**3
100   continue
      return
      
      end
C* :1 * 
      
