
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 equiv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "equiv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "equiv.web"
      subroutine equiv(MAXAP3,A,B,ATMCHG,NATOMS,ITST)
      implicit none
      double precision A,ATMCHG,B,gabs,test,Tol2,Toler
      integer iat,ITST,ixyz,jat,MAXAP3,NATOMS
      dimension A(MAXAP3,3),B(MAXAP3,3),ATMCHG(*)
      common/tol/Toler,Tol2
      
      
      
      
      
      
      do 100 iat=1,NATOMS
      do 50 jat=1,NATOMS
      if(gabs(ATMCHG(iat)-ATMCHG(jat)).LE.Tol2)then
      do 10 ixyz=1,3
      test=A(iat,ixyz)-B(jat,ixyz)
      if(gabs(test).GT.Toler)goto 50
10    continue
      goto 100
      endif
      
50    continue
      ITST=0
      return
      
100   continue
      ITST=1
      return
      
      end
C* :1 * 
      
