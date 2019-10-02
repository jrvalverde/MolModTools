
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ssso"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ssso.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "ssso.web"
      subroutine ssso(MAXAP3,NATOMS,IATFLG,A,ITST)
      implicit none
      double precision A,gabs,Tol2,Toler
      integer iat,IATFLG,ITST,ixyz,MAXAP3,NATOMS
      dimension IATFLG(*),A(MAXAP3,3)
      common/tol/Toler,Tol2
      
      
      
      
      
      
      ITST=0
      do 100 iat=1,NATOMS
      if(IATFLG(iat).EQ.0)then
      do 20 ixyz=1,3
      if(gabs(A(iat,ixyz)).GT.Toler)goto 100
20    continue
      IATFLG(iat)=2
      ITST=1
      return
      endif
      
100   continue
      return
      
      end
C* :1 * 
      
