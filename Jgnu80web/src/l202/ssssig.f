
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ssssig"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ssssig.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "ssssig.web"
      subroutine ssssig(MAXAP3,NATOMS,IATFLG,A,IXYZ,ITST)
      implicit none
      double precision A,gabs,Tol2,Toler
      integer iat,IATFLG,ITST,IXYZ,MAXAP3,NATOMS
      dimension IATFLG(*),A(MAXAP3,3)
      common/tol/Toler,Tol2
      
      
      
      
      
      
      ITST=0
      do 100 iat=1,NATOMS
      if(gabs(A(iat,IXYZ)).LE.Toler.AND.IATFLG(iat).EQ.0)then
      IATFLG(iat)=2
      ITST=1
      endif
100   continue
      return
      
      end
C* :1 * 
      
