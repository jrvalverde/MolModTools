
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sssc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sssc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "sssc.web"
      subroutine sssc(MAXAP3,NATOMS,IATFLG,A,IXYZ,ITST)
      implicit none
      double precision A,gabs,Tol2,Toler
      integer i1,i2,iat,IATFLG,ITST,IXYZ,MAXAP3,NATOMS
      dimension IATFLG(*),A(MAXAP3,3)
      common/tol/Toler,Tol2
      
      
      
      
      
      
      i1=1+mod(IXYZ,3)
      i2=1+mod(i1,3)
      ITST=0
      do 100 iat=1,NATOMS
      if(gabs(A(iat,i1)).LE.Toler.AND.gabs(A(iat,i2)).LE.Toler.AND.IATFL
     &G(iat).EQ.0)then
      ITST=1
      IATFLG(iat)=2
      endif
100   continue
      return
      
      end
C* :1 * 
      
