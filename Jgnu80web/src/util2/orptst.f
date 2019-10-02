
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 orptst"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "orptst.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "orptst.web"
      subroutine orptst(MAXAP3,A,NATOMS,IXYZ)
      implicit none
      double precision A,gabs,Tol2,Toler
      integer iat,IXYZ,jxyz,MAXAP3,NATOMS
      dimension A(MAXAP3,3)
      common/tol/Toler,Tol2
      
      
      
      
      
      
      do 100 jxyz=1,3
      do 50 iat=1,NATOMS
      if(gabs(A(iat,jxyz)).GT.Toler)goto 100
50    continue
      IXYZ=jxyz
      return
      
100   continue
      IXYZ=0
      return
      
      end
C* :1 * 
      
