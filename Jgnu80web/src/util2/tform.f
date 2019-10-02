
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tform"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tform.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "tform.web"
      subroutine tform(MAXAP3,T,A,B,N)
      implicit none
      double precision A,B,T
      integer iat,MAXAP3,N
      dimension T(3,3),A(MAXAP3,3),B(MAXAP3,3)
      
      
      
      
      
      do 100 iat=1,N
      B(iat,1)=T(1,1)*A(iat,1)+T(1,2)*A(iat,2)+T(1,3)*A(iat,3)
      B(iat,2)=T(2,1)*A(iat,1)+T(2,2)*A(iat,2)+T(2,3)*A(iat,3)
      B(iat,3)=T(3,1)*A(iat,1)+T(3,2)*A(iat,2)+T(3,3)*A(iat,3)
100   continue
      return
      
      end
C* :1 * 
      
