
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 vec"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "vec.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "vec.web"
      subroutine vec(SMALL,OHOH,U,C,J,K)
      implicit none
      double precision C,gsqrt,r,r2,SMALL,U,zero
      integer i,J,jtemp,K,ktemp
      logical OHOH
      dimension C(*),r(3),U(3)
      data zero/0.0D0/
      
      
      
      
      r2=zero
      jtemp=(J-1)*3
      ktemp=(K-1)*3
      do 100 i=1,3
      r(i)=C(i+jtemp)-C(i+ktemp)
      r2=r2+r(i)*r(i)
100   continue
      r2=gsqrt(r2)
      OHOH=r2.LT.SMALL
      if(OHOH)return
      do 200 i=1,3
      U(i)=r(i)/r2
200   continue
      return
      
      end
C* :1 * 
      
