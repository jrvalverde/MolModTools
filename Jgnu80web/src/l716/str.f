
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 str"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "str.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "str.web"
      subroutine str(NOINT,I,J,B,IB,C,NPARM)
      implicit none
      double precision B,C,dijsq,gsqrt,rij,zero
      integer I,iaind,IB,J,jaind,m,NOINT,NPARM
      dimension B(3,4,NPARM),IB(4,NPARM),C(*)
      dimension rij(3)
      data zero/0.D0/
      
      
      
      
      
      
      
      iaind=3*(I-1)
      jaind=3*(J-1)
      IB(1,NOINT)=I
      IB(2,NOINT)=J
      dijsq=zero
      do 100 m=1,3
      rij(m)=C(m+jaind)-C(m+iaind)
      dijsq=dijsq+rij(m)**2
100   continue
      do 200 m=1,3
      B(m,1,NOINT)=-rij(m)/gsqrt(dijsq)
      B(m,2,NOINT)=-B(m,1,NOINT)
200   continue
      
      return
      
      end
C* :1 * 
      
