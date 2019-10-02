
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 secmom"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "secmom.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "secmom.web"
      subroutine secmom(MAXAP3,NATOMS,A,ATMCHG,EIGVAL,EIGVEC)
      implicit none
      double precision A,an,ATMCHG,e,e2,EIGVAL,EIGVEC,t,x,y,z,zero
      integer i,iat,MAXAP3,NATOMS
      dimension A(MAXAP3,3),ATMCHG(*),EIGVAL(*),EIGVEC(*)
      dimension t(6),e(9),e2(18)
      data zero/0.0D0/
      
      
      
      
      
      
      do 100 i=1,6
      t(i)=zero
100   continue
      
      do 200 iat=1,NATOMS
      an=ATMCHG(iat)
      x=A(iat,1)
      y=A(iat,2)
      z=A(iat,3)
      t(1)=t(1)+an*(y*y+z*z)
      t(3)=t(3)+an*(x*x+z*z)
      t(6)=t(6)+an*(x*x+y*y)
      t(2)=t(2)-an*x*y
      t(4)=t(4)-an*x*z
      t(5)=t(5)-an*y*z
200   continue
      call diagd(t,EIGVEC,EIGVAL,3,e,e2,3,.FALSE.)
      return
      
      end
C* :1 * 
      
