
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rotmat"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rotmat.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "rotmat.web"
      subroutine rotmat(PHI,THETA,CHI,T)
      implicit none
      double precision CHI,cosc,cosp,cost,gcos,gsin,PHI,sinc,sinp,sint,T
     &,THETA
      dimension T(3,3)
      
      
      
      
      
      cosp=gcos(PHI)
      sinp=gsin(PHI)
      cost=gcos(THETA)
      sint=gsin(THETA)
      cosc=gcos(CHI)
      sinc=gsin(CHI)
      
      T(1,1)=cosp*cost*cosc-sinp*sinc
      T(2,1)=-(cosp*cost*sinc+sinp*cosc)
      T(3,1)=cosp*sint
      
      T(1,2)=sinp*cost*cosc+cosp*sinc
      T(2,2)=-(sinp*cost*sinc-cosp*cosc)
      T(3,2)=sinp*sint
      
      T(1,3)=-sint*cosc
      T(2,3)=sint*sinc
      T(3,3)=cost
      
      return
      
      end
C* :1 * 
      
