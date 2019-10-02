
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 c2axes"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "c2axes.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "c2axes.web"
      subroutine c2axes(MAXAP3,N,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D,THET
     &A0)
      implicit none
      double precision A,B,chi,D,four,gatan,gfloat,half,one,phi,pi,q,r,s
     &,t,theta,THETA0,TRANS,two
      integer iop,MAXAP3,MAXOP,N,NATOMS,NOP,NPERM
      dimension q(3,3),r(3,3),s(3,3),t(3,3)
      dimension A(*),B(*),D(*),TRANS(*),NPERM(*)
      data half,one,two,four/0.5D0,1.0D0,2.0D0,4.0D0/
      
      
      
      
      
      
      pi=four*gatan(one)
      phi=two*pi/gfloat(N)
      if(mod(N,2).EQ.0)phi=half*phi
      theta=THETA0-phi
      do 100 iop=1,N
      theta=theta+phi
      call rotate(MAXAP3,A,B,NATOMS,q,3,theta)
      call rotate(MAXAP3,B,D,NATOMS,r,2,pi)
      chi=-theta
      call rotate(MAXAP3,D,B,NATOMS,s,3,chi)
      call matpac(r,q,t,3,3,1)
      call matpac(s,t,q,3,3,1)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,q,TRANS,NPERM,A,B)
100   continue
      return
      
      end
C* :1 * 
      
