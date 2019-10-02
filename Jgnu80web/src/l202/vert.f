
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 vert"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "vert.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "vert.web"
      subroutine vert(MAXAP3,N,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D)
      implicit none
      double precision A,B,chi,D,eight,gatan,gfloat,half,one,phi,q,r,s,t
     &,theta,TRANS
      integer iop,MAXAP3,MAXOP,N,NATOMS,NOP,NPERM
      dimension q(3,3),r(3,3),s(3,3),t(3,3)
      dimension TRANS(*),NPERM(*),A(*),B(*),D(*)
      data half,one,eight/0.5D0,1.0D0,8.0D0/
      
      
      
      
      
      
      phi=eight*gatan(one)/gfloat(N)
      if(mod(N,2).EQ.0)phi=half*phi
      theta=-phi
      do 100 iop=1,N
      theta=theta+phi
      call rotate(MAXAP3,A,B,NATOMS,q,3,theta)
      call reflct(MAXAP3,B,D,NATOMS,r,1)
      chi=-theta
      call rotate(MAXAP3,D,B,NATOMS,s,3,chi)
      call matpac(r,q,t,3,3,1)
      call matpac(s,t,q,3,3,1)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,q,TRANS,NPERM,A,B)
100   continue
      return
      
      end
C* :1 * 
      
