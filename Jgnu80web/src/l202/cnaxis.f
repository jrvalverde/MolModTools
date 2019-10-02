
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cnaxis"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cnaxis.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "cnaxis.web"
      subroutine cnaxis(MAXAP3,N,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B)
      implicit none
      double precision A,B,eight,gatan,gfloat,one,phi,t,theta,TRANS,zero
      integer iop,MAXAP3,MAXOP,N,NATOMS,NOP,NPERM,num
      dimension t(3,3)
      dimension TRANS(*),NPERM(*),A(*),B(*)
      data zero,one,eight/0.0D0,1.0D0,8.0D0/
      
      
      
      
      
      
      phi=eight*gatan(one)/gfloat(N)
      theta=zero
      num=N-1
      do 100 iop=1,num
      theta=theta+phi
      call rotate(MAXAP3,A,B,NATOMS,t,3,theta)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,A,B)
100   continue
      return
      
      end
C* :1 * 
      
