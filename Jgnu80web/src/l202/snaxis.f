
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 snaxis"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "snaxis.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "snaxis.web"
      subroutine snaxis(MAXAP3,N,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D)
      implicit none
      double precision A,B,D,eight,gatan,gfloat,one,phi,r,s,t,theta,TRAN
     &S,zero
      integer iop,MAXAP3,MAXOP,N,NATOMS,NOP,NPERM,num
      dimension r(3,3),s(3,3),t(3,3)
      dimension TRANS(*),NPERM(*),A(*),B(*),D(*)
      data zero,one,eight/0.0D0,1.0D0,8.0D0/
      
      
      
      
      
      
      phi=eight*gatan(one)/gfloat(N)
      theta=zero
      num=N-1
      do 100 iop=1,num
      theta=theta+phi
      call rotate(MAXAP3,A,B,NATOMS,t,3,theta)
      if(mod(iop,2).EQ.1)then
      
      call reflct(MAXAP3,B,D,NATOMS,r,3)
      call matpac(r,t,s,3,3,1)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,s,TRANS,NPERM,A,D)
      else
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,A,B)
      endif
      
100   continue
      return
      
      end
C* :1 * 
      
