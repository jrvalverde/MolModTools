
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tetra"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tetra.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "tetra.web"
      subroutine tetra(MAXAP3,NGRP,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D)
      implicit none
      double precision A,abc,B,D,gatan,gfloat,halfpi,one,phi3,pi,piovr4,
     &q,r,s,t,theta,three,TRANS,two,v
      double precision zero
      integer i,iop,iord,ixyz,j,k,l,MAXAP3,MAXOP,NATOMS,NGRP,NOP,NPERM
      dimension NGRP(4)
      dimension v(3),q(3,3),r(3,3),s(3,3),t(3,3)
      dimension TRANS(*),NPERM(*),A(*),B(*),D(*)
      data zero,one,two,three/0.0D0,1.0D0,2.0D0,3.0D0/
      
      
      
      
      
      
      
      piovr4=gatan(one)
      halfpi=two*piovr4
      pi=two*halfpi
      phi3=two*pi/three
      
      
      call move(MAXAP3,A,D,NATOMS+3)
      v(3)=one
      do 100 i=1,3,2
      v(1)=gfloat(2-i)*one
      do 50 j=1,3,2
      v(2)=gfloat(2-j)*one
      theta=zero
      do 20 iop=1,2
      call put(MAXAP3,A,B,q,v,NATOMS,3)
      theta=theta+phi3
      call rotate(MAXAP3,A,B,NATOMS,r,3,theta)
      call matpac(r,q,s,3,3,1)
      do 10 k=1,3
      do 5 l=1,k
      if(k.NE.l)then
      abc=q(k,l)
      q(k,l)=q(l,k)
      q(l,k)=abc
      endif
5     continue
10    continue
      call matpac(q,s,t,3,3,1)
      call tform(MAXAP3,q,B,A,NATOMS)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,D,A)
      call move(MAXAP3,D,A,NATOMS)
20    continue
50    continue
100   continue
      
      
      do 200 ixyz=1,3
      call rotate(MAXAP3,D,B,NATOMS,t,ixyz,pi)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,D,B)
200   continue
      
      
      if(NGRP(2).EQ.iord('D'))then
      do 250 ixyz=1,3
      theta=-halfpi
      do 220 iop=1,2
      theta=theta+pi
      call rotate(MAXAP3,D,B,NATOMS,t,ixyz,theta)
      call reflct(MAXAP3,B,A,NATOMS,s,ixyz)
      call matpac(t,s,r,3,3,1)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,r,TRANS,NPERM,D,A)
220   continue
250   continue
      
      
      do 300 ixyz=1,3
      do 260 iop=1,3
      if(iop.NE.ixyz)then
      call rotate(MAXAP3,D,B,NATOMS,q,ixyz,piovr4)
      call reflct(MAXAP3,B,A,NATOMS,r,iop)
      call matpac(r,q,s,3,3,1)
      call rotate(MAXAP3,A,B,NATOMS,r,ixyz,-piovr4)
      call matpac(r,s,t,3,3,1)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,D,B)
      endif
260   continue
300   continue
      endif
      
      call move(MAXAP3,D,A,NATOMS+3)
      return
      
      end
C* :1 * 
      
