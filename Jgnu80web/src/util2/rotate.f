
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rotate"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rotate.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "rotate.web"
      subroutine rotate(MAXAP3,A,B,NATOMS,T,IXYZ,THETA)
      implicit none
      double precision A,B,c,gcos,gsin,one,s,T,THETA,zero
      integer i1,i2,i3,IXYZ,MAXAP3,NATOMS
      dimension T(3,3),A(*),B(*)
      data zero,one/0.0D0,1.0D0/
      
      
      
      
      
      
      i1=IXYZ
      i2=1+mod(i1,3)
      i3=1+mod(i2,3)
      s=gsin(THETA)
      c=gcos(THETA)
      T(i1,i1)=one
      T(i1,i2)=zero
      T(i1,i3)=zero
      T(i2,i1)=zero
      T(i2,i2)=c
      T(i2,i3)=s
      T(i3,i1)=zero
      T(i3,i2)=-s
      T(i3,i3)=c
      call tform(MAXAP3,T,A,B,NATOMS)
      return
      
      end
C* :1 * 
      
