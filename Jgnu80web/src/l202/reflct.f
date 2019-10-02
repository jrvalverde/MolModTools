
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 reflct"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "reflct.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "reflct.web"
      subroutine reflct(MAXAP3,A,B,NATOMS,T,IXYZ)
      implicit none
      double precision A,B,one,T,zero
      integer i,IXYZ,j,MAXAP3,NATOMS
      dimension T(3,3),A(*),B(*)
      data zero,one/0.0D0,1.0D0/
      
      
      
      
      
      
      do 100 i=1,3
      do 50 j=1,3
      T(i,j)=zero
50    continue
      T(i,i)=one
100   continue
      T(IXYZ,IXYZ)=-one
      call tform(MAXAP3,T,A,B,NATOMS)
      return
      
      end
C* :1 * 
      
