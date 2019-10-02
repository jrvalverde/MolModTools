
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 invert"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "invert.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "invert.web"
      subroutine invert(MAXAP3,A,B,NATOMS,T)
      implicit none
      double precision A,B,one,T,zero
      integer i,j,MAXAP3,NATOMS
      dimension T(3,3),A(*),B(*)
      data zero,one/0.0D0,1.0D0/
      
      
      
      
      
      
      do 100 i=1,3
      do 50 j=1,3
      T(i,j)=zero
50    continue
100   continue
      T(1,1)=-one
      T(2,2)=-one
      T(3,3)=-one
      call tform(MAXAP3,T,A,B,NATOMS)
      return
      
      end
C* :1 * 
      
