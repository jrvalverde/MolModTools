
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 center"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "center.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "center.web"
      subroutine center(MAXAP3,NATOMS,A,ATMCHG,V)
      implicit none
      double precision A,ATMCHG,totwt,V,wt,zero
      integer i,iat,MAXAP3,NATOMS
      dimension A(MAXAP3,3),ATMCHG(*),V(3)
      data zero/0.0D0/
      
      
      
      
      
      
      do 100 i=1,3
      V(i)=zero
100   continue
      totwt=zero
      
      do 200 iat=1,NATOMS
      wt=ATMCHG(iat)
      totwt=totwt+wt
      V(1)=V(1)+wt*A(iat,1)
      V(2)=V(2)+wt*A(iat,2)
      V(3)=V(3)+wt*A(iat,3)
200   continue
      
      V(1)=V(1)/totwt
      V(2)=V(2)/totwt
      V(3)=V(3)/totwt
      return
      
      end
C* :1 * 
      
