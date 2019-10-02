
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 frcnn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "frcnn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "frcnn.web"
      subroutine frcnn(NATOMS,ATMCHG,C,FXYZ)
      implicit none
      double precision ab,ATMCHG,C,fn,FXYZ,gsqrt,r,zero
      integer i,i1,iaind,j,jaind,k,NATOMS
      dimension ATMCHG(*),C(*),FXYZ(*)
      dimension ab(3)
      data zero/0.D0/
      
      
      
      
      
      
      
      do 100 i=1,NATOMS
      i1=i-1
      if(i.NE.1)then
      iaind=3*(i-1)
      do 40 j=1,i1
      jaind=3*(j-1)
      r=zero
      do 10 k=1,3
      ab(k)=C(k+iaind)-C(k+jaind)
      r=r+ab(k)**2
10    continue
      fn=(ATMCHG(i)*ATMCHG(j))/(r*gsqrt(r))
      do 20 k=1,3
      r=ab(k)*fn
      FXYZ(k+iaind)=FXYZ(k+iaind)-r
      FXYZ(k+jaind)=FXYZ(k+jaind)+r
20    continue
40    continue
      endif
100   continue
      return
      
      end
C* :1 * 
      
