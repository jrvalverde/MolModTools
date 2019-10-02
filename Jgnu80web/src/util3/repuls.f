
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repuls"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repuls.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "repuls.web"
      subroutine repuls(NATOMS,ATMCHG,C,REP)
      implicit none
      double precision anij,ATMCHG,C,gsqrt,REP,rij,xi,yi,zero,zi
      integer i,idx,In,Iout,ip1,Ipunch,j,jdx,natm1,NATOMS
      dimension ATMCHG(*),C(*)
      common/io/In,Iout,Ipunch
      data zero/0.0D0/
      
      
      
      REP=zero
      natm1=NATOMS-1
      if(NATOMS.LE.1)return
      
      do 100 i=1,natm1
      ip1=i+1
      idx=3*(i-1)
      xi=C(idx+1)
      yi=C(idx+2)
      zi=C(idx+3)
      do 50 j=ip1,NATOMS
      anij=ATMCHG(i)*ATMCHG(j)
      jdx=3*(j-1)
      rij=gsqrt((xi-C(jdx+1))**2+(yi-C(jdx+2))**2+(zi-C(jdx+3))**2)
      REP=REP+anij/rij
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
