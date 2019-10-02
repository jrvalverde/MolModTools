
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ord2h"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ord2h.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "ord2h.web"
      subroutine ord2h(MAXAP3,A,B,NATOMS,ATMCHG,IAN)
      implicit none
      double precision A,ATMCHG,B,gatan,halfpi,one,pi,t,two
      integer IAN,itst,ixyz,MAXAP3,NATOMS,numatm
      integer ornax
      dimension t(3,3)
      dimension A(*),B(*),ATMCHG(*),IAN(*)
      data one,two/1.0D0,2.0D0/
      
      
      
      
      
      
      
      call orptst(MAXAP3,A,NATOMS,ixyz)
      if(ixyz.EQ.0)return
      
      
      if(ixyz.NE.1)call oryz(MAXAP3,A,B,NATOMS,ATMCHG,ixyz)
      
      
      itst=ornax(MAXAP3,A,NATOMS,IAN)
      if(itst.NE.2)return
      numatm=NATOMS+3
      halfpi=two*gatan(one)
      pi=two*halfpi
      call rotate(MAXAP3,A,B,numatm,t,1,-halfpi)
      call rotate(MAXAP3,B,A,numatm,t,3,-pi)
      return
      
      end
C* :1 * 
      
