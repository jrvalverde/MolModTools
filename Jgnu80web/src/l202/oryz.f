
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 oryz"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "oryz.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "oryz.web"
      subroutine oryz(MAXAP3,A,B,NATOMS,ATMCHG,IXYZ)
      implicit none
      double precision A,ATMCHG,B,e,gatan,halfpi,one,t,two
      integer IXYZ,MAXAP3,NATOMS,numatm
      dimension t(3,3),e(3),A(*),B(*),ATMCHG(*)
      data one,two/1.0D0,2.0D0/
      
      
      
      
      
      
      numatm=NATOMS+3
      halfpi=two*gatan(one)
      if(IXYZ.EQ.2)then
      
      call rotate(MAXAP3,A,B,numatm,t,3,halfpi)
      call move(MAXAP3,B,A,numatm)
      call orplan(MAXAP3,A,B,ATMCHG,numatm,e,t,1)
      return
      elseif(IXYZ.NE.3)then
      
      call orplan(MAXAP3,A,B,ATMCHG,numatm,e,t,1)
      return
      endif
      
      call rotate(MAXAP3,A,B,numatm,t,2,halfpi)
      call move(MAXAP3,B,A,numatm)
      call orplan(MAXAP3,A,B,ATMCHG,numatm,e,t,1)
      return
      
      end
C* :1 * 
      
