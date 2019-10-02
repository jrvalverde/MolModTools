
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 orplan"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "orplan.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "orplan.web"
      subroutine orplan(MAXAP3,A,B,ATMCHG,NUMATM,PRMOM,PRAXES,IXYZ)
      implicit none
      double precision A,ATMCHG,B,gabs,gatan,halfpi,one,PRAXES,PRMOM,t,t
     &heta,Tol2,Toler,tst1,tst2,tst3,two,v2,v3
      integer i2,i3,iax,itop,IXYZ,izyx,MAXAP3,natoms,NUMATM
      dimension PRMOM(3),PRAXES(3,3)
      dimension t(3,3),A(*),B(*),ATMCHG(*)
      common/tol/Toler,Tol2
      data one,two/1.0D0,2.0D0/
      
      
      
      
      
      
      
      halfpi=two*gatan(one)
      natoms=NUMATM-3
      call secmom(MAXAP3,natoms,A,ATMCHG,PRMOM,PRAXES)
      
      
      itop=0
      tst1=PRMOM(2)-PRMOM(3)
      tst2=PRMOM(1)-PRMOM(3)
      tst3=PRMOM(1)-PRMOM(2)
      if(gabs(tst1).LT.Tol2)itop=itop+1
      if(gabs(tst2).LT.Tol2)itop=itop+1
      if(gabs(tst3).LT.Tol2)itop=itop+1
      if(itop.NE.3)itop=itop+1
      if(itop.NE.1)return
      
      i2=1+mod(IXYZ,3)
      i3=1+mod(i2,3)
      
      if(IXYZ.EQ.2)then
      
      v2=PRAXES(1,3)
      v3=PRAXES(3,3)
      elseif(IXYZ.EQ.3)then
      
      v2=PRAXES(1,2)
      v3=PRAXES(2,2)
      else
      
      v2=PRAXES(2,3)
      v3=PRAXES(3,3)
      endif
      
      if(gabs(v2).GE.Toler)then
      theta=halfpi
      if(gabs(v3).GT.Toler)theta=gatan(v2/v3)
      call rotate(MAXAP3,A,B,NUMATM,t,IXYZ,theta)
      call move(MAXAP3,B,A,NUMATM)
      endif
      
      
      call orptst(MAXAP3,A,natoms,izyx)
      if(izyx.EQ.0)then
      
      
      iax=3
      if(IXYZ.EQ.3)iax=2
      call oraxis(MAXAP3,A,B,natoms,ATMCHG,iax)
      if(IXYZ.EQ.2)call oraxis(MAXAP3,A,B,natoms,ATMCHG,2)
      return
      
      
      elseif(izyx.EQ.IXYZ)then
      call oraxis(MAXAP3,A,B,natoms,ATMCHG,i3)
      call oraxis(MAXAP3,A,B,natoms,ATMCHG,i2)
      return
      endif
      
      
      iax=i2
      if(i2.EQ.izyx)iax=i3
      call oraxis(MAXAP3,A,B,natoms,ATMCHG,iax)
      return
      
      end
C* :1 * 
      
