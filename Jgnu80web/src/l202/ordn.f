
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ordn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ordn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "ordn.web"
      subroutine ordn(MAXAP3,A,B,ASET,ATMCHG,NPOP,NSET,NATOMS,NORDER,IDU
     &MP)
      implicit none
      double precision A,ASET,ATMCHG,B,curpx,curpy,direct,four,gabs,gata
     &n,one,phi,pi,t,theta,Tol2,Toler,zero
      integer IDUMP,In,Iout,Ipunch,itry,ixyz,key,MAXAP3,NATOMS,NORDER,NP
     &OP,NSET,numatm
      integer orkey
      dimension A(MAXAP3,3),B(MAXAP3,3)
      dimension ASET(*),ATMCHG(*),NPOP(*),NSET(*)
      dimension t(3,3)
      common/tol/Toler,Tol2
      common/io/In,Iout,Ipunch
      data zero,one,four/0.0D0,1.0D0,4.0D0/
      
      
      
      
      
      
      
99001 format(1x,'ORDN-- KEY ATOM:',i3)
      
      pi=four*gatan(one)
      numatm=NATOMS+3
      theta=pi/NORDER
      key=orkey(MAXAP3,NATOMS,A,ATMCHG,NSET,NPOP,ASET)
      if(IDUMP.NE.0)write(Iout,99001)key
      curpy=A(key,2)
      curpx=A(key,1)
      
      
      if(curpy.LE.zero)then
      call rotate(MAXAP3,A,B,numatm,t,3,pi)
      call move(MAXAP3,B,A,numatm)
      curpx=A(key,1)
      curpy=A(key,2)
      endif
      
      
      itry=0
      direct=-dsign(one,curpx)
100   phi=direct*theta
      call rotate(MAXAP3,A,B,numatm,t,3,phi)
      itry=itry+1
      if(gabs(curpy-B(key,2)).LT.Toler)then
      
      if(B(key,1).GE.zero)call move(MAXAP3,B,A,numatm)
      elseif(curpy.GT.B(key,2))then
      
      if(itry.LE.1)then
      direct=-direct
      goto 100
      endif
      else
      call move(MAXAP3,B,A,numatm)
      curpy=A(key,2)
      if(gabs(A(key,1)).GE.Toler)goto 100
      endif
      
      
      call orptst(MAXAP3,A,NATOMS,ixyz)
      if(ixyz.EQ.3)call oraxis(MAXAP3,A,B,NATOMS,ATMCHG,1)
      return
      
      end
C* :1 * 
      
