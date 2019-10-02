
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 orc2v"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "orc2v.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "orc2v.web"
      subroutine orc2v(MAXAP3,A,B,NATOMS,ATMCHG)
      implicit none
      double precision A,ATMCHG,B,e,gabs,gatan,gsqrt,halfpi,heavy,one,on
     &z,t,Tol2,Toler,two,x,y
      integer iat,ixyz,MAXAP3,NATOMS,numatm,numxz,numyz
      dimension A(MAXAP3,3),B(*),ATMCHG(*)
      dimension t(3,3),e(3)
      common/tol/Toler,Tol2
      data one,two/1.0D0,2.0D0/
      data heavy/2.D0/
      
      
      
      
      
      
      
      halfpi=two*gatan(one)
      numatm=NATOMS+3
      
      
      call orptst(MAXAP3,A,NATOMS,ixyz)
      if(ixyz.EQ.2)then
      
      
      call oryz(MAXAP3,A,B,NATOMS,ATMCHG,ixyz)
      return
      elseif(ixyz.EQ.1)then
      
      
      call orplan(MAXAP3,A,B,ATMCHG,NATOMS+3,e,t,1)
      return
      else
      
      
      numyz=0
      numxz=0
      do 50 iat=1,NATOMS
      x=A(iat,1)
      y=A(iat,2)
      onz=gsqrt(x*x+y*y)
      if(onz.GE.Toler)then
      if(gabs(x).LT.Toler)numyz=numyz+1
      if(gabs(y).LT.Toler)numxz=numxz+1
      endif
50    continue
      if(numyz.LT.numxz)then
      elseif(numyz.EQ.numxz)then
      
      
      numyz=0
      numxz=0
      do 60 iat=1,NATOMS
      if(ATMCHG(iat).GT.heavy)then
      x=A(iat,1)
      y=A(iat,2)
      onz=gsqrt(x*x+y*y)
      if(onz.GE.Toler)then
      if(gabs(x).LT.Toler)numyz=numyz+1
      if(gabs(y).LT.Toler)numxz=numxz+1
      endif
      endif
60    continue
      if(numyz.LT.numxz)then
      elseif(numyz.EQ.numxz)then
      
      
      do 70 iat=1,NATOMS
      x=A(iat,1)
      y=A(iat,2)
      onz=gsqrt(x*x+y*y)
      if(onz.GE.Toler)then
      if(gabs(x).LT.Toler)goto 200
      if(gabs(y).LT.Toler)goto 100
      endif
70    continue
      call orplan(MAXAP3,A,B,ATMCHG,numatm,e,t,3)
      return
      else
      goto 200
      endif
      else
      goto 200
      endif
      
100   call rotate(MAXAP3,A,B,numatm,t,3,halfpi)
      call move(MAXAP3,B,A,numatm)
      endif
      
200   call oraxis(MAXAP3,B,A,NATOMS,ATMCHG,2)
      return
      
      end
C* :1 * 
      
