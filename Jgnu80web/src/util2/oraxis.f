
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 oraxis"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "oraxis.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "oraxis.web"
      subroutine oraxis(MAXAP3,A,B,NATOMS,ATMCHG,IXYZ)
      implicit none
      double precision A,ATMCHG,B,four,gabs,gatan,one,or3mom,pi,t,test,T
     &ol2,Toler,zero
      integer i2,iat,IXYZ,MAXAP3,NATOMS,numatm
      dimension A(MAXAP3,3),B(*),ATMCHG(*)
      dimension t(3,3)
      common/tol/Toler,Tol2
      data zero,one,four/0.0D0,1.0D0,4.0D0/
      
      
      
      
      
      
      
      pi=four*gatan(one)
      
      
      test=or3mom(MAXAP3,A,ATMCHG,NATOMS,IXYZ)
      if(gabs(test).LT.Toler)then
      
      
      test=zero
      do 50 iat=1,NATOMS
      test=test+A(iat,IXYZ)
50    continue
      if(gabs(test).LT.Toler)then
      
      
      do 60 iat=1,NATOMS
      test=A(iat,IXYZ)
      if(gabs(test).GT.Toler)goto 100
60    continue
      return
      else
      if(test.GT.zero)return
      goto 200
      endif
      
100   if(test.GT.zero)return
      elseif(test.GT.zero)then
      return
      endif
      
      
200   i2=1+mod(IXYZ,3)
      numatm=NATOMS+3
      call rotate(MAXAP3,A,B,numatm,t,i2,pi)
      call move(MAXAP3,B,A,numatm)
      return
      
      end
C* :1 * 
      
