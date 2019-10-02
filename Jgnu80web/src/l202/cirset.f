
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cirset"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cirset.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "cirset.web"
      subroutine cirset(MAXAP3,NATOMS,A,ATMCHG,IXYZ,NSET,NPOP,ASET,NUMSE
     &T)
      implicit none
      double precision A,ad,an,ap,ASET,ATMCHG,gabs,gsqrt,q2,q3,Tol2,Tole
     &r,zero
      integer i2,i3,iat,iattop,iset,IXYZ,j1,jat,MAXAP3,NATOMS,NPOP,NSET,
     &NUMSET
      dimension A(MAXAP3,3),NSET(*),NPOP(*),ASET(MAXAP3,3),ATMCHG(*)
      common/tol/Toler,Tol2
      data zero/0.0D0/
      
      
      
      
      
      
      
      
      i2=1+mod(IXYZ,3)
      i3=1+mod(i2,3)
      
      
      do 100 iat=1,NATOMS
      ASET(iat,1)=ATMCHG(iat)
      ASET(iat,2)=A(iat,IXYZ)
      q2=A(iat,i2)
      q3=A(iat,i3)
      ASET(iat,3)=gsqrt(q2*q2+q3*q3)
100   continue
      
      
      do 200 iat=1,NATOMS
      if(gabs(ASET(iat,3)).LE.Toler)then
      NSET(iat)=0
      ASET(iat,1)=zero
      endif
200   continue
      
      
      iattop=NATOMS-1
      iset=0
      do 300 iat=1,iattop
      if(ASET(iat,1).NE.zero)then
      iset=iset+1
      NSET(iat)=iset
      NPOP(iset)=1
      an=ASET(iat,1)
      ap=ASET(iat,2)
      ad=ASET(iat,3)
      ASET(iat,1)=zero
      j1=iat+1
      do 220 jat=j1,NATOMS
      if(gabs(ASET(jat,1)-an).LE.Tol2.AND.gabs(ASET(jat,2)-ap).LE.Toler.
     &AND.gabs(ASET(jat,3)-ad).LE.Toler)then
      NSET(jat)=iset
      NPOP(iset)=NPOP(iset)+1
      ASET(jat,1)=zero
      endif
220   continue
      endif
300   continue
      NUMSET=iset
      
      
      do 400 iat=1,NATOMS
      ASET(iat,1)=ATMCHG(iat)
400   continue
      return
      
      end
C* :1 * 
      
