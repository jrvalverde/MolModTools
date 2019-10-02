
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 orkey"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "orkey.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "orkey.web"
      integer function orkey(MAXAP3,NATOMS,A,ATMCHG,NSET,NPOP,ASET)
      implicit none
      double precision A,ASET,ATMCHG,gabs,p,small,test,Tol2,Toler,zero
      integer iat,iset,jat,jset,MAXAP3,NATOMS,NPOP,NSET,numset
      dimension NSET(*),ASET(MAXAP3,3)
      dimension A(*),ATMCHG(*),NPOP(*)
      common/tol/Toler,Tol2
      data zero/0.0D0/
      
      
      
      
      
      
      
      call cirset(MAXAP3,NATOMS,A,ATMCHG,3,NSET,NPOP,ASET,numset)
      iset=99
      do 100 jat=1,NATOMS
      jset=NSET(jat)
      p=ASET(jat,2)
      if(jset.NE.0.AND.jset.NE.iset)then
      if(iset.NE.99)then
      
      test=gabs(small)-gabs(p)
      if(gabs(test).LE.Toler)then
      test=p-small
      if(gabs(test).LE.Toler)then
      test=ASET(iat,3)-ASET(jat,3)
      if(gabs(test).LE.Toler)test=ASET(iat,1)-ASET(jat,1)
      endif
      endif
      if(test.GE.zero)then
      iset=jset
      small=p
      iat=jat
      endif
      else
      iset=jset
      iat=jat
      small=p
      endif
      endif
100   continue
      orkey=iat
      return
      
      end
C* :1 * 
      
