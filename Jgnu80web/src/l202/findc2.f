
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 findc2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "findc2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "findc2.web"
      subroutine findc2(MAXAP3,A,B,ASET,NPOP,NSET,ATMCHG,NATOMS,ITST)
      implicit none
      double precision A,ani,ASET,ATMCHG,B,disi,gabs,gatan,half,halfpi,o
     &ne,pi,proi,t,theta,Tol2,Toler,two,x,xi
      double precision y,yi
      integer iat,iattop,iset,ITST,j1,jat,jset,MAXAP3,NATOMS,NPOP,NSET,n
     &umatm,numset
      dimension A(MAXAP3,3),NSET(*),ASET(MAXAP3,3)
      dimension t(3,3),B(*),ATMCHG(*),NPOP(*)
      common/tol/Toler,Tol2
      data half,one,two/0.5D0,1.0D0,2.0D0/
      
      
      
      
      
      
      
      numatm=NATOMS+3
      halfpi=two*gatan(one)
      pi=two*halfpi
      
      call cirset(MAXAP3,NATOMS,A,ATMCHG,3,NSET,NPOP,ASET,numset)
      
      
      iattop=NATOMS-1
      do 200 iset=1,numset
      do 50 iat=1,NATOMS
      if(NSET(iat).EQ.iset)goto 100
50    continue
      goto 200
      
100   if(gabs(ASET(iat,2)).LE.Toler)then
      j1=iat+1
      do 120 jat=j1,NATOMS
      if(NSET(jat).EQ.iset)then
      x=(A(iat,1)+A(jat,1))*half
      y=(A(iat,2)+A(jat,2))*half
      theta=halfpi
      if(gabs(y).GT.Toler)theta=-gatan(x/y)
      call rotate(MAXAP3,A,B,numatm,t,3,theta)
      call rotate(MAXAP3,B,ASET,NATOMS,t,2,pi)
      call equiv(MAXAP3,B,ASET,ATMCHG,NATOMS,ITST)
      if(ITST.NE.0)then
      call move(MAXAP3,B,A,numatm)
      return
      endif
      endif
      
120   continue
      endif
200   continue
      
      
      call cirset(MAXAP3,NATOMS,A,ATMCHG,3,NSET,NPOP,ASET,numset)
      iset=1
      do 300 iat=1,NATOMS
      if(NSET(iat).EQ.iset)goto 400
300   continue
      ITST=0
      return
      
400   proi=ASET(iat,2)
      ani=ASET(iat,1)
      disi=ASET(iat,3)
      xi=A(iat,1)
      yi=A(iat,2)
      j1=iset+1
      do 600 jset=j1,numset
      do 450 jat=1,NATOMS
      if(NSET(jat).EQ.jset)goto 500
450   continue
      ITST=0
      return
      
500   if(gabs(proi+ASET(jat,2)).LE.Toler.AND.gabs(ani-ASET(jat,1)).LE.To
     &l2.AND.gabs(disi-ASET(jat,3)).LE.Toler)goto 700
      
600   continue
      ITST=0
      return
      
700   do 800 jat=1,NATOMS
      if(NSET(jat).EQ.jset)then
      x=(xi+A(jat,1))*half
      y=(yi+A(jat,2))*half
      theta=halfpi
      if(gabs(y).GT.Tol2)theta=-gatan(x/y)
      call rotate(MAXAP3,A,B,numatm,t,3,theta)
      call rotate(MAXAP3,B,ASET,NATOMS,t,2,pi)
      call equiv(MAXAP3,B,ASET,ATMCHG,NATOMS,ITST)
      if(ITST.NE.0)then
      call move(MAXAP3,B,A,numatm)
      return
      endif
      endif
      
800   continue
      ITST=0
      return
      
      end
C* :1 * 
      
