
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 findv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "findv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "findv.web"
      subroutine findv(MAXAP3,A,B,D,NATOMS,NPOP,NSET,ATMCHG,ITST)
      implicit none
      double precision A,ATMCHG,B,D,gabs,gatan,half,halfpi,one,t,theta,T
     &ol2,Toler,two,x,y
      integer iat,iattop,iset,ITST,j1,jat,MAXAP3,NATOMS,NPOP,NSET,numatm
     &,numset
      dimension A(MAXAP3,3),NSET(*),B(*),D(*),NPOP(*),ATMCHG(*)
      dimension t(3,3)
      common/tol/Toler,Tol2
      data half,one,two/0.5D0,1.0D0,2.0D0/
      
      
      
      
      
      
      
      numatm=NATOMS+3
      halfpi=two*gatan(one)
      
      
      call cirset(MAXAP3,NATOMS,A,ATMCHG,3,NSET,NPOP,D,numset)
      
      iset=1
      iattop=NATOMS-1
      do 100 iat=1,iattop
      if(NSET(iat).EQ.iset)goto 200
      
100   continue
      ITST=0
      return
      
200   j1=iat+1
      do 300 jat=j1,NATOMS
      if(NSET(jat).EQ.iset)then
      x=(A(iat,1)+A(jat,1))*half
      y=(A(iat,2)+A(jat,2))*half
      if(gabs(x).LE.Toler.AND.gabs(y).LE.Toler)then
      x=half*A(jat,1)
      y=half*A(jat,2)
      endif
      theta=halfpi
      if(gabs(y).GT.Toler)theta=-gatan(x/y)
      call rotate(MAXAP3,A,B,numatm,t,3,theta)
      call reflct(MAXAP3,B,D,NATOMS,t,1)
      call equiv(MAXAP3,B,D,ATMCHG,NATOMS,ITST)
      if(ITST.NE.0)then
      call move(MAXAP3,B,A,numatm)
      ITST=1
      return
      endif
      endif
      
300   continue
      ITST=0
      return
      
      end
C* :1 * 
      
