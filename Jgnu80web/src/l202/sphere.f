
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sphere"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sphere.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "sphere.web"
      subroutine sphere(MAXAP3,NATOMS,A,B,D,ATMCHG,NSET,NPOP,NORDER,IDUM
     &P)
      implicit none
      double precision A,ATMCHG,B,centr,cmax,curz,D,gabs,gatan,half,half
     &pi,one,pi,save,save2,savez,t,theta,Tol,Toler
      double precision tst,two,x,y,z,zero
      integer i,i2,iat,IDUMP,ihop,In,ioff,Iout,Ipunch,iset,itop,itst,ixy
     &z,j,j1,j2,jat,k,k1,kat
      integer key,MAXAP3,moff,mpop1,mpop2,mset,NATOMS,NORDER,NPOP,NSET,n
     &um3,numatm,numset
      dimension A(MAXAP3,3),B(MAXAP3,3),D(MAXAP3,3),ATMCHG(*),NPOP(*),NS
     &ET(*)
      dimension centr(3),save(3),save2(3),t(3,3)
      common/tol/Toler,Tol
      common/io/In,Iout,Ipunch
      data half,one,two/0.5D0,1.0D0,2.0D0/
      
      
      
      
      
      
      
99001 format(1x,'SPHERE-- KEY ATOM',i4)
      
      NORDER=0
      numatm=NATOMS+3
      halfpi=two*gatan(one)
      pi=two*halfpi
      
      
      call sphset(MAXAP3,NATOMS,A,ATMCHG,NSET,NPOP,D,numset)
      
      
      key=NATOMS
      itop=NPOP(1)
      do 100 i=1,itop
      key=min0(key,NSET(i))
100   continue
      if(IDUMP.NE.0)write(Iout,99001)key
      
      
      ioff=0
      mpop1=NATOMS
      do 200 iset=1,numset
      mpop2=min0(mpop1,NPOP(iset))
      if(mpop2.NE.mpop1)then
      mpop1=mpop2
      mset=iset
      moff=ioff
      endif
      ioff=ioff+NPOP(iset)
200   continue
      
      
      num3=0
      i2=mpop1-2
      j2=mpop1-1
      do 300 i=1,i2
      iat=NSET(moff+i)
      j1=i+1
      do 250 j=j1,j2
      jat=NSET(moff+j)
      k1=j+1
      do 220 k=k1,mpop1
      kat=NSET(moff+k)
      
      
      if(NORDER.LE.3)then
      call move(MAXAP3,A,D,numatm)
      call tstc5(MAXAP3,D,B,NATOMS,ATMCHG,iat,jat,kat,centr,itst)
      if(itst.EQ.0)then
      
      call move(MAXAP3,A,D,numatm)
      call tstc4(MAXAP3,D,B,NATOMS,ATMCHG,iat,jat,kat,centr,itst)
      if(itst.NE.0)then
      NORDER=4
      call move(MAXAP3,D,A,numatm)
      
      
      elseif(num3.NE.2)then
      call move(MAXAP3,A,D,numatm)
      call tstc3(MAXAP3,D,B,NATOMS,ATMCHG,iat,jat,kat,centr,itst)
      if(itst.NE.0)then
      ihop=num3+1
      if(ihop.EQ.2)then
      
      num3=2
      save2(1)=centr(1)
      save2(2)=centr(2)
      save2(3)=centr(3)
      else
      
      NORDER=3
      num3=1
      save(1)=centr(1)
      save(2)=centr(2)
      save(3)=centr(3)
      endif
      endif
      endif
      else
      NORDER=5
      savez=gabs(D(key,3))
      save(1)=centr(1)
      save(2)=centr(2)
      save(3)=centr(3)
      endif
      
      elseif(NORDER.EQ.4)then
      
      
      call move(MAXAP3,A,D,numatm)
      call tstc4(MAXAP3,D,B,NATOMS,ATMCHG,iat,jat,kat,centr,itst)
      if(itst.NE.0)then
      if(gabs(centr(3)).LE.Toler)then
      x=centr(1)
      y=centr(2)
      theta=halfpi
      if(gabs(y).GT.Toler)theta=gatan(x/y)
      call rotate(MAXAP3,D,A,numatm,t,3,theta)
      goto 400
      endif
      endif
      else
      
      
      call move(MAXAP3,D,A,numatm)
      call tstc5(MAXAP3,D,B,NATOMS,ATMCHG,iat,jat,kat,centr,itst)
      if(itst.NE.0)then
      curz=gabs(D(key,3))
      if(gabs(curz-savez).GE.Toler)then
      if(savez.LE.curz)then
      savez=curz
      save(1)=centr(1)
      save(2)=centr(2)
      save(3)=centr(3)
      endif
      endif
      endif
      endif
      
220   continue
250   continue
300   continue
      
      
400   if(NORDER.EQ.0)return
      ihop=NORDER-2
      if(ihop.EQ.2)then
      elseif(ihop.EQ.3)then
      
      
      call put(MAXAP3,A,B,t,save,numatm,3)
      goto 600
      else
      
      
      centr(1)=half*(save(1)+save2(1))
      centr(2)=half*(save(2)+save2(2))
      centr(3)=half*(save(3)+save2(3))
      call put(MAXAP3,A,B,t,centr,numatm,3)
      B(1,1)=save(1)
      B(1,2)=save(2)
      B(1,3)=save(3)
      B(2,1)=save2(1)
      B(2,2)=save2(2)
      B(2,3)=save2(3)
      call put(MAXAP3,B,D,t,centr,2,3)
      save(1)=B(1,1)
      save(2)=B(1,2)
      save(3)=B(1,3)
      save2(1)=B(2,1)
      save2(2)=B(2,2)
      save2(3)=B(2,3)
      
      
      x=half*(save(1)-save2(2))
      y=half*(save(2)+save2(1))
      theta=halfpi
      if(gabs(y).GT.Toler)theta=gatan(x/y)
      call rotate(MAXAP3,A,B,numatm,t,3,theta)
      
      
      call move(MAXAP3,B,A,numatm)
      endif
      
      
      x=A(key,1)
      y=A(key,2)
      z=A(key,3)
      cmax=gabs(z)
      ixyz=3
      do 500 i=1,2
      tst=gabs(A(key,i))
      if(gabs(tst-cmax).GE.Toler)then
      if(cmax.LE.tst)then
      ixyz=i
      cmax=tst
      endif
      endif
500   continue
      if(ixyz.NE.3)then
      ixyz=iabs(ixyz-2)+1
      call rotate(MAXAP3,A,B,numatm,t,ixyz,halfpi)
      call move(MAXAP3,B,A,numatm)
      endif
600   if(A(key,3).GT.zero)return
      call rotate(MAXAP3,A,B,numatm,t,1,pi)
      call move(MAXAP3,B,A,numatm)
      return
      
      end
C* :1 * 
      
