
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 orcn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "orcn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "orcn.web"
      subroutine orcn(MAXAP3,A,B,D,ASET,ATMCHG,NPOP,NSET,NATOMS,IDUMP)
      implicit none
      double precision A,angmat,ASET,ATMCHG,B,bestx,besty,curang,D,flag,
     &gabs,gatan,halfpi,heavy,one,pi,savang,t,theta,Tol2
      double precision Toler,two,x,x1,y,y1,zero
      integer i,i2,iat,ibest,IDUMP,idx,In,Iout,Ipunch,ixyz,j,j1,j2,jat,k
     &ey,MAXAP3,maxhvy,msav,NATOMS,ncur
      integer nhvy,nmax,NPOP,nsav,NSET,numatm
      integer orkey
      dimension A(MAXAP3,3),B(MAXAP3,3),D(MAXAP3,3),ATMCHG(*)
      dimension angmat(435),savang(20),t(3,3),NPOP(*),ASET(*),NSET(*)
      common/io/In,Iout,Ipunch
      common/tol/Toler,Tol2
      data maxhvy/30/,msav/20/
      data heavy/2.D0/
      data zero,one,two,flag/0.0D0,1.0D0,2.0D0,100.D0/
      
      
      
      
      
      
      
99001 format(1x,'ORCN-- KEY ATOM ',i3)
99002 format(1x,'ORCN-- HEAVY ATOM MATRIX TRUNCATED AT ',i3)
      
      numatm=NATOMS+3
      halfpi=two*gatan(one)
      pi=two*halfpi
      key=orkey(MAXAP3,NATOMS,A,ATMCHG,NSET,NPOP,ASET)
      if(IDUMP.NE.0)write(Iout,99001)key
      
      
      nhvy=0
      idx=0
      do 100 iat=1,NATOMS
      if(ATMCHG(iat).GT.heavy)then
      x1=A(iat,1)
      y1=A(iat,2)
      j2=iat-1
      nhvy=nhvy+1
      if(nhvy.NE.1)then
      if(nhvy.LE.maxhvy)then
      
      do 5 jat=1,j2
      if(ATMCHG(jat).GT.heavy)then
      idx=idx+1
      x=A(jat,1)-x1
      y=A(jat,2)-y1
      theta=halfpi
      if(gabs(y).GT.Toler)theta=-gatan(x/y)
      angmat(idx)=theta
      endif
5     continue
      else
      write(Iout,99001)maxhvy
      goto 200
      endif
      endif
      endif
100   continue
      
      
200   if(nhvy.NE.1)then
      if(nhvy.GT.2)then
      
      i2=nhvy*(nhvy-1)/2
      nmax=0
      nsav=0
      do 220 i=1,i2
      curang=angmat(i)
      if(curang.NE.flag)then
      j1=i+1
      ncur=1
      do 205 j=j1,i2
      if(gabs(curang-angmat(j)).LE.Toler)then
      ncur=ncur+1
      angmat(j)=flag
      endif
205   continue
      if(nmax.LT.ncur)then
      nsav=1
      savang(1)=curang
      nmax=ncur
      elseif(nmax.EQ.ncur)then
      
      nsav=nsav+1
      if(nsav.LE.msav)savang(nsav)=curang
      endif
      endif
220   continue
      if(nmax.EQ.1)goto 300
      else
      savang(1)=angmat(1)
      nsav=1
      endif
      
      
      if(nsav.GT.1)then
      
      
      D(1,1)=A(key,1)
      D(1,2)=A(key,2)
      D(1,3)=A(key,3)
      call rotate(MAXAP3,D,B,1,t,3,savang(1))
      bestx=B(1,1)
      besty=B(1,2)
      ibest=1
      do 240 i=2,nsav
      call rotate(MAXAP3,D,B,1,t,3,savang(i))
      if(gabs(gabs(besty)-gabs(B(1,2))).GE.Toler)then
      if(gabs(besty).GE.Toler.OR.gabs(B(1,2)).GE.Toler)then
      if(besty.LE.B(1,2))then
      ibest=i
      besty=B(1,2)
      bestx=B(1,1)
      endif
      goto 240
      endif
      endif
      
      if(gabs(gabs(bestx)-gabs(B(1,1))).GE.Toler)then
      if(bestx.LE.B(1,1))then
      ibest=i
      bestx=B(1,1)
      besty=B(1,2)
      endif
      endif
240   continue
      
      call rotate(MAXAP3,A,B,numatm,t,3,savang(ibest))
      call move(MAXAP3,B,A,numatm)
      return
      else
      call rotate(MAXAP3,A,B,numatm,t,3,savang(1))
      call move(MAXAP3,B,A,numatm)
      call oraxis(MAXAP3,A,B,numatm,ATMCHG,2)
      return
      endif
      endif
      
      
300   theta=halfpi
      x=A(key,1)
      y=A(key,2)
      if(gabs(y).GT.Toler)theta=-gatan(x/y)
      call rotate(MAXAP3,A,B,numatm,t,3,theta)
      if(B(key,2).GT.zero)then
      
      call move(MAXAP3,B,A,numatm)
      else
      call rotate(MAXAP3,B,A,numatm,t,3,pi)
      endif
      
      
      call orptst(MAXAP3,A,NATOMS,ixyz)
      if(ixyz.EQ.3)call oraxis(MAXAP3,A,B,NATOMS,ATMCHG,1)
      return
      
      end
C* :1 * 
      
