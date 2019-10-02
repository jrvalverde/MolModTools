
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ornax"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ornax.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "ornax.web"
      integer function ornax(MAXAP3,A,NATOMS,IAN)
      implicit none
      double precision A,cutoff,dis,dx,dy,dz,gabs,gsqrt,one,qa2,qa3,qb2,
     &qb3,Tol2,Toler,tst1,tst2,x1,xx,y1
      double precision yy,z1,zero,zz
      integer i1,i2,i3,IAN,iat,iclass,idx,ix,jan,jat,jdx,jjdx,kan,kat,kd
     &x,kkdx,MAXAP3,NATOMS,nx,ny
      integer nz
      dimension A(MAXAP3,3),IAN(*)
      dimension cutoff(10),iclass(18),ix(4)
      dimension dis(5050)
      common/tol/Toler,Tol2
      data cutoff/0.89,1.73,2.91,1.25,2.53,1.70,1.66,2.4,2.04,2.53/
      data iclass/1,1,2,2,3,3,3,3,3,3,4,4,4,4,4,4,4,4/
      data ix/0,1,3,6/
      data zero,one/0.0D0,1.0D0/
      
      
      
      
      
      
      
      nx=0
      ny=0
      nz=0
      
      
      do 100 iat=1,NATOMS
      xx=A(iat,1)*A(iat,1)
      yy=A(iat,2)*A(iat,2)
      zz=A(iat,3)*A(iat,3)
      dx=gsqrt(yy+zz)
      dy=gsqrt(xx+zz)
      dz=gsqrt(xx+yy)
      if(gabs(dx).LT.Toler)nx=nx+1
      if(gabs(dy).LT.Toler)ny=ny+1
      if(gabs(dz).LT.Toler)nz=nz+1
100   continue
      
      
      if(nz.LT.ny)then
      if(ny.LT.nx)then
      elseif(ny.EQ.nx)then
      goto 200
      else
      
      ornax=2
      return
      endif
      elseif(nz.EQ.ny)then
      if(nx.LE.nz)goto 200
      elseif(nz.LT.nx)then
      elseif(nz.EQ.nx)then
      goto 200
      else
      
      ornax=3
      return
      endif
      ornax=1
      return
      
      
      
200   idx=0
      nx=0
      ny=0
      nz=0
      do 300 iat=1,NATOMS
      x1=A(iat,1)
      y1=A(iat,2)
      z1=A(iat,3)
      do 250 jat=1,iat
      idx=idx+1
      dis(idx)=gsqrt((x1-A(jat,1))**2+(y1-A(jat,2))**2+(z1-A(jat,3))**2)
250   continue
300   continue
      
      
      idx=0
      do 400 jat=1,NATOMS
      jan=IAN(jat)
      jjdx=iclass(jan)
      jdx=ix(jjdx)
      do 350 kat=1,jat
      kan=IAN(kat)
      kkdx=iclass(kan)
      kdx=jdx+kkdx
      if(kkdx.GT.jjdx)kdx=ix(kkdx)+jjdx
      idx=idx+1
      if(dis(idx).GT.cutoff(kdx))dis(idx)=zero
350   continue
400   continue
      
      
      idx=0
      do 500 iat=1,NATOMS
      do 450 jat=1,iat
      idx=idx+1
      if(gabs(dis(idx)).GE.Tol2)then
      do 410 i1=1,3
      i2=1+mod(i1,3)
      i3=1+mod(i2,3)
      qa2=A(iat,i2)
      qa3=A(iat,i3)
      qb2=A(jat,i2)
      qb3=A(jat,i3)
      
      
      tst1=gsqrt(qa2*qa2+qa3*qa3)
      if(tst1.GE.Toler)then
      tst2=gsqrt(qb2*qb2+qb3*qb3)
      if(tst2.GE.Toler)then
      
      
      tst1=qa3*(qa2-qb2)
      tst2=qa2*(qa3-qb3)
      if(gabs(tst1-tst2).LE.Tol2)then
      
      
      if(gabs(qa2).LT.Toler.AND.gabs(qb2).LT.Toler)then
      
      tst2=dsign(one,qa3)+dsign(one,qb3)
      if(tst2.NE.zero)goto 410
      else
      tst1=dsign(one,qa2)+dsign(one,qb2)
      if(tst1.NE.zero)goto 410
      endif
      
      
      if(i1.EQ.2)then
      
      ny=ny+1
      elseif(i1.EQ.3)then
      
      nz=nz+1
      else
      
      nx=nx+1
      endif
      endif
      endif
      endif
      
410   continue
      endif
450   continue
500   continue
      
      
      if(nz.LT.ny)then
      if(ny.LT.nx)then
      elseif(ny.EQ.nx)then
      goto 600
      else
      
      ornax=2
      return
      endif
      elseif(nz.EQ.ny)then
      if(nx.LE.nz)goto 600
      elseif(nz.LT.nx)then
      elseif(nz.EQ.nx)then
      goto 600
      else
      
      ornax=3
      return
      endif
      ornax=1
      return
      
600   ornax=0
      return
      
      end
C* :1 * 
      
