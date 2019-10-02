
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ehoudh"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ehoudh.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 59 "ehoudh.web"
      subroutine ehoudh(AR,AI,N,D,E,TAU)
      implicit none
      double precision AI,AR,bb,D,delta,E,eps,eta,gabs,gsqrt,one,q1,q2,r
     &atio,rdelp,rho,root,sqrtvr,t1,t2
      double precision TAU,testbb,toler,tt1,tt2,vr,x1,x2,zero
      integer i,im1,indx,inx1,inx2,ix,j,jj,jp1,kk,l,N,nm1,nn,nr,nrm1
      dimension AR(*),AI(*),D(*),E(*),TAU(2,*)
      data zero,one/0.D0,1.D0/
      
      eps=2.**(-37)
      eta=2.0D00**(-126)
      rdelp=eta/eps
      nm1=N-1
      toler=zero
      nn=N*(N+1)/2
      do 100 i=1,nn
      t1=gabs(AR(i))
      t2=gabs(AI(i))
      if(t2.GT.t1)t1=t2
      if(t1.GT.toler)toler=t1
100   continue
      testbb=rdelp*toler
      if(N.GT.2)then
      do 200 nr=2,nm1
      nrm1=nr-1
      vr=zero
      TAU(1,nr)=zero
      TAU(2,nr)=zero
      TAU(2,1)=zero
      do 120 l=nr,N
      indx=l*(l-1)/2+nrm1
      vr=AR(indx)**2+AI(indx)**2+vr
120   continue
      indx=nr*nrm1/2+nrm1
      sqrtvr=gsqrt(vr)
      if(testbb.LT.sqrtvr)then
      root=gsqrt(AR(indx)**2+AI(indx)**2)*sqrtvr
      if(root.NE.zero)then
      
      delta=vr+root
      ratio=vr/root
      TAU(1,1)=-ratio*AR(indx)
      TAU(2,1)=ratio*AI(indx)
      AR(indx)=(ratio+one)*AR(indx)
      AI(indx)=(ratio+one)*AI(indx)
      else
      AR(indx)=sqrtvr
      delta=vr
      TAU(1,1)=-AR(indx)
      endif
      do 140 j=nr,N
      jj=j*(j-1)/2
      indx=jj+nrm1
      TAU(1,j)=AR(indx)/delta
      TAU(2,j)=AI(indx)/delta
      D(j)=zero
      E(j)=zero
      do 125 l=nr,j
      inx1=l*(l-1)/2+nrm1
      inx2=jj+l
      D(j)=D(j)+AR(inx2)*AR(inx1)-AI(inx2)*AI(inx1)
      E(j)=E(j)+AR(inx2)*AI(inx1)+AI(inx2)*AR(inx1)
125   continue
      jp1=j+1
      if(jp1.GT.N)goto 150
      do 130 l=jp1,N
      kk=l*(l-1)/2
      inx1=kk+nrm1
      inx2=kk+j
      D(j)=D(j)+AR(inx2)*AR(inx1)+AI(inx2)*AI(inx1)
      E(j)=E(j)+AR(inx2)*AI(inx1)-AI(inx2)*AR(inx1)
130   continue
140   continue
150   rho=zero
      do 160 l=nr,N
      rho=rho+D(l)*TAU(1,l)+E(l)*TAU(2,l)
160   continue
      ix=nrm1*(nr-2)/2
      do 170 i=nr,N
      ix=ix+i-1
      inx2=ix+nrm1
      do 165 j=nr,i
      inx1=ix+j
      x1=TAU(1,i)*D(j)+TAU(2,i)*E(j)
      x2=TAU(2,i)*D(j)-TAU(1,i)*E(j)
      q1=D(i)-rho*AR(inx2)
      q2=E(i)-rho*AI(inx2)
      t1=q1*TAU(1,j)+q2*TAU(2,j)
      t2=q2*TAU(1,j)-q1*TAU(2,j)
      AR(inx1)=AR(inx1)-x1-t1
      AI(inx1)=AI(inx1)-x2-t2
165   continue
170   continue
      TAU(1,nr)=TAU(1,1)
      TAU(2,nr)=TAU(2,1)
      endif
200   continue
      endif
      indx=0
      do 300 i=1,N
      indx=indx+i
      D(i)=AR(indx)
300   continue
      TAU(1,1)=one
      TAU(2,1)=zero
      E(1)=zero
      if(N.NE.1)then
      indx=(N*nm1)/2+nm1
      TAU(1,N)=AR(indx)
      TAU(2,N)=-AI(indx)
      indx=1
      do 350 i=2,N
      indx=indx+i
      im1=i-1
      bb=gsqrt(TAU(1,i)**2+TAU(2,i)**2)
      E(i)=bb
      AI(indx)=bb
      if(testbb.GE.bb)then
      TAU(1,i)=one
      TAU(2,i)=zero
      bb=one
      endif
      tt1=TAU(1,i)*TAU(1,im1)-TAU(2,i)*TAU(2,im1)
      tt2=TAU(1,i)*TAU(2,im1)+TAU(2,i)*TAU(1,im1)
      TAU(1,i)=tt1/bb
      TAU(2,i)=tt2/bb
350   continue
      endif
      return
      
      end
C* :1 * 
      
