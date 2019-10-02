
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ehousd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ehousd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "ehousd.web"
      subroutine ehousd(A,N,D,E,E2)
      implicit none
      double precision A,D,E,E2,eps,eps1,eta,f,g,gsqrt,h,hh,tol,zero
      integer i,ii,In,Iout,Ipunch,iz,j,jk,k,l,N,np1
      dimension A(*),D(*),E(*),E2(*)
      common/io/In,Iout,Ipunch
      data zero/.0D0/
      
      
      
      eps=2.**(-37)
      eta=2.0D00**(-126)
      tol=eta/eps
      eps1=eps+1
      if(eps1.EQ.eps)write(Iout,99001)eps,eps1
      
99001 format(' EHOUSD-- EPS IS TO SMALL: EPS,EPS+1= ',2G15.5)
      np1=N+1
      do 100 ii=1,N
      i=np1-ii
      l=i-1
      iz=i*l/2
      h=zero
      if(l.GT.0)then
      do 20 k=1,l
      f=A(iz+k)
      D(k)=f
      h=h+f*f
20    continue
      endif
      if(h.GT.tol)then
      
      E2(i)=h
      E(i)=gsqrt(h)
      if(f.GE.0.)E(i)=-E(i)
      g=E(i)
      h=h-f*g
      D(l)=f-g
      A(iz+l)=D(l)
      f=zero
      if(l.GT.0)then
      do 30 j=1,l
      g=zero
      jk=j*(j-1)/2+1
      do 25 k=1,l
      g=g+A(jk)*D(k)
      jk=jk+1
      if(k.GE.j)jk=jk+k-1
25    continue
      g=g/h
      E(j)=g
      f=f+g*D(j)
30    continue
      endif
      hh=f/(h+h)
      jk=0
      if(l.GT.0)then
      do 40 j=1,l
      f=D(j)
      E(j)=E(j)-hh*f
      g=E(j)
      do 35 k=1,j
      jk=jk+1
      A(jk)=A(jk)-f*E(k)-g*D(k)
35    continue
40    continue
      endif
      else
      E(i)=zero
      E2(i)=zero
      h=zero
      endif
      D(i)=A(iz+i)
      A(iz+i)=h
100   continue
      return
      
      end
C* :1 * 
      
