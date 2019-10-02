
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 eqrt2d"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "eqrt2d.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "eqrt2d.web"
      subroutine eqrt2d(D,E,N,Z,IZ,IER)
      implicit none
      double precision b,c,D,E,eps,f,g,gabs,gsqrt,h,one,p,r,s,Z,zero
      integer i,IER,ii,ip1,IZ,j,k,l,m,mm1,mm1pl,N
      dimension D(*),E(*),Z(IZ,*)
      data zero,one/.0D0,1.D0/
      
      
      
      eps=2.**(-37)
      IER=0
      if(N.NE.1)then
      do 50 i=2,N
      E(i-1)=E(i)
50    continue
      E(N)=zero
      b=zero
      f=zero
      do 150 l=1,N
      j=0
      h=eps*(gabs(D(l))+gabs(E(l)))
      if(b.LT.h)b=h
      do 60 m=l,N
      k=m
      if(gabs(E(k)).LE.b)goto 80
      
60    continue
80    m=k
      if(m.NE.l)then
90    if(j.EQ.30)goto 300
      j=j+1
      p=(D(l+1)-D(l))/(2.*E(l))
      r=gsqrt(p*p+one)
      if(p.GE.zero)h=D(l)-E(l)/(p+r)
      if(p.LT.zero)h=D(l)-E(l)/(p-r)
      do 100 i=l,N
      D(i)=D(i)-h
100   continue
      f=f+h
      p=D(m)
      c=one
      s=zero
      mm1=m-1
      mm1pl=mm1+l
      if(l.LE.mm1)then
      do 105 ii=l,mm1
      i=mm1pl-ii
      g=c*E(i)
      h=c*p
      if(gabs(p).LT.gabs(E(i)))then
      
      c=p/E(i)
      r=gsqrt(c*c+one)
      E(i+1)=s*E(i)*r
      s=one/r
      c=c/r
      else
      c=E(i)/p
      r=gsqrt(c*c+one)
      E(i+1)=s*p*r
      s=c/r
      c=one/r
      endif
      p=c*D(i)-s*g
      D(i+1)=h+s*(c*g+s*D(i))
      do 102 k=1,N
      h=Z(k,i+1)
      Z(k,i+1)=s*Z(k,i)+c*h
      Z(k,i)=c*Z(k,i)-s*h
102   continue
105   continue
      endif
      E(l)=s*p
      D(l)=c*p
      if(gabs(E(l)).GT.b)goto 90
      endif
      D(l)=D(l)+f
150   continue
      do 200 i=1,N
      k=i
      p=D(i)
      ip1=i+1
      if(ip1.LE.N)then
      do 160 j=ip1,N
      if(D(j).LT.p)then
      k=j
      p=D(j)
      endif
160   continue
      endif
      if(k.NE.i)then
      D(k)=D(i)
      D(i)=p
      do 170 j=1,N
      p=Z(j,i)
      Z(j,i)=Z(j,k)
      Z(j,k)=p
170   continue
      endif
200   continue
      endif
      goto 400
      
300   IER=1
      call lnk1e
400   return
      
      end
C* :1 * 
      
