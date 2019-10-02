
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ryspol"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ryspol.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "ryspol.web"
      subroutine ryspol(N,X,T,W,IPRINT)
      implicit none
      double precision a,ap,c,c00,c01,c11,dr,extrap,f,f0,f1,f2,ff,ffp,fl
     &amb1,fx,gabs,gsqrt,one,ratio
      double precision root,sum,T,thr2,thr3,W,X,zero
      integer i,ier,In,Iout,ipm1,IPRINT,Ipunch,itmax,j,jsave,k,km1,Kop1,
     &Kop2,maxdim,N,ndeg,ndegp1,nfd,nfdp1
      integer np1
      real ra,rr,thr1,rtest,rzero
      dimension T(*),W(*)
      dimension f(10,10),c(10,10),a(20),ra(20),rr(2,18),dr(18),ap(20)
      common/io/In,Iout,Ipunch
      common/rys/Kop1,Kop2
      equivalence(c00,c(1,1)),(c01,c(1,2)),(c11,c(2,2))
      equivalence(f0,f(1,1)),(f1,f(2,1)),(f2,f(2,2))
      equivalence(dr(1),rr(1,1))
      data zero/0.0D0/,one/1.0D0/,thr3/1.0D-6/,thr2/1.0D-17/
      data thr1/1.0E-4/,rzero/0.0E0/
      data itmax/50/,maxdim/10/
      
      
      
      
      
      
99001 format(25H1FROM RYSPOL, ILLEGAL N: ,i6/)
99002 format(//15H0IN RYSPOL, N =,i2//5H0X = ,d32.25//15x,8HF-MATRIX/)
99003 format(//9H0FLAMB1 =,d32.25///15x,16HINITIAL C-MATRIX/)
99004 format(///15x,14HFINAL C-MATRIX/)
99005 format(///22H0MODIFIED COEFFICIENTS/(1x,i2,1x,d32.25))
99006 format(27H1COMPLEX ROOTS ENCOUNTERED./)
99007 format(///27H0RAW DOUBLE-PRECISION ROOTS/(1x,i2,2x,d32.25))
99008 format(///20H0DERIVATIVE ORDER = ,i2//13H0COEFFICIENTS/(1x,i2,2x,d
     &32.25))
99009 format(/24H0ITMAX EXCEEDED FOR I = ,i2/)
99010 format(/24H0ZERO DERIVATIVE AT I = ,i2,2x,d32.25/)
99011 format(///24H0FINAL ROOTS AND WEIGHTS/(1x,i2,2D32.25))
      
      
      
      ipm1=IPRINT-1
      np1=N+1
      
      if(N.LE.0)then
      write(Iout,99001)N
      return
      
      elseif(Kop2.NE.0)then
      
      
      
      
      call formfn(Kop1,maxdim,f,N,X)
      if(ipm1.GT.0)then
      write(Iout,99002)N,X
      call matout(f,maxdim,maxdim,np1,np1)
      endif
      
      do 50 i=1,np1
      do 20 j=1,np1
      c(i,j)=zero
20    continue
      c(i,i)=one
50    continue
      
      c00=one/gsqrt(f0)
      flamb1=one/(gsqrt(f0*(f0*f2-f1*f1)))
      c01=-f1*flamb1
      c11=f0*flamb1
      
      if(ipm1.GT.0)then
      write(Iout,99003)flamb1
      call matout(c,maxdim,maxdim,np1,np1)
      endif
      
      if(N.GT.1)call cmpltc(maxdim,np1,3,c,c,f)
      
      if(ipm1.GT.0)then
      write(Iout,99004)
      call matout(c,maxdim,maxdim,np1,np1)
      endif
      
      
      if(N.LE.1)then
      
      
      
      
      
      
      T(1)=gsqrt(-c01/c11)
      W(1)=f0
      else
      
      k=np1+N
      do 60 i=1,np1
      a(k)=c(i,np1)
      ra(k)=sngl(a(k))
      k=k-2
60    continue
      ndeg=N+N
      ndegp1=ndeg+1
      
      if(ipm1.GT.0)write(Iout,99005)(i,a(i),i=1,ndegp1)
      
      call zpolyr(ra,ndeg,rr,rr,ier)
      
      k=0
      do 80 i=1,ndeg
      if(gabs(rr(2,i)).GE.thr1)goto 180
      if(rr(1,i).GT.thr1)then
      k=k+1
      ra(k)=rr(1,i)
      endif
80    continue
      
      k=N
      do 100 i=1,N
      rtest=rzero
      do 90 j=1,N
      if(ra(j).GT.rtest)then
      jsave=j
      rtest=ra(j)
      endif
90    continue
      dr(k)=dble(ra(jsave))
      ra(jsave)=rzero
      k=k-1
100   continue
      
      if(ipm1.GT.0)write(Iout,99007)(i,dr(i),i=1,N)
      
      
      call fpx(ndeg,nfd,a,ap)
      nfdp1=nfd+1
      
      if(ipm1.GT.0)write(Iout,99008)nfd,(i,ap(i),i=1,nfdp1)
      
      do 140 i=1,N
      root=dr(i)
      do 110 j=1,itmax
      ff=fx(ndeg,root,a)
      ffp=fx(nfd,root,ap)
      if(gabs(ffp).LT.thr3)goto 120
      extrap=ff/ffp
      ratio=extrap/root
      if(gabs(ratio).LE.thr2)goto 130
      root=root-extrap
110   continue
      write(Iout,99009)i
      goto 130
      
120   write(Iout,99010)i,ffp
130   T(i)=root
140   continue
      
      do 160 i=1,N
      W(i)=zero
      root=T(i)
      do 150 j=1,N
      sum=zero
      do 145 k=1,j
      km1=k-1
      sum=sum+(c(k,j)*(root**(2*km1)))
145   continue
      W(i)=W(i)+sum*sum
150   continue
      W(i)=one/W(i)
160   continue
      goto 200
      
180   write(Iout,99006)rr(2,i)
      return
      endif
      
      
200   if(ipm1.GT.0)write(Iout,99011)(i,T(i),W(i),i=1,N)
      
      do 250 i=1,N
      T(i)=T(i)*T(i)
250   continue
      
      return
      endif
      
      
      call rpol1(N,X,T,W)
      return
      
      end
C* :1 * 
      
