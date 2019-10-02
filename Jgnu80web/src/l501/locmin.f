
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 locmin"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "locmin.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "locmin.web"
      subroutine locmin(LM,LN,X,Y,XL,NOMIN,XLAM,YMIN,IPRINT,MDM)
      implicit none
      double precision A,a0,a1,a2,a3,an,B,Big,di,f1,f2,Fillab,Four,gabs,
     &gfloat,One,Onept5,Pt5,rootac,Small
      double precision tau,Three,Two,X,x0,xi,XL,XLAM,xlimit,xst,Y,YMIN,Z
     &ero
      integer i,i1,i2,i3,ierr,In,Iout,IPRINT,Ipunch,j,k,LM,LN,m2,m21,m22
     &,m3,m6,MDM,mdm1
      integer mpts
      logical NOMIN
      real Bs(1)
      dimension X(MDM,MDM),Y(MDM),XL(MDM)
      dimension f1(10),f2(10),an(10)
      common/const/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/memry/A(4970),B(4970),Fillab(40060)
      common/io/In,Iout,Ipunch
      equivalence(Bs(1),B(1))
      data rootac/1.D-9/
      data mpts/201/
      
      
      
      
      
      
99001 format('1VALUES FOR THE POLYNOMIAL'/1x,25(1H*)//7x,'X',26x,4HF(X),
     &20x,5HF'(X),18x,6HF''(X)/)
99002 format(f10.4,10x,3D25.12)
99003 format(/)
99004 format(' NO MINIMUM FOUND')
      mdm1=MDM+1
      m2=LM+1
      m3=3*mpts
      m6=m3+1
      
      call inv(X,m2,f1,f2,an,B,MDM)
      if(m2.NE.0)then
      
      do 50 i=1,m2
      a0=Zero
      do 20 j=1,m2
      a0=a0+X(i,j)*Y(j)
20    continue
      an(i)=a0
50    continue
      
      do 100 i=2,m2
      i1=i-1
      f1(i1)=an(i)*gfloat(i1)
100   continue
      
      do 150 i=3,m2
      i1=i-1
      i2=i-2
      f2(i2)=f1(i1)*gfloat(i2)
150   continue
      
      xlimit=XL(LN)
      xst=XL(1)
      tau=XL(2)-XL(1)
      i3=0
      NOMIN=.TRUE.
      do 200 i=1,LN
      x0=XL(i)
      call rootn(ierr,a1,x0,f1,i2)
      if(ierr.EQ.0)then
      if(.NOT.(tau.GT.Zero.AND.(a1.GT.xlimit.OR.a1.LT.xst)))then
      if(.NOT.(tau.LT.Zero.AND.(a1.LT.xlimit.OR.a1.GT.xst)))then
      a0=One
      a2=Zero
      do 152 j=1,i2
      a2=a2+f2(j)*a0
      a0=a0*a1
152   continue
      if(a2.GE.Zero)then
      if(i3.NE.0)then
      do 154 k=1,i3
      if(gabs(a1-Y(k)).LE.rootac)goto 200
154   continue
      endif
      i3=i3+1
      Y(i3)=a1
      NOMIN=.FALSE.
      endif
      endif
      endif
      endif
200   continue
      
      if(.NOT.(IPRINT.LE.0.OR.(IPRINT.LE.2.AND.i3.EQ.1)))then
      write(Iout,99001)
      m21=m2-1
      m22=m2-2
      xi=xst
      di=(xlimit-xst)/gfloat(mpts-1)
      do 240 i=1,mpts
      a0=One
      a1=Zero
      do 210 j=1,m2
      a1=a1+an(j)*a0
      a0=a0*xi
210   continue
      a0=One
      a2=Zero
      do 220 j=1,m21
      a2=a2+f1(j)*a0
      a0=a0*xi
220   continue
      a0=One
      a3=Zero
      do 230 j=1,m22
      a3=a3+f2(j)*a0
      a0=a0*xi
230   continue
      if(mod(i-1,10).EQ.0)write(Iout,99002)xi,a1,a2,a3
      Bs(i)=sngl(xi)
      Bs(i+mpts)=sngl(xi)
      Bs(i+2*mpts)=sngl(xi)
      Bs(i+3*mpts)=sngl(a1)
      Bs(i+4*mpts)=sngl(a2)
      Bs(i+5*mpts)=sngl(a3)
      xi=xi+di
240   continue
      write(Iout,99003)
      endif
      if(.NOT.NOMIN)goto 300
      endif
      XLAM=XL(LN)
      YMIN=Y(LN)
      write(Iout,99004)
      NOMIN=.TRUE.
      return
      
300   YMIN=Zero
      do 400 i=1,i3
      a0=Zero
      a1=One
      xi=Y(i)
      do 350 j=1,m2
      a0=a0+a1*an(j)
      a1=a1*xi
350   continue
      if(a0.LT.YMIN)then
      YMIN=a0
      XLAM=Y(i)
      endif
400   continue
      return
      
      end
C* :1 * 
      
