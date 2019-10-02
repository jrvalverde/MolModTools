
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rootn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rootn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "rootn.web"
      subroutine rootn(IERR,ROOT,X0,A,N)
      implicit none
      double precision A,a0,a01,a02,a2,a23,a3,Big,conv,disc,dx,f0,f1,Fou
     &r,gabs,gfloat,gsqrt,One,Onept5,Pt5
      double precision ROOT,Small,Three,Two,X0,x00,Zero
      integer i,IERR,jcyc,maxcyc,N,n1
      dimension A(3)
      common/const/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      data maxcyc,conv/50,1.D-10/
      
      
      
      if(N.NE.0)then
      IERR=0
      if(N.GT.2)then
      n1=N+1
      jcyc=0
      x00=X0
      
20    jcyc=jcyc+1
      if(jcyc.LE.maxcyc)then
      
      f0=Zero
      a0=One
      do 30 i=1,n1
      f0=f0+A(i)*a0
      a0=a0*x00
30    continue
      
      f1=Zero
      a0=One
      do 40 i=1,N
      f1=f1+gfloat(i)*A(i+1)*a0
      a0=a0*x00
40    continue
      
      dx=f0/f1
      if(gabs(dx).LE.conv)then
      
      ROOT=x00-dx
      return
      else
      x00=x00-dx
      goto 20
      endif
      endif
      
      elseif(N.EQ.2)then
      
      a2=A(2)
      a3=A(3)
      disc=a2*a2-Four*A(1)*a3
      if(disc.GE.Zero)then
      disc=gsqrt(disc)
      a23=a3+a3
      a01=(disc-a2)/a23
      a02=-(a2+disc)/a23
      if(gabs(X0-a01).GT.gabs(X0-a02))then
      
      ROOT=a02
      return
      else
      ROOT=a01
      return
      endif
      endif
      else
      
      ROOT=-A(1)/A(2)
      return
      endif
      endif
      
      IERR=1
      return
      
      end
C* :1 * 
      
