
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rpol1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rpol1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "rpol1.web"
      subroutine rpol1(N,X,TP2,WP2)
      implicit none
      double precision ap2,Aphi,at2,Atheta,bp2,bp3,bp4,Bphi,bt2,bt3,bt4,
     &Btheta,C0,C1,C2,Cm1,dintrp,F100,F20i,F6i
      double precision Four,Hroot2,Hweigh,One,ph2,phi,S,sx,T2,th2,theta,
     &TP2,W2,WP2,X,xi,Xint,xrooti,y,y100
      double precision Ycut,Zero
      integer i,iad,Iadr,In,Iout,Ipunch,Lent,Lind,M,N
      dimension TP2(*),WP2(*)
      common/t2w21e/Lent(4),Lind(4),Iadr(4),S(4),Ycut(4),T2(1030),W2(103
     &0)
      common/herm1e/Hroot2(10),Hweigh(10)
      common/intcon/F6i,F20i,F100
      common/io/In,Iout,Ipunch
      common/int/Zero,Xint(12)
      common/mtpc/Atheta,Btheta,Aphi,Bphi,Cm1,C0,C1,C2,M
      equivalence(One,Xint(1)),(Four,Xint(4))
      equivalence(th2,ph2,bt2,y100)
      
      
      F100=100.0D0
      
      sx=S(N)*X
      y=sx/(One+sx)
      
      if(y.LT.Ycut(N))then
      
      y100=F100*y
      M=idint(y100)
      theta=y100-dfloat(M)
      phi=One-theta
      
      th2=theta*theta
      Atheta=theta*(th2-One)*F6i
      Btheta=Atheta*(th2-Four)*F20i
      ph2=phi*phi
      Aphi=phi*(ph2-One)*F6i
      Bphi=Aphi*(ph2-Four)*F20i
      
      at2=Atheta+Atheta
      bt2=Btheta+Btheta
      bt3=bt2+Btheta
      bt4=bt3+Btheta
      ap2=Aphi+Aphi
      bp2=Bphi+Bphi
      bp3=bp2+Bphi
      bp4=bp3+Bphi
      Cm1=Btheta-bp4+Aphi
      C0=phi-bt4+(bp3+bp3)+Atheta-ap2
      C1=theta+(bt3+bt3)-bp4-at2+Aphi
      C2=Bphi+Atheta-bt4
      
      iad=Iadr(N)
      do 50 i=1,N
      TP2(i)=dintrp(T2(iad))
      WP2(i)=dintrp(W2(iad))
      WP2(i)=dsqrt(WP2(i))
      iad=iad+Lent(N)
50    continue
      return
      endif
      
      
      xi=One/X
      xrooti=dsqrt(xi)
      do 100 i=1,N
      iad=Lind(N)+i
      TP2(i)=Hroot2(iad)*xi
      WP2(i)=Hweigh(iad)*xrooti
100   continue
      return
      
      end
C* :1 * 
      
