
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 redop"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "redop.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "redop.web"
      subroutine redop(NBASIS,NBAS6D,NAO,D,IPRINT)
      implicit none
      double precision C1,C2,C3,D,d0,d1m,d1p,d2m,d2p,Exx,gsqrt,h,r,three
     &,tq,X,Y,Z,zero
      integer i,ii,iin,ij,ijn,In,Iout,IPRINT,Ipunch,ishell,ist,itmp,j,Ja
     &n,jbasis,jn,jshell,jst,jtmp,lamax
      integer lbmax,Lbound,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,mu,munu,N1
     &0ord,N5ord,N6ord,N7ord,NAO,NBAS6D,NBASIS,Nordr,Nshell,ntt,ntt6d
      integer nu
      integer scona,sconb,Ubound,Ulpure
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon
      dimension D(*),NAO(*),tq(6,6)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/io/In,Iout,Ipunch
      data zero/0.D0/,h/0.5D0/,three/3.0D0/
      
99001 format(' FROM REDOP, EXPANDED MATRIX:')
99002 format('             TRANSFORMED MATRIX:')
      
      
      r=h*gsqrt(three)
      ntt6d=(NBAS6D*(NBAS6D+1))/2
      
      do 100 itmp=1,NBASIS
      i=NBASIS+1-itmp
      ii=i*(i-1)/2
      In=NAO(i)
      iin=In*(In-1)/2
      do 50 jtmp=1,i
      j=i+1-jtmp
      ij=ii+j
      jn=NAO(j)
      ijn=iin+jn
      D(ijn)=D(ij)
      if(ij.LT.ijn)D(ij)=zero
50    continue
100   continue
      
      if(IPRINT.NE.0)then
      write(Iout,99001)
      call ltoutd(NBAS6D,D,1)
      endif
      
      jbasis=Aos(Nshell+1)-1
      ntt=jbasis*(jbasis+1)/2
      do 200 ishell=1,Nshell
      do 150 jshell=1,ishell
      if((Shellt(ishell).GE.2).OR.(Shellt(jshell).GE.2))then
      
      lamax=Shellt(ishell)+1
      lbmax=Shellt(jshell)+1
      scona=Shellc(ishell)+1
      sconb=Shellc(jshell)+1
      ist=Aos(ishell)+Lbound(lamax,scona)-2
      jst=Aos(jshell)+Lbound(lbmax,sconb)-2
      In=Ubound(lamax)-Lbound(lamax,scona)+1
      jn=Ubound(lbmax)-Lbound(lbmax,sconb)+1
      
      
      do 110 i=1,In
      do 105 j=1,jn
      mu=ist+i
      nu=jst+j
      if(mu.GE.nu)munu=mu*(mu-1)/2+nu
      if(nu.GT.mu)munu=nu*(nu-1)/2+mu
      tq(i,j)=D(munu)
105   continue
110   continue
      
      
      if(Shellt(ishell).GE.2)then
      do 115 j=1,In
      d0=tq(1,j)
      d1p=tq(2,j)
      d1m=tq(3,j)
      d2p=tq(4,j)
      d2m=tq(5,j)
      tq(1,j)=-h*d0+r*d2p
      tq(2,j)=-h*d0-r*d2p
      tq(3,j)=d0
      tq(4,j)=d2m
      tq(5,j)=d1p
      tq(6,j)=d1m
115   continue
      endif
      
      
      if(Shellt(jshell).GE.2)then
      do 120 i=1,In
      d0=tq(i,1)
      d1p=tq(i,2)
      d1m=tq(i,3)
      d2p=tq(i,4)
      d2m=tq(i,5)
      tq(i,1)=-h*d0+r*d2p
      tq(i,2)=-h*d0-r*d2p
      tq(i,3)=d0
      tq(i,4)=d2m
      tq(i,5)=d1p
      tq(i,6)=d1m
120   continue
      endif
      
      
      do 130 i=1,In
      do 125 j=1,jn
      mu=ist+i
      nu=jst+j
      if(mu.GE.nu)munu=mu*(mu-1)/2+nu
      if(nu.GT.mu)munu=nu*(nu-1)/2+mu
      D(munu)=tq(i,j)
125   continue
130   continue
      endif
      
150   continue
200   continue
      
      
      if(IPRINT.NE.0)then
      write(Iout,99002)
      call ltoutd(NBAS6D,D,1)
      endif
      return
      
      end
C* :1 * 
      
