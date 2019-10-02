
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nextx"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nextx.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 31 "nextx.web"
      subroutine nextx(X,XLAST,G,GLAST,S,P,Q,Z,F,FLAST,N,NMAX)
      implicit none
      double precision Alpha,c,Delta1,Delta2,diff,Epsiln,F,Fconv,FLAST,F
     &lowb,G,gabs,Gkm1p,gkp,GLAST,gmin1,Gnorm,gsqrt,half,one
      double precision P,Pnorm,Q,S,Stpmin,test,three,two,w,X,XLAST,Z,zee
     &,zero,ztg,ztz
      integer i,Idnts,Ierr,Igo,Iguess,In,Iout,Ipunch,irset,Istype,j,N,Nd
     &um,NMAX
      double precision norm1
      dimension X(N),XLAST(N),G(N),GLAST(N),Q(N),Z(N),P(N)
      dimension S(NMAX,NMAX)
      common/ctests/Alpha,Delta1,Delta2,Epsiln,Stpmin,Fconv,Flowb,Gkm1p,
     &Pnorm,Gnorm,Idnts,Iguess,Ierr,Igo,Istype,Ndum
      common/io/In,Iout,Ipunch
      data zero/0.0D0/,half/0.5D0/,one/1.0D0/
      data two/2.0D0/,three/3.0D0/
      
      
      
      
      
      
      
      
99001 format(1x,'PREVIOUS STEP FAILED, RE-DO WITH ALPHA = ',f9.6)
99002 format(/1x,'C IS TOO SMALL'/1x,'     -- RESETTING SECOND DERIVATIV
     &E MATRIX'/1x,'     -- RESET ',i1)
99003 format(/1x,'SECOND DERIVATIVE MATRIX INDICATES A LOCAL MAXIMUM'/1x
     &,'     -- RESETTING SECOND DERIVATIVE MATRIX'/1x,'     -- RESET ',
     &i1)
99004 format(/1x,'ALPHA = ',f9.6,' IS TOO SMALL.'/1x,'TRY A DIFFERENT ST
     &ARTING POINT')
      
      
      if(Igo.EQ.2)then
      
      
      Gnorm=norm1(G,N)
      
      
      diff=FLAST-F
      test=-(Epsiln*Alpha*Gkm1p)
      if(test.GT.zero.AND.diff+Fconv.GT.test)then
      do 20 i=1,N
      Q(i)=G(i)-GLAST(i)
20    continue
      call axmxv(Z,one,S,Q,NMAX,N)
      do 40 i=1,N
      Z(i)=Alpha*P(i)-Z(i)
40    continue
      call vtxv(Q,Z,c,NMAX,N)
      call vtxv(Z,Z,ztz,NMAX,N)
      call vtxv(Z,GLAST,ztg,NMAX,N)
      irset=Istype+1
      if(gabs(c).GE.Delta2*ztz)then
      
      if(ztg/c.LE.-Delta1)goto 500
      write(Iout,99003)irset
      if(Istype.NE.0)then
      c=ztz
      goto 500
      endif
      else
      write(Iout,99002)irset
      if(Istype.NE.0)then
      c=ztz
      goto 500
      endif
      endif
      else
      call vtxv(G,P,gkp,NMAX,N)
      if(gabs(gkp).LT.Delta2.OR.Alpha*Pnorm.LT.Delta2)then
      
      Alpha=Alpha*half
      if(Alpha.GT.Stpmin)goto 300
      write(Iout,99004)Alpha
      Ierr=1
      return
      else
      zee=three*diff/Alpha+Gkm1p+gkp
      w=gsqrt(zee**2-Gkm1p*gkp)
      Alpha=Alpha*(one-(gkp+w-zee)/(gkp-Gkm1p+two*w))
      goto 300
      endif
      endif
      endif
      if(Idnts.NE.1)call ident(S,NMAX,N)
      Idnts=0
      Igo=2
100   call axmxv(P,-one,S,G,NMAX,N)
      call vtxv(G,P,Gkm1p,NMAX,N)
      Pnorm=norm1(P,N)
      Alpha=one
      if(Iguess.EQ.1)Alpha=gmin1(one,two*gabs((Flowb-F)/Gkm1p))
      do 200 i=1,N
      GLAST(i)=G(i)
      XLAST(i)=X(i)
      X(i)=X(i)+Alpha*P(i)
200   continue
      FLAST=F
      Ierr=0
      return
      
300   do 400 i=1,N
      X(i)=XLAST(i)+Alpha*P(i)
400   continue
      Ierr=0
      write(Iout,99001)Alpha
      return
      
500   do 600 i=1,N
      do 550 j=i,N
      S(i,j)=S(i,j)+Z(i)*Z(j)/c
      S(j,i)=S(i,j)
550   continue
600   continue
      goto 100
      
      end
C* :1 * 
      
