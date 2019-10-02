
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fpmain"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fpmain.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 16 "fpmain.web"
      subroutine fpmain
      implicit none
      double precision Alpha,Convrg,D1var,D1vold,D2var,Delvar,F,F1,Fzero
     &,Gen,gsqrt,H,Phycon,Pool0,Pool1,sqrtd2,Tcurcy,Telcur,Teltot,Tlstcy
      double precision Tmax,Toang,Vname,Xi,Yold
      integer i,Idone,Iflinf,Ifpgen,Ihflag,In,Incldh,Index,Intent,Iofp,I
     &ogen,Iout,Ipunch,Iret,Irmsf,Isect,Istats,Ititle,Itype,Jump
      integer K,Lambda,Mode,Ncyc,Ncycls,Ncytot,Noinch,Noruns,Npar,Nstep,
     &Nvar
      common/j102/Jump,Iret
      common/io/In,Iout,Ipunch
      common/gen/Gen(47)
      common/irw/Iogen,Irmsf,Iofp,Ifpgen
      common/fpinfo/Pool0(30),Pool1(30),Delvar(30),Yold(30),D1var(30),D2
     &var(30),D1vold(30),Xi(30),Fzero,F1(4),F,Alpha,Convrg,Teltot,Telcur
     &,Tlstcy,Tmax,Tcurcy,Iflinf,K,Nvar,Npar,Itype,Ncycls,Isect,Ncyc,Nor
     &uns,Ncytot,Ititle(8),Mode,Nstep,Index,Lambda,Ihflag,Idone,H(30,30)
     &,Noinch,Incldh,Vname(30),Istats,Intent
      common/phycon/Toang,Phycon(29)
      
      
      
      
      call enter(Toang)
      
      if(Isect.EQ.2)then
      elseif(Isect.EQ.3)then
      goto 100
      elseif(Isect.EQ.4)then
      goto 200
      elseif(Isect.EQ.5)then
      goto 300
      elseif(Isect.EQ.6)then
      goto 400
      else
      
      Isect=2
      call value
      if(Iret.GT.0)goto 500
      endif
      Fzero=F
      call twrite(Ifpgen,Gen,47,1,47,1,0)
      K=0
      Intent=0
100   Isect=3
      
      call deriv(Toang)
      if(Iret.GT.0)goto 500
      
      Intent=0
200   Isect=4
      
      call search
      if(Iret.GT.0)goto 500
      
300   Fzero=F
      K=K+1
      call twrite(Ifpgen,Gen,47,1,47,1,0)
      Intent=0
400   Isect=6
      
      call getmat(Toang)
      if(Iret.LE.0)then
      
      Intent=0
      
      
      do 450 i=1,Nvar
      
      sqrtd2=gsqrt(D2var(i))
      
      Pool1(i)=Pool1(i)/sqrtd2
      D1var(i)=D1var(i)*sqrtd2
      
450   continue
      goto 200
      endif
      
500   return
      
      end
C* :1 * 
      
