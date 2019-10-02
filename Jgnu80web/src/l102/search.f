
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 search"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "search.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "search.web"
      subroutine search
      implicit none
      double precision Alpha,Convrg,D1var,D1vold,D2var,Delvar,F,F1,Fzero
     &,gfloat,gsqrt,H,Pool0,Pool1,rlmbda,sqrtd2,Tcurcy,Telcur,Teltot,Tls
     &tcy
      double precision Tmax,Vname,Xi,Yold,zero
      integer i,Idone,Iflinf,Ihflag,In,Incldh,Index,Intent,Iout,Ipunch,I
     &ret,Isect,Istats,Ititle,Itype,j,Jump,K,Lambda,Mode
      integer Ncyc,Ncycls,Ncytot,Noinch,Noruns,Npar,Nstep,Nvar
      common/j102/Jump,Iret
      common/io/In,Iout,Ipunch
      common/fpinfo/Pool0(30),Pool1(30),Delvar(30),Yold(30),D1var(30),D2
     &var(30),D1vold(30),Xi(30),Fzero,F1(4),F,Alpha,Convrg,Teltot,Telcur
     &,Tlstcy,Tmax,Tcurcy,Iflinf,K,Nvar,Npar,Itype,Ncycls,Isect,Ncyc,Nor
     &uns,Ncytot,Ititle(8),Mode,Nstep,Index,Lambda,Ihflag,Idone,H(30,30)
     &,Noinch,Incldh,Vname(30),Istats,Intent
      data zero/0.0D0/
      
      
      
99001 format('  AFTER EXTRAPOLATION, ALPHA = ',g20.10)
      
      
      if(Intent.NE.0)goto 600
      do 100 i=1,Nvar
      sqrtd2=gsqrt(D2var(i))
      D1var(i)=D1var(i)/sqrtd2
      Pool1(i)=Pool1(i)*sqrtd2
100   continue
      Lambda=1
      if(K.NE.0)then
      do 150 i=1,Nvar
      Xi(i)=zero
150   continue
      do 200 i=1,Nvar
      do 160 j=1,Nvar
      Xi(i)=Xi(i)-(H(i,j)*D1var(j))
160   continue
200   continue
      else
      do 250 i=1,Nvar
      Xi(i)=-D1var(i)
250   continue
      endif
300   rlmbda=gfloat(Lambda)
      do 400 i=1,Nvar
      Pool0(i)=Pool1(i)+rlmbda*Xi(i)
400   continue
      do 500 i=1,Nvar
      Pool0(i)=Pool0(i)/(gsqrt(D2var(i)))
500   continue
      
      call value
      if(Iret.GT.0)goto 900
      
600   F1(Lambda)=F
      Lambda=Lambda+1
      if(Lambda.LE.2)goto 300
      Alpha=1.0D0-(F1(2)-Fzero)/(2.0D0*(F1(2)+Fzero-2.0D0*F1(1)))
      write(Iout,99001)Alpha
      do 700 i=1,Nvar
      Pool0(i)=Pool1(i)+Alpha*Xi(i)
700   continue
      do 800 i=1,Nvar
      sqrtd2=gsqrt(D2var(i))
      Pool0(i)=Pool0(i)/sqrtd2
      Pool1(i)=Pool1(i)/sqrtd2
800   continue
      Isect=5
      
      call value
      
900   return
      
      end
C* :1 * 
      
