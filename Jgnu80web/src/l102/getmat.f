
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getmat"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getmat.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "getmat.web"
      subroutine getmat(TOANG)
      implicit none
      double precision Alpha,Convrg,d1doth,D1var,D1vold,D2var,Deltab,Del
     &taz,Delvar,F,F1,Fzero,gsqrt,H,hbar,hdotd1,one,Pool0,Pool1,rmsd
      double precision rmsfp,rmshi,rmslo,sqrtd2,Tcurcy,Telcur,Teltot,Tls
     &tcy,Tmax,TOANG,Vname,Xi,Yold,zdotd1,zero
      integer i,Idone,Iflinf,Ihflag,In,Incldh,Index,Intent,Iop,Iout,Ipun
     &ch,Iret,Isect,Istats,Ititle,Itype,j,Jump,K,km1
      integer Lambda,Mode,Ncyc,Ncycls,Ncytot,Noinch,Noruns,Npar,Nstep,Nv
     &ar
      double precision mone,mtwo
      dimension d1doth(30),hdotd1(30)
      dimension mone(30,30),mtwo(30,30)
      dimension Deltaz(30),Deltab(30)
      common/j102/Jump,Iret
      common/iop/Iop(50)
      common/io/In,Iout,Ipunch
      common/fpinfo/Pool0(30),Pool1(30),Delvar(30),Yold(30),D1var(30),D2
     &var(30),D1vold(30),Xi(30),Fzero,F1(4),F,Alpha,Convrg,Teltot,Telcur
     &,Tlstcy,Tmax,Tcurcy,Iflinf,K,Nvar,Npar,Itype,Ncycls,Isect,Ncyc,Nor
     &uns,Ncytot,Ititle(8),Mode,Nstep,Index,Lambda,Ihflag,Idone,H(30,30)
     &,Noinch,Incldh,Vname(30),Istats,Intent
      equivalence(Deltab(1),D1vold(1))
      equivalence(Deltaz(1),Yold(1))
      data zero/0.0D0/,one/1.0D0/
      data rmshi/0.16D0/,rmslo/0.0010D0/
      
      
      
      
      
      
99001 format(1x,'AT CYCLE ',i6,' ZDOTD1 = ',g20.10,' AND HBAR = ',g20.10
     &/1x,16x,'DELTAB',20x,'DELTAZ')
99002 format(2(10x,g20.10))
99003 format('       H-MATRIX:')
99004 format(1x,'THE RMS DISPLACEMENT FROM CYCLE',i3,' TO',i3,' IS ',f8.
     &5)
99005 format(1x,'COMPUTE FIRST AND SECOND DERIVATIVES:')
99006 format(1x,'COMPUTE FIRST DERIVATIVES:')
      
      if(Intent.EQ.0)then
      do 50 i=1,Nvar
      Yold(i)=Pool1(i)
      Pool1(i)=Pool0(i)
50    continue
      Mode=1
      km1=K-1
      rmsd=rmsfp(Nvar,2)
      write(Iout,99004)km1,K,rmsd
      if(rmsd.LT.rmslo.OR.rmsd.GT.rmshi)Mode=0
      if(Mode.EQ.0)write(Iout,99005)
      if(Mode.NE.0)write(Iout,99006)
      endif
      call deriv(TOANG)
      if(Iret.LE.0)then
      do 100 i=1,Nvar
      
      sqrtd2=gsqrt(D2var(i))
      Pool1(i)=Pool1(i)*sqrtd2
      Yold(i)=Yold(i)*sqrtd2
      D1var(i)=D1var(i)/sqrtd2
      
      Deltaz(i)=Pool1(i)-Yold(i)
      Deltab(i)=D1var(i)-D1vold(i)
100   continue
      if(K.EQ.1)then
      do 120 i=1,Nvar
      do 110 j=1,Nvar
      H(i,j)=zero
110   continue
      H(i,i)=one
120   continue
      Ihflag=1
      endif
      do 150 i=1,Nvar
      hdotd1(i)=zero
      d1doth(i)=zero
150   continue
      do 200 i=1,Nvar
      do 160 j=1,Nvar
      hdotd1(i)=hdotd1(i)+(H(i,j)*Deltab(j))
      d1doth(i)=d1doth(i)+(H(i,j)*Deltab(j))
160   continue
200   continue
      zdotd1=zero
      hbar=zero
      do 250 i=1,Nvar
      zdotd1=zdotd1+(Deltaz(i)*Deltab(i))
      hbar=hbar+(Deltab(i)*hdotd1(i))
250   continue
      do 300 i=1,Nvar
      do 260 j=1,Nvar
      mone(i,j)=(Deltaz(i)*Deltaz(j))/zdotd1
      mtwo(i,j)=(-hdotd1(i)*d1doth(j))/hbar
      
      H(i,j)=H(i,j)+mone(i,j)+mtwo(i,j)
      
260   continue
300   continue
      
      if(Iop(33).NE.0.OR.Iop(34).NE.0)write(Iout,99001)K,zdotd1,hbar
      write(Iout,99003)
      call matout(H,30,30,Nvar,Nvar)
      endif
      return
      
      end
C* :1 * 
      
