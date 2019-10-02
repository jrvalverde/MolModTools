
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fpexit"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fpexit.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 17 "fpexit.web"
      subroutine fpexit
      implicit none
      double precision Alpha,Alphaz,autoan,Beta,Bl,Convrg,D1var,D1vold,D
     &2var,delloc,Delvar,F,F1,f45,Fzero,gatan,H,one,Pool0,Pool1
      double precision Tcurcy,Telcur,Teltot,Tlstcy,Tmax,todeg,varloc,Vna
     &me,Xi,Yold
      integer i,Ianz,iblank,icur,Idone,Iflfp,Iflinf,Ifpgen,Igen,Ihflag,I
     &n,Incldh,Index,Intent,Iofp,Iogen,Iout,iozmat,Ipunch,Iret
      integer Irmsf,Isect,Istats,istr,Ititle,itmp,Itype,ivar,Iz,iz1,j,Ju
     &mp,K,Lalpha,Lambda,Lbeta,Lbl,len,Mode,ncur
      integer Ncyc,Ncycls,Ncytot,Noinch,Noruns,Npar,Nstep,Nvar,Nvarrd,Nz
      integer getchr
      dimension delloc(50),varloc(50)
      dimension itmp(8),istr(8)
      common/j102/Jump,Iret
      common/fpinfo/Pool0(30),Pool1(30),Delvar(30),Yold(30),D1var(30),D2
     &var(30),D1vold(30),Xi(30),Fzero,F1(4),F,Alpha,Convrg,Teltot,Telcur
     &,Tlstcy,Tmax,Tcurcy,Iflinf,K,Nvar,Npar,Itype,Ncycls,Isect,Ncyc,Nor
     &uns,Ncytot,Ititle(8),Mode,Nstep,Index,Lambda,Ihflag,Idone,H(30,30)
     &,Noinch,Incldh,Vname(30),Istats,Intent
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alphaz(50),Beta(50),Lbl(50),L
     &alpha(50),Lbeta(50),Nz,Nvarrd
      common/irw/Iogen,Irmsf,Iofp,Ifpgen
      common/gen/Igen(94)
      common/io/In,Iout,Ipunch
      equivalence(Iflfp,Igen(1))
      data iozmat/507/
      data f45/45.D0/,one/1.0D0/,autoan/0.52917706D0/
      data iblank/1H /
      
      
      
      
      
      
      
99001 format(2x,8A1,f10.5,5x,f10.5)
99002 format(1x,'FINAL POOL OF VARIABLES:'/1x,' NAME',9x,'VALUE',8x,'INC
     &REMENT'/1x,4(1H-),9x,5(1H-),8x,9(1H-))
99003 format(1x,' FINAL OPTIMIZED VALUE = ',d20.13)
      
      
      call tread(iozmat,Ianz,351,1,351,1,0)
      todeg=f45/gatan(one)
      do 200 ivar=1,Nvar
      
      
      do 50 iz1=2,Nz
      Index=Lbl(iz1)
      if(Index.EQ.ivar)goto 100
50    continue
      
      
      delloc(ivar)=Delvar(ivar)*todeg
      varloc(ivar)=Pool0(ivar)*todeg
      goto 200
      
      
100   delloc(ivar)=Delvar(ivar)*autoan
      varloc(ivar)=Pool0(ivar)*autoan
      
200   continue
      
      
      
      write(Iout,99002)
      ncur=0
      do 400 i=1,Nvar
      call getb(2,itmp,len,Vname,ncur)
      do 250 j=1,8
      istr(j)=iblank
250   continue
      len=min0(len,8)
      icur=0
      do 300 j=1,len
      istr(j)=getchr(itmp,icur)
300   continue
      write(Iout,99001)(istr(j),j=1,8),varloc(i),delloc(i)
400   continue
      write(Iout,99003)Fzero
      
      
      Istats=3
      Iflfp=Iflinf
      call tread(Ifpgen,Igen,47,1,47,1,0)
      call twrite(Iogen,Igen,47,1,47,1,0)
      call twrite(Iofp,Pool0(1),1197,1,1197,1,0)
      call udfp(Pool0)
      
      Jump=1
      Iret=1
      return
      
      end
C* :1 * 
      
