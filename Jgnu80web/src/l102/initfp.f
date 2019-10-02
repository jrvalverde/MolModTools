
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 initfp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "initfp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 17 "initfp.web"
      subroutine initfp
      implicit none
      double precision Alpha,Alphaz,Anames,antoau,autoan,Beta,Bl,Convrg,
     &D1var,D1vold,D2var,dblank,delbnd,delstr,Delvar,F,F1,f45,Fpvec,Fzer
     &o
      double precision gatan,H,one,Pool0,Pool1,Tcurcy,Telcur,Teltot,Tlst
     &cy,Tmax,torad,Values,Vname,Xi,Yold
      integer i,Ianz,icur,Idone,Iflinf,Ihflag,In,Incldh,Index,Intent,Int
     &vec,Iout,iozmat,iozsub,Ipunch,Isect,Istats,Ititle,itmp,Itype
      integer Iz,j,K,Lalpha,Lambda,Lbeta,Lbl,len,Mode,ncur,Ncyc,Ncycls,N
     &cytot,Noinch,Noruns,Npar,Nstep,Nvar,Nvarrd,Nz
      dimension itmp(20)
      common/io/In,Iout,Ipunch
      common/fpinfo/Pool0(30),Pool1(30),Delvar(30),Yold(30),D1var(30),D2
     &var(30),D1vold(30),Xi(30),Fzero,F1(4),F,Alpha,Convrg,Teltot,Telcur
     &,Tlstcy,Tmax,Tcurcy,Iflinf,K,Nvar,Npar,Itype,Ncycls,Isect,Ncyc,Nor
     &uns,Ncytot,Ititle(8),Mode,Nstep,Index,Lambda,Ihflag,Idone,H(30,30)
     &,Noinch,Incldh,Vname(30),Istats,Intent
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alphaz(50),Beta(50),Lbl(50),L
     &alpha(50),Lbeta(50),Nz,Nvarrd
      common/zsubst/Anames(50),Values(50),Intvec(50),Fpvec(50)
      data iozmat/507/,iozsub/570/
      data delstr/0.01D0/,delbnd/1.0D0/
      data autoan/0.52917706D0/
      data one/1.0D0/,f45/45.0D0/
      data dblank/1H /
      
      
      
      
      
      
      
99001 format(1x,'NVAR OUTSIDE OF VALID RANGE IN LINK 102, NVAR= ',i10)
99002 format(1x,'SYMBOL ',i4,' IN SUBROUTINE INITFP IN LINK 102 IS ','NE
     &ITHER BOND LENGTH NOR ANGLE')
      
      
      call tread(iozmat,Ianz,351,1,351,1,0)
      if(Nvarrd.LE.0.OR.Nvarrd.GT.30)then
      write(Iout,99001)Nvarrd
      call lnk1e
      endif
      call tread(iozsub,Anames,175,1,175,1,0)
      
      
      Itype=2
      Iflinf=10
      Ncycls=5
      
      
      ncur=0
      icur=0
      do 100 i=1,30
      Vname(i)=dblank
100   continue
      
      Nvar=Nvarrd
      Npar=Nvarrd
      do 200 i=1,Nvar
      Pool0(i)=Values(i)
      Pool1(i)=Values(i)
      call getb(2,itmp,len,Anames,ncur)
      call putb(itmp,len,Vname,icur)
      call putdel(2,Vname,icur)
200   continue
      
      
      antoau=one/autoan
      torad=gatan(one)/f45
      do 400 i=1,Nvar
      do 250 j=2,Nz
      if(iabs(Lbl(j)).EQ.i)then
      Delvar(i)=delstr*antoau
      if(Intvec(i).NE.0)Delvar(i)=Fpvec(i)*antoau
      goto 400
      endif
      
250   continue
      do 300 j=3,Nz
      if(iabs(Lalpha(j)).EQ.i.OR.iabs(Lbeta(j)).EQ.i)then
      Delvar(i)=delbnd*torad
      if(Intvec(i).NE.0)Delvar(i)=Fpvec(i)*torad
      goto 400
      endif
      
300   continue
      write(Iout,99002)i
      call lnk1e
400   continue
      
      return
      
      end
C* :1 * 
      
