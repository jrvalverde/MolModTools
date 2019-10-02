
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 enter"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "enter.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "enter.web"
      subroutine enter(TOANG)
      implicit none
      double precision Alpha,Alphaz,Beta,Bl,Convrg,D1var,D1vold,D2var,de
     &lloc,Delvar,F,F1,Fgen,Fzero,H,Pool0,Pool1,Tcurcy,Telcur,Teltot
      double precision Tlstcy,Tmax,TOANG,varloc,Vname,Xi,Yold
      integer Ianz,idate,Idone,ifcont,Iflfp,Iflinf,Ifpgen,Igen,Ihflag,In
     &,Incldh,Index,Intent,Iofp,Iogen,Iold,Iop,Iout,Ipunch,Irmsf
      integer Isect,Istats,itime,Ititle,Itype,Iz,K,Lalpha,Lambda,Lbeta,L
     &bl,Mode,Ncyc,Ncycls,Ncytot,Noinch,Noruns,Npar,Nstep,Nvar
      integer Nvarrd,Nz
      dimension varloc(30),delloc(30)
      dimension idate(3),itime(2)
      common/io/In,Iout,Ipunch
      common/irw/Iogen,Irmsf,Iofp,Ifpgen
      common/gen/Igen(94)
      common/fpinfo/Pool0(30),Pool1(30),Delvar(30),Yold(30),D1var(30),D2
     &var(30),D1vold(30),Xi(30),Fzero,F1(4),F,Alpha,Convrg,Teltot,Telcur
     &,Tlstcy,Tmax,Tcurcy,Iflinf,K,Nvar,Npar,Itype,Ncycls,Isect,Ncyc,Nor
     &uns,Ncytot,Ititle(8),Mode,Nstep,Index,Lambda,Ihflag,Idone,H(30,30)
     &,Noinch,Incldh,Vname(30),Istats,Intent
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alphaz(50),Beta(50),Lbl(50),L
     &alpha(50),Lbeta(50),Nz,Nvarrd
      common/iop/Iop(50)
      equivalence(Iold,Tmax)
      equivalence(Fgen,Igen(85))
      equivalence(Iflfp,Igen(1))
      
      
      
      
      
      
      
      
      
99001 format(1x,'FLETCHER-POWELL TOTAL OPTIMIZATION PROGRAM. ')
99002 format(1x,'FLETCHER-POWELL TOTAL OPTIMIZATION PROGRAM. ')
      
      Iogen=501
      Iofp=510
      Irmsf=21
      Ifpgen=553
      call tread(Iogen,Igen,47,1,47,1,0)
      
      
      call ilsw(2,19,ifcont)
      call ilsw(1,19,1)
      if(Iop(12).EQ.1)then
      
      write(Iout,99002)
      Intent=1
      if(Isect.EQ.1)Intent=0
      return
      elseif(ifcont.NE.1)then
      call initfp(TOANG)
      Idone=0
      Istats=1
      Isect=1
      write(Iout,99001)
      return
      endif
      call tread(Iofp,Pool0(1),1197,1,1197,1,0)
      if(Iop(34).GE.2)call fpdump
      Istats=0
      F=Fgen
      Intent=1
      if(Isect.EQ.1)Intent=0
      return
      
      end
C* :1 * 
      
