
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 value"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "value.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 16 "value.web"
      subroutine value
      implicit none
      double precision Alpha,Convrg,D1var,D1vold,D2var,Delvar,e,F,F1,Fze
     &ro,H,Pool0,Pool1,Tcurcy,Telcur,Teltot,Tlstcy,Tmax,Vname,Xi
      double precision Yold
      integer idoll,Idone,Iflinf,Ifpgen,Igen,Ihflag,In,Incldh,Index,Inte
     &nt,Iofp,Iogen,Iop,Iout,Ipunch,Iret,Irmsf,Isect,Istats,itest
      integer Ititle,Itype,Jump,K,Lambda,Mode,Ncyc,Ncycls,Ncytot,Noinch,
     &Noruns,Npar,Nstep,Nvar
      common/j102/Jump,Iret
      common/iop/Iop(50)
      common/io/In,Iout,Ipunch
      common/irw/Iogen,Irmsf,Iofp,Ifpgen
      common/gen/Igen(94)
      common/fpinfo/Pool0(30),Pool1(30),Delvar(30),Yold(30),D1var(30),D2
     &var(30),D1vold(30),Xi(30),Fzero,F1(4),F,Alpha,Convrg,Teltot,Telcur
     &,Tlstcy,Tmax,Tcurcy,Iflinf,K,Nvar,Npar,Itype,Ncycls,Isect,Ncyc,Nor
     &uns,Ncytot,Ititle(8),Mode,Nstep,Index,Lambda,Ihflag,Idone,H(30,30)
     &,Noinch,Incldh,Vname(30),Istats,Intent
      data idoll/2H##/
      
      
      
      
      
99001 format(a2,11x,d30.20)
99002 format(' CARD READ BY VALUE: ',a2,11x,d30.20)
99003 format(' TRANSFERRING CONTROL TO GAUSSIAN SYSTEM,  ISECT=',i1,',  
     &NCYC=',i2,',  ISTATS=',i1,'.')
      
      Idone=1
      if(Idone.EQ.0)then
      read(In,99001)itest,e
      write(Iout,99002)itest,e
      if(itest.NE.idoll)then
      F=e
      return
      else
      
      Idone=1
      endif
      endif
      call twrite(Iogen,Igen,47,1,47,1,0)
      call twrite(Iofp,Pool0(1),1197,1,1197,1,0)
      call udfp(Pool0)
      if(Iop(34).NE.0.OR.Iop(33).NE.0)write(Iout,99003)Isect,Ncyc,Istats
      if(Iop(34).GE.1)call fpdump
      
      Jump=0
      Iret=1
      return
      
      end
C* :1 * 
      
