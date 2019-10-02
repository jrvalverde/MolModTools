
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sfopt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sfopt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "sfopt.web"
      subroutine sfopt
      implicit none
      double precision Alpha,C1,C2,C3,C4,Convrg,D1var,D1vold,D2var,Delva
     &r,Exx,F,F1,Fzero,H,Pool0,Pool1,sc,Scale,Tcurcy
      double precision Telcur,Teltot,Tlstcy,Tmax,Vname,X,Xi,Y,Yold,Z
      integer i,Idone,iend,Iflinf,Ihflag,imap,In,Incldh,Index,Inf,Intent
     &,iofp,Iold,Iout,Ipunch,Isect,Isfmap,ishell,istart,Istats
      integer Ititle,Itype,ivar,Jan,K,Lambda,lenfps,MAXPRM,MAXS21,MAXSH1
     &,MAXSHL,Maxtyp,Mode,Ncyc,Ncycls,Ncytot,Noinch,Noruns,Npar,nsfvar
      integer Nshell,Nstep,Nvar
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      dimension Inf(2394),Isfmap(30,2)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/scale/Scale(MAXSHL)
      common/io/In,Iout,Ipunch
      common/fpinfo/Pool0(30),Pool1(30),Delvar(30),Yold(30),D1var(30),D2
     &var(30),D1vold(30),Xi(30),Fzero,F1(4),F,Alpha,Convrg,Teltot,Telcur
     &,Tlstcy,Tmax,Tcurcy,Iflinf,K,Nvar,Npar,Itype,Ncycls,Isect,Ncyc,Nor
     &uns,Ncytot,Ititle(8),Mode,Nstep,Index,Lambda,Ihflag,Idone,H(30,30)
     &,Noinch,Incldh,Vname(30),Istats,Intent
      equivalence(Inf(1),Pool0(1))
      equivalence(Iold,Tmax)
      equivalence(Isfmap(1,1),Inf(382))
      data iofp/510/,lenfps/400/
      
      
      
      
      
      
      
      
      
      call tread(iofp,Inf(1),lenfps,1,lenfps,1,0)
      
      
      do 100 imap=1,nsfvar
      ishell=Isfmap(imap,1)
      ivar=Isfmap(imap,2)
      istart=Shella(ishell)
      iend=Shella(ishell)+Shelln(ishell)-1
      sc=Pool0(ivar)
      Scale(ishell)=sc
      do 50 i=istart,iend
      Exx(i)=Exx(i)*sc*sc
50    continue
100   continue
      
      return
      
99001 format(' IN SFOPT, NSFVAR,NVAR,NPAR=',3I6)
99002 format(' POOL0'/(1x,d20.10))
99003 format(' ISFMAP'/(1x,2I6))
      
      end
C* :1 * 
      
