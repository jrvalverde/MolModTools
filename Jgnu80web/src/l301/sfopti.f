
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sfopti"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sfopti.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "sfopti.web"
      subroutine sfopti(IOP)
      implicit none
      double precision Alpha,C1,C2,C3,C4,Convrg,D1var,D1vold,D2var,defde
     &l,Delvar,Exx,F,F1,Fzero,H,one,Pool0,Pool1,rescal
      double precision Scale,Tcurcy,Telcur,Teltot,temp,temp1,Tlstcy,Vnam
     &e,X,Xi,Y,Yold,Z,zero
      integer i,Idone,iend,iflg,Iflinf,Ihflag,In,Incldh,Index,Inf,Intent
     &,iofp,Iold,IOP,Iout,Ipunch,irwbv,isave,Isect,Isfmap
      integer isfo,ishell,istart,Istats,Ititle,Itype,Jan,K,Lambda,len,LE
     &NB,lenfp,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,Mode,Ncyc,Ncycls
      integer Ncytot,Noinch,Noruns,Npar,nsfvar,Nshell,Nstep,Nvar,nvarmx
      real Tmax
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      dimension IOP(50)
      dimension Inf(2394),Isfmap(30,2)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
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
      data zero/0.0D0/,one/1.0D0/,nvarmx/30/
      data defdel/0.01D0/,iofp/510/,lenfp/1563/
      data irwbv/550/
      
      
      
      
      
      
      
      
99001 format(' UN-SCALING REQUIRED FOR SHELL ',i3,'   SCALE=',d20.10)
99002 format(2I4)
99003 format(' UNACCEPTABLE VALUE OF NSFVAR:',i6)
99004 format(' SCALE-FACTOR VARIBLE MAP'/' ************************')
99005 format(6x,2I6)
99006 format(2D20.10)
99007 format(1x,2D20.10)
99008 format(6x,' POOL OF VARIABLES AND CONSTANTS'/6x,' ****************
     &***************')
99009 format(' SFOPTI DONE,  NVAR=',i2,'   NPAR=',i2)
99010 format(' SORRY ... SCALE FACTOR OPTIMIZATIONS ARE NOT'/'          
     & IMPLEMENTED FOR NVAR .GT. 0',i6)
      
      
      
      
      iflg=0
      do 100 ishell=1,Nshell
      if(Scale(ishell).NE.one)then
      iflg=1
      
      write(Iout,99001)ishell,Scale(ishell)
      istart=Shella(ishell)
      iend=istart+Shelln(ishell)-1
      rescal=one/(Scale(ishell)**2)
      do 20 i=istart,iend
      Exx(i)=Exx(i)*rescal
20    continue
      endif
100   continue
      if(iflg.EQ.1)call twrite(irwbv,Exx(1),LENB,1,LENB,1,0)
      
      call ilsw(1,isfo,1)
      
      istart=0
      call tquery(iofp,len)
      if(len.NE.0)then
      
      
      call tread(iofp,Inf(1),lenfp,1,lenfp,1,0)
      
      if(Npar.LE.0)then
      write(Iout,99010)Npar
      call lnk1e
      endif
      
      istart=Nvar
      else
      
      
      Ncycls=5
      Itype=2
      
      
      Idone=0
      Iflinf=10
      Istats=0
      Isect=2
      Ncyc=0
      Ncytot=0
      Noruns=0
      endif
      
      write(Iout,99004)
      i=0
200   i=i+1
      read(In,99002)Isfmap(i,1),Isfmap(i,2)
      write(Iout,99005)Isfmap(i,1),Isfmap(i,2)
      if(Isfmap(i,1).GT.0)goto 200
      nsfvar=i-1
      if(nsfvar.LE.nvarmx)goto 400
300   write(Iout,99003)nsfvar
      call lnk1e
400   if(nsfvar.LE.0)goto 300
      
      write(Iout,99008)
      i=istart
500   i=i+1
      read(In,99006)temp,temp1
      if(temp.EQ.zero)then
      if(i.EQ.(istart+1))goto 800
      endif
      Pool0(i)=temp
      Delvar(i)=temp1
      if(Delvar(i).EQ.zero)Delvar(i)=defdel
      write(Iout,99007)Pool0(i),Delvar(i)
      if(Pool0(i).NE.zero)goto 500
      Nvar=i-1
      isave=i
      i=Nvar
600   i=i+1
      read(In,99006)Pool0(i)
      write(Iout,99007)Pool0(i)
      if(Pool0(i).NE.zero)goto 600
      Npar=i-Nvar-1
      write(Iout,99009)Nvar,Npar
      
      do 700 i=1,Nvar
      Pool1(i)=Pool0(i)
700   continue
      
800   if(IOP(34).NE.0)call fpdump
      call twrite(iofp,Inf(1),lenfp,1,lenfp,1,0)
      return
      
      end
C* :1 * 
      
