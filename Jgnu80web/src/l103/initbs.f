
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 initbs"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "initbs.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "initbs.web"
      subroutine initbs(IOP,TOANG)
      implicit none
      double precision Alpha,Anames,Beta,Bl,blank,chknam,Cnst,Convf,cpar
     &m,cprm,cutoff,delta,Dxmaxt,Eiglim,Eigmax,Eigmin,Energy,Es,F,Fc
      double precision Ff,ffin,Fmaxt,Fnccnv,Fncerr,Fpvec,Frcnst,Fs,Fswtc
     &h,gabs,gfloat,Grderr,one,pt001,pt01,pt1,Rlim,Rmax,Rmin,TOANG
      double precision Values,X,Xname,Xx,zero
      integer i,Ianz,Ic,icalce,icalcf,icalff,idx,ihop,ij,ijs,In,Intvec,I
     &OP,Iout,iozsub,Ipunch,irdfcd,Istep,ititle,itmp
      integer Iz,j,jc,ji,Lalpha,Lbeta,Lbl,len,lzsub,nb,nc,Ndum,Ndum2,Neg
     &,nel,Nmax,Np,Nstep,Nvar,Nvarrd
      integer nwrd,Nz
      logical cnstrn
      logical test,streq
      logical Prnt,Exit
      dimension IOP(50)
      dimension chknam(100),ititle(20)
      dimension itmp(16)
      dimension cparm(10),cprm(3),Cnst(10)
      common/io/In,Iout,Ipunch
      common/grdnt/Energy,F(50),Frcnst(1275),Nvar,Ndum
      common/optgrd/X(100),Xname(100),Fc(2500),Es,Fs(50),Xx(50,50),Ff(50
     &,50),Convf,Fmaxt,Dxmaxt,Rmax,Rmin,Rlim,Eigmax,Eigmin,Eiglim,Fswtch
     &,Fncerr,Grderr,Fnccnv,Ic(50),Nstep,Istep,Nmax,Np,Neg,Prnt,Exit,Ndu
     &m2
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alpha(50),Beta(50),Lbl(50),La
     &lpha(50),Lbeta(50),Nz,Nvarrd
      common/zsubst/Anames(50),Values(50),Intvec(50),Fpvec(50)
      equivalence(Cnst(1),Convf)
      data zero/0.0D0/,one/1.0D0/,pt1/0.1D0/,pt01/0.01D0/
      data pt001/0.001D0/
      data cutoff/1.0D-06/,delta/0.005D0/
      data iozsub/570/,lzsub/175/
      data irdfcd/1/,icalce/2/,icalcf/3/,icalff/-1/
      data cparm/0.0003D0,1.0D+10,0.2D0,0.6D0,0.0015D0,0.07D0,25.0D00,0.
     &0001D0,5.D-04,0.01D0/
      data cprm/1.0D-06,1.0D-06,1.D0/
      data blank/1H /
      
      
      
      
      
      
      
      
99001 format(1x,'NVAR OUT OF VALID RANGE IN LINK 103, NVAR= ',i10)
99002 format(1x,'OPTION 10 OUT OF RANGE IN L103, IOP(10)= ',i5)
99003 format(8F10.6)
99004 format(2I3,f20.0)
99005 format(10F8.6)
99006 format(' NEW OPTIMIZATION CONTROL PARAMETERS'/1x,'FMAXT,DXMAXT,RMA
     &X,RMIN,RLIM,EIGMAX,EIGMIN,EIGLIM'/1x,8F10.6)
99007 format(' ATTEMP TO READ FORCE CONSTANST MATRIX FAILS'/1x,'MIS-MATC
     &H IN VARIABLE NAMES'/1x,'LENGTH= ',i5,' EXPECTED LENGTH= ',i5)
99008 format(1x,'NAME STRING READ, FIRST 72 CHARACTERS:')
99009 format(1x,'NAME STRING EXPECTED, FIRST 72 CHARACTERS:')
99010 format(' FORCE CONSTANT MATRIX READ FROM GUESS FILE:')
99011 format(' ATTEMPT TO READ FORCE CONSTANT MATRIX FAILS'/1x,'FILE IS 
     &WRONG LENGTH'/1x,'LENGTH= ',i5,' EXPECTED LENGTH= ',i5)
      
      
      
      if(Nvarrd.GT.0.AND.Nvarrd.LE.50)then
      
      call tread(iozsub,Anames,lzsub,1,lzsub,1,0)
      
      
      do 50 i=1,50
      do 20 j=1,50
      Ff(j,i)=zero
20    continue
50    continue
      
      
      do 100 i=1,100
      Xname(i)=blank
100   continue
      nc=0
      jc=0
      do 150 i=1,Nvarrd
      call getb(2,itmp,len,Anames,nc)
      call putb(itmp,len,Xname,jc)
      call putdel(2,Xname,jc)
      X(i)=Values(i)
      Ic(i)=Intvec(i)
      Ff(i,i)=Fpvec(i)
150   continue
      Nvar=Nvarrd
      
      
      ihop=IOP(10)
      if(ihop.NE.0)then
      if(ihop.GT.4)then
      write(Iout,99002)ihop
      call lnk1e
      endif
      if(ihop.EQ.2)then
      
160   read(In,99004)i,j,ffin
      if(i.NE.0)then
      Ff(i,j)=ffin
      Ff(j,i)=ffin
      goto 160
      endif
      elseif(ihop.EQ.3)then
      
      nel=200
      call binrd(chknam,ititle,16HVARIABLE NAMES  ,nwrd,nb)
      test=nwrd.EQ.nel
      test=test.AND.streq(chknam,Xname,200)
      write(Iout,99010)
      call strout(Iout,ititle,72,1)
      if(.NOT.(test))then
      write(Iout,99007)nwrd,nel
      write(Iout,99008)
      call strout(Iout,chknam,72,1)
      write(Iout,99009)
      call strout(Iout,Xname,72,1)
      call lnk1e
      endif
      call binrd(Frcnst,ititle,16HFORCE CONSTANTS ,nwrd,nb)
      nel=Nvar*(Nvar+1)
      test=nwrd.EQ.nel
      if(.NOT.(test))then
      write(Iout,99011)nwrd,nel
      call lnk1e
      endif
      ijs=0
      do 170 i=1,Nvar
      do 165 j=1,i
      ijs=ijs+1
      ij=Nvar*(j-1)+i
      ji=Nvar*(i-1)+j
      Fc(ij)=Frcnst(ijs)
      Fc(ji)=Frcnst(ijs)
165   continue
170   continue
      do 180 i=1,Nvar
      idx=Nvar*(i-1)+i
      if(Ic(i).EQ.1)Fc(idx)=Ff(i,i)
      if(Ic(i).EQ.0)Ic(i)=1
      test=Ic(i).EQ.2.OR.Ic(i).EQ.3
      if(test)Fc(idx)=Ff(i,i)
      if(test.AND.Ff(i,i).LT.cutoff)Fc(idx)=delta
180   continue
      goto 300
      elseif(ihop.NE.4)then
      
      read(In,99003)((Ff(i,j),j=1,i),i=1,Nvar)
      endif
      endif
      
      
      do 250 i=1,Nvar
      if((Ic(i).NE.icalce).AND.(Ic(i).NE.icalcf).AND.(gabs(Ff(i,i)).GT.c
     &utoff))Ic(i)=irdfcd
      if(((Ic(i).EQ.icalce).OR.(Ic(i).EQ.icalcf)).AND.(gabs(Ff(i,i)).LE.
     &cutoff))Ff(i,i)=delta
      if(IOP(10).EQ.4)Ic(i)=icalff
      if(IOP(10).EQ.4)Ff(i,i)=zero
      do 200 j=1,i
      ij=i+(j-1)*Nvar
      ji=j+(i-1)*Nvar
      Fc(ij)=Ff(i,j)
      Fc(ji)=Ff(i,j)
200   continue
250   continue
      else
      write(Iout,99001)Nvar
      call lnk1e
      stop
      endif
      
      
300   call prmtbl(0,Xname,X,Ic,Fc,Nvar,Lbl,Nz,TOANG)
      
      
      Np=-1
      Istep=0
      Neg=IOP(5)
      cnstrn=.FALSE.
      Nmax=max0(Nvar,49)
      Nstep=min0(20,Nvar+10)
      Prnt=IOP(33).NE.0.OR.IOP(34).NE.0
      
      
      do 400 i=1,10
      Cnst(i)=cparm(i)
400   continue
      
      
      if(IOP(6).NE.0)Nstep=IOP(6)
      if(IOP(7).NE.0)Convf=pt001/gfloat(IOP(7))
      if(IOP(8).NE.0)Dxmaxt=pt01*gfloat(IOP(8))
      if(IOP(14).NE.0)Fswtch=pt001*gfloat(IOP(14))
      if(IOP(15).NE.0)Fmaxt=pt1*gfloat(IOP(15))
      if(IOP(16).NE.0)Eigmax=gfloat(IOP(16))
      if(IOP(17).NE.0)Eigmin=one/gfloat(IOP(17))
      if(IOP(12).NE.0)read(In,99005)(Cnst(i),i=1,10)
      if(IOP(12).NE.0)write(Iout,99006)(Cnst(i),i=1,10)
      
      
      Fncerr=cprm(1)
      Grderr=cprm(2)
      Fnccnv=cprm(3)
      
      return
      
      end
C* :1 * 
      
