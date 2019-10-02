
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 estm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "estm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 16 "estm.web"
      subroutine estm
      implicit none
      double precision a1,a2,aa,aaa,Alpha,autoan,b1,b2,bbb,Beta,Bl,conbn
     &d,constr,Convf,Dxmaxt,Eiglim,Eigmax,Eigmin,Energy,Es
      double precision F,Fc,Ff,Fmaxt,Fnccnv,Fncerr,Frcnst,Fs,Fswtch,Grde
     &rr,hartre,one,Rlim,Rmax,Rmin,X,xangst,Xname,Xx
      integer i,ia,Ianz,iatno,ib,Ic,ii,In,Iout,iozmat,Ipunch,irow,isave,
     &Istep,ivar,Iz,jatno,jatom,Lalpha,Lbeta
      integer Lbl,Ndum,Ndum2,Neg,Nmax,Np,nrep,nsave,Nstep,Nvar,Nvarrd,Nz
      logical Prnt,Exit
      integer maxz
      dimension aa(3,3)
      dimension irow(86)
      dimension xangst(50),isave(50)
      common/io/In,Iout,Ipunch
      common/grdnt/Energy,F(50),Frcnst(1275),Nvar,Ndum
      common/optgrd/X(100),Xname(100),Fc(2500),Es,Fs(50),Xx(50,50),Ff(50
     &,50),Convf,Fmaxt,Dxmaxt,Rmax,Rmin,Rlim,Eigmax,Eigmin,Eiglim,Fswtch
     &,Fncerr,Grderr,Fnccnv,Ic(50),Nstep,Istep,Nmax,Np,Neg,Prnt,Exit,Ndu
     &m2
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alpha(50),Beta(50),Lbl(50),La
     &lpha(50),Lbeta(50),Nz,Nvarrd
      data maxz/86/
      data a1,a2/5.38D0,4.00D0/
      data aa/-.129D0,0.186D0,0.349D0,0.186D0,0.574D0,0.805D0,0.349D0,0.
     &805D0,1.094D0/
      data b1,b2/1.35D0,1.0D0/
      data irow/2*1,8*2,8*3,68*3/
      data one/1.D0/
      data autoan/0.52917706D0/,hartre/4.359814D0/
      data iozmat/507/
      
      
      
      
99001 format(1x,'ATOMIC NUMBER TOO LARGE IN SUBROUTINE ESTM')
      
      
      do 100 i=1,Nz
      if(Ianz(i).GT.maxz)then
      write(Iout,99001)
      call lnk1e
      endif
100   continue
      
      call tread(iozmat,Ianz,351,1,351,1,0)
      call ilsw(2,3,ia)
      aaa=a1
      if(ia.NE.0)aaa=a2
      bbb=b1
      if(ia.NE.0)bbb=b2
      if(Nz.GE.2)then
      
      do 150 i=1,Nvar
      xangst(i)=X(i)
      if(nrep(i,Lbl,Nz).NE.0)xangst(i)=xangst(i)*autoan
150   continue
      do 200 i=2,Nz
      ivar=iabs(Lbl(i))
      if(ivar.NE.0)then
      iatno=Ianz(i)
      ia=irow(iatno)
      if(iatno.LT.1)ia=1
      jatom=Iz(i,1)
      jatno=Ianz(jatom)
      ib=irow(jatno)
      if(jatno.LT.1)ib=1
      ii=ivar+(ivar-1)*Nvar
      if(Ic(ivar).EQ.0)Fc(ii)=Fc(ii)+aaa/((xangst(ivar)-aa(ia,ib))**3)
      endif
200   continue
      if(Nz.GE.3)then
      
      do 220 i=3,Nz
      ivar=iabs(Lalpha(i))
      if(ivar.NE.0)then
      ii=ivar+(ivar-1)*Nvar
      if(Ic(ivar).EQ.0)Fc(ii)=Fc(ii)+bbb
      endif
220   continue
      if(Nz.GE.4)then
      
      do 230 i=4,Nz
      ivar=iabs(Lbeta(i))
      if(ivar.NE.0)then
      ii=ivar+(ivar-1)*Nvar
      if(Ic(ivar).EQ.0)Fc(ii)=Fc(ii)+bbb
      endif
230   continue
      endif
      endif
      endif
      
      constr=autoan**2/hartre
      conbnd=one/hartre
      call iclear(50,isave)
      nsave=0
      do 300 i=2,Nz
      ivar=iabs(Lbl(i))
      if(ivar.NE.0.AND.nrep(ivar,isave,nsave).EQ.0)then
      nsave=nsave+1
      isave(nsave)=ivar
      ii=ivar+(ivar-1)*Nvar
      if(Ic(ivar).EQ.0)Fc(ii)=Fc(ii)*constr
      endif
300   continue
      
      call iclear(nsave,isave)
      nsave=0
      do 400 i=3,Nz
      ivar=iabs(Lalpha(i))
      if(ivar.NE.0.AND.nrep(ivar,isave,nsave).EQ.0)then
      nsave=nsave+1
      isave(nsave)=ivar
      ii=ivar+(ivar-1)*Nvar
      if(Ic(ivar).EQ.0)Fc(ii)=Fc(ii)*conbnd
      endif
400   continue
      
      do 500 i=4,Nz
      ivar=iabs(Lbeta(i))
      if(ivar.NE.0.AND.nrep(ivar,isave,nsave).EQ.0)then
      nsave=nsave+1
      isave(nsave)=ivar
      ii=ivar+(ivar-1)*Nvar
      if(Ic(ivar).EQ.0)Fc(ii)=Fc(ii)*conbnd
      endif
500   continue
      
      return
      
      end
C* :1 * 
      
