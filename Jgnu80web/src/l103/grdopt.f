
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 grdopt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "grdopt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 35 "grdopt.web"
      subroutine grdopt(IOP,TOANG)
      implicit none
      double precision a,Alph,Beta,Bl,cnvfmx,cnvfx,Convf,convx,de,dxmax,
     &Dxmaxt,dxrms,Eiglim,Eigmax,Eigmin,Energy,Es,Esave,F,Fc
      double precision Ff,fmax,Fmaxt,Fnccnv,Fncerr,Frcnst,frms,Fs,Fswtch
     &,ftemp,gabs,Grderr,gsqrt,Rlim,Rmax,Rmin,scale,t1,t2,TOANG
      double precision two,X,Xname,xnew,xquad,Xx,zero
      integer i,Ianz,iblank,Ic,icur,ij,In,IOP,Iout,iozmat,Ipunch,ireslt,
     &Istep,istr,itmp,Iz,j,jpos,k,Lalpha
      integer Lbeta,Lbl,len,lzmat,Nc,ncur,Ndum,Ndum2,Neg,Nmax,Np,Nstep,N
     &var,Nvarrd,Nz
      integer getchr
      logical ok,linr
      logical Prnt,Exit
      dimension IOP(50)
      dimension xquad(50),xnew(50),ftemp(50)
      dimension a(50,50),Esave(50)
      dimension ireslt(3),itmp(20),istr(20)
      save
      common/io/In,Iout,Ipunch
      common/grdnt/Energy,F(50),Frcnst(1275),Nvar,Ndum
      common/optgrd/X(100),Xname(100),Fc(2500),Es,Fs(50),Xx(50,50),Ff(50
     &,50),Convf,Fmaxt,Dxmaxt,Rmax,Rmin,Rlim,Eigmax,Eigmin,Eiglim,Fswtch
     &,Fncerr,Grderr,Fnccnv,Ic(50),Nstep,Istep,Nmax,Np,Neg,Prnt,Exit,Ndu
     &m2
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alph(50),Beta(50),Lbl(50),Lal
     &pha(50),Lbeta(50),Nz,Nvarrd
      equivalence(Es,Esave(1)),(Nvar,Nc)
      data zero,two/0.D0,2.D0/
      data cnvfmx/1.5D0/,cnvfx/4.D0/
      data iozmat,lzmat/507,351/
      data iblank/1H /
      
99001 format(8x,'ITEM',8x,5x,2x,'VALUE',3x,2x,'THRESHOLD',2x,'CONVERGED'
     &)
99002 format(' MAXIMUM FORCE       ',5x,f8.4,5x,f8.4,5x,3A1)
99003 format(1x,'RMS     FORCE       ',5x,f8.4,5x,f8.4,5x,3A1)
99004 format(1x,'MAXIMUM DISPLACEMENT',5x,f8.6,5x,f8.6,5x,3A1)
99005 format(1x,'RMS     DISPLACEMEMT',5x,f8.6,5x,f8.6,5x,3A1)
99006 format(' THE SECOND DERIVATIVE MATRIX:')
99007 format(1x,'PREDICTED CHANGE IN ENERGY ',f9.6)
99008 format(' SEARCH FOR A LOCAL MINIMUM.')
99009 format(' SEARCH FOR A SADDLE POINT.')
99010 format(' SEARCH FOR A STATIONARY POINT OF ORDER ',i2)
99011 format(' STEP NUMBER ',i3,' OUT OF A MAXIMUM OF ',i3/1x,'ALL QUANT
     &ITIES PRINTED IN INTERNAL UNITS (HARTREES-','BOHRS-RADIANS)')
99012 format(' VARIABLE',2x,5x,'OLD X',4x,'-DE/DX',3(3x,'DELTA X'),5x,'N
     &EW X'/1x,30x,2x,'(LINEAR)',4x,'(QUAD)',3x,'(TOTAL)'/)
99013 format(1x,8A1,2x,6F10.5)
99014 format(' OPTIMIZATION COMPLETED.'/1x,'   -- STATIONARY POINT FOUND
     &.'/1x,'   -- LAST STEP NOT IMPLEMENTED.')
99015 format(' OPTIMIZATION STOPPED.'/1x,'   -- NUMBER OF STEPS EXCEEDED
     &,  NSTEP=',i4/1x,'   -- FLAG RESET TO PREVENT ARCHIVING.')
99016 format(' MAXIMUM STEP SIZE (DXMAXT= ',f8.3,') EXCEEDED''  IN LINEA
     &R SEARCH.'/1x,'   -- STEP SIZE SCALED BY ',f7.3/1x,'   -- SKIP QUA
     &DRATIC SEARCH.')
99017 format(' SECOND DERIVATIVE MATRIX NOT UPDATED -- FIRST STEP')
99018 format(' LINEAR SEARCH NOT ATTEMPTED -- OPTION 19 SET')
99019 format(' LINEAR SEARCH NOT ATTEMPTED -- NOT SEARCHING FOR ','A LOC
     &AL MINIMUM')
99020 format(' LINEAR SEARCH NOT ATTEMPTED -- FIRST POINT')
99021 format(' LINEAR SEARCH NOT ATTEMPTED -- RMS FORCE IS ','LESS THAN 
     &FSWTCH (',f8.5,')')
99022 format(' ENERGY RISES -- SKIP QUADRATIC SEARCH')
99023 format(' MAXIMUM STEP SIZE (DXMAXT= ',f8.3,') EXCEEDED'' IN QUADRA
     &TIC SEARCH.'/1x,'   -- STEP SIZE SCALED BY ',f7.3)
99024 format(' OPTIMIZATION ABORTED.'/1x,'   -- GRADIENT OUT OF RANGE'/1
     &x,'   -- MAXIMUM ALLOWED FORCE (FMAXT) = ',f8.3)
      
      Istep=Istep+1
      if(Neg.EQ.0)write(Iout,99008)
      if(Neg.EQ.1)write(Iout,99009)
      if(Neg.GT.1)write(Iout,99010)Neg
      write(Iout,99011)Istep,Nstep
      
      
      frms=zero
      fmax=zero
      do 100 i=1,Nc
      if(gabs(F(i)).GT.fmax)fmax=gabs(F(i))
      frms=frms+F(i)**2
100   continue
      frms=gsqrt(frms/Nc)
      Exit=(frms.LT.Convf).AND.(fmax.LT.cnvfmx*Convf)
      if(fmax.GE.Fmaxt)then
      ncur=0
      call putbc('-DE byDX',8,itmp,ncur)
      call putdel(2,itmp,ncur)
      call matprt(F,1,50,1,Nc,1,1,itmp,Xname,0,0,0)
      write(Iout,99024)Fmaxt
      call lnk1e
      endif
      do 200 i=1,Nc
      xnew(i)=zero
      xquad(i)=zero
      ftemp(i)=F(i)
200   continue
      
      
      if(Np.NE.0)then
      
      call d2corr(a)
      else
      write(Iout,99017)
      endif
      
      
      call savept
      write(Iout,99006)
      call matprt(Fc,Nc,Nc,Nc,Nc,1,1,Xname,Xname,1,0,0)
      
      
      linr=IOP(19).EQ.0.AND.Np.GT.1.AND.IOP(5).EQ.0
      linr=linr.AND.frms.GT.Fswtch
      if(linr)then
      call dxlinr(ftemp,xnew,dxrms,dxmax,Nc,ok)
      if(dxmax.GE.Dxmaxt)then
      scale=Dxmaxt/dxmax
      write(Iout,99016)Dxmaxt,scale
      do 220 i=1,Nc
      xnew(i)=xnew(i)*scale
220   continue
      goto 300
      
      elseif((Es-Fncerr).GT.(Fs(1)+Fncerr))then
      write(Iout,99022)
      goto 300
      endif
      endif
      
      
      
      call dxquad(ftemp,xquad,a,dxrms,dxmax,ok)
      
      
      if(.NOT.(linr))then
      if(IOP(19).NE.0)then
      write(Iout,99018)
      
      elseif(IOP(5).NE.0)then
      write(Iout,99019)
      
      elseif(Np.LE.1)then
      write(Iout,99020)
      
      elseif(frms.LT.Fswtch)then
      write(Iout,99021)Fswtch
      endif
      endif
      
      
      if(dxmax.GE.Dxmaxt)then
      scale=Dxmaxt/dxmax
      write(Iout,99023)Dxmaxt,scale
      do 250 i=1,Nc
      xquad(i)=xquad(i)*scale
250   continue
      endif
300   dxmax=zero
      dxrms=zero
      do 400 i=1,Nc
      a(1,i)=X(i)
      a(2,i)=F(i)
      a(3,i)=xnew(i)
      a(4,i)=xquad(i)
      xnew(i)=xnew(i)+xquad(i)
      X(i)=X(i)+xnew(i)
      a(5,i)=xnew(i)
      a(6,i)=X(i)
      dxrms=dxrms+xnew(i)**2
      if(gabs(xnew(i)).GT.dxmax)dxmax=gabs(xnew(i))
400   continue
      dxrms=gsqrt(dxrms/Nc)
      
      
      de=zero
      ij=0
      do 500 i=1,Nc
      do 450 j=1,Nc
      ij=ij+1
      de=de-xnew(i)*Fc(ij)*xnew(j)/two
450   continue
500   continue
      de=de/Fnccnv
      convx=cnvfx*Convf
      Exit=Exit.AND.dxrms.LT.convx.AND.dxmax.LT.cnvfmx*convx
      Exit=Exit.AND.Istep.NE.1
      
      
      write(Iout,99012)
      ncur=0
      do 700 i=1,Nc
      call getb(2,itmp,len,Xname,ncur)
      do 550 j=1,8
      istr(j)=iblank
550   continue
      icur=0
      len=min0(len,8)
      jpos=(10-len)/2
      do 600 j=1,len
      istr(jpos)=getchr(itmp,icur)
      jpos=jpos+1
600   continue
      write(Iout,99013)(istr(k),k=1,8),(a(j,i),j=1,6)
700   continue
      t1=Convf*cnvfmx
      t2=convx*cnvfmx
      write(Iout,99001)
      call convgd(fmax,t1,ireslt)
      write(Iout,99002)fmax,t1,ireslt
      call convgd(frms,Convf,ireslt)
      write(Iout,99003)frms,Convf,ireslt
      call convgd(dxmax,t2,ireslt)
      write(Iout,99004)dxmax,t2,ireslt
      call convgd(dxrms,convx,ireslt)
      write(Iout,99005)dxrms,convx,ireslt
      write(Iout,99007)de
      
      
      if(Exit)then
      write(Iout,99014)
      call tread(iozmat,Ianz,lzmat,1,lzmat,1,0)
      do 750 i=1,Nc
      X(i)=X(i)-xnew(i)
      Ic(i)=99
750   continue
      call prmtbl(1,Xname,X,Ic,F,Nc,Lbl,Nz,TOANG)
      return
      endif
      
      
      if(Istep.LT.Nstep)return
      write(Iout,99015)Nstep
      Exit=.TRUE.
      call ilsw(1,25,0)
      call tread(iozmat,Ianz,lzmat,1,lzmat,1,0)
      do 800 i=1,Nc
      Ic(i)=99
800   continue
      call prmtbl(2,Xname,X,Ic,F,Nc,Lbl,Nz,TOANG)
      return
      
      
      end
C* :1 * 
      
