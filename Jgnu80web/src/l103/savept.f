
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 savept"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "savept.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "savept.web"
      subroutine savept
      implicit none
      double precision Convf,Dxmaxt,Eiglim,Eigmax,Eigmin,Energy,Es,Esave
     &,F,Fc,Ff,Fmaxt,Fnccnv,Fncerr,Frcnst,Fs,Fswtch,Grderr,Rlim,Rmax
      double precision Rmin,X,Xname,Xx
      integer i,Ic,ii,iip,In,Iout,ip,Ipunch,Istep,Nc,Ndum,Ndum2,Neg,Nmax
     &,Np,Nstep,Nvar
      logical Prnt,Exit
      dimension Esave(50)
      common/grdnt/Energy,F(50),Frcnst(1275),Nvar,Ndum
      common/optgrd/X(100),Xname(100),Fc(2500),Es,Fs(50),Xx(50,50),Ff(50
     &,50),Convf,Fmaxt,Dxmaxt,Rmax,Rmin,Rlim,Eigmax,Eigmin,Eiglim,Fswtch
     &,Fncerr,Grderr,Fnccnv,Ic(50),Nstep,Istep,Nmax,Np,Neg,Prnt,Exit,Ndu
     &m2
      common/io/In,Iout,Ipunch
      equivalence(Es,Esave(1)),(Nvar,Nc)
      
      
      Np=Np+1
      if(Np.GE.2)then
      do 50 iip=2,Np
      ip=Np+1-iip
      ii=ip+1
      Esave(ii)=Esave(ip)
      Ic(ii)=Ic(ip)
      do 20 i=1,Nc
      Ff(i,ii)=Ff(i,ip)
      Xx(i,ii)=Xx(i,ip)
20    continue
50    continue
      endif
      Esave(1)=Energy
      Ic(1)=Istep
      do 100 i=1,Nc
      Xx(i,1)=X(i)
      Ff(i,1)=F(i)
100   continue
      if(Np.GT.Nmax)Np=Nmax
      return
      
      end
C* :1 * 
      
