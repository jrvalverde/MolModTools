
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 d2corr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "d2corr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "d2corr.web"
      subroutine d2corr(A)
      implicit none
      double precision A,Convf,ddx,deltak,dx,Dxmaxt,Eiglim,Eigmax,Eigmin
     &,Energy,error,Es,Esave,F,Fc,fcnew,fcold,Ff,Fmaxt,Fnccnv
      double precision Fncerr,Frcnst,Fs,Fswtch,gabs,Grderr,gsqrt,Rlim,Rm
     &ax,Rmin,rx,X,Xname,Xx,zero
      integer i,Ic,ii,ij,In,Iout,ip,Ipunch,is,Istep,j,jp,Nc,Ndum,Ndum2,N
     &eg,Nmax,nn,Np,Nstep
      integer Nvar
      logical Prnt,Exit
      dimension A(50,50),fcold(50),fcnew(50)
      dimension Esave(50)
      dimension is(50)
      common/grdnt/Energy,F(50),Frcnst(1275),Nvar,Ndum
      common/optgrd/X(100),Xname(100),Fc(2500),Es,Fs(50),Xx(50,50),Ff(50
     &,50),Convf,Fmaxt,Dxmaxt,Rmax,Rmin,Rlim,Eigmax,Eigmin,Eiglim,Fswtch
     &,Fncerr,Grderr,Fnccnv,Ic(50),Nstep,Istep,Nmax,Np,Neg,Prnt,Exit,Ndu
     &m2
      common/io/In,Iout,Ipunch
      equivalence(Es,Esave(1)),(Nvar,Nc)
      data zero/0.D0/
      
      
      
      
      
      
      
      
99001 format(' UPDATE SECOND DERIVATIVES USING INFORMATION FROM ','POINT
     &S:',5I3,(/1x,55x,5I3))
      
      
      do 100 j=1,Np
      do 50 i=1,Nc
      A(i,j)=(Xx(i,j)-X(i))
50    continue
100   continue
      nn=Np
      call schmdt(A,is,Nc,nn,Rmax,Rmin,Rlim)
      do 300 ip=1,nn
      ii=is(ip)
      ij=0
      dx=zero
      rx=zero
      
      do 150 i=1,Nc
      ddx=(Xx(i,ii)-X(i))
      dx=dx+ddx**2
      rx=rx+ddx*A(i,ip)
      fcold(i)=zero
      fcnew(i)=F(i)-Ff(i,ii)
      do 120 j=1,Nc
      ij=ij+1
      fcold(i)=fcold(i)+Fc(ij)*(Xx(j,ii)-X(j))
120   continue
150   continue
      
      dx=gsqrt(dx)
      do 200 jp=ip,nn
      deltak=zero
      do 160 i=1,Nc
      deltak=deltak+(fcnew(i)-fcold(i))*A(i,jp)
160   continue
      deltak=deltak/rx
      
      
      error=Grderr*dx/rx
      if(gabs(deltak).GE.error)then
      ij=0
      do 170 i=1,Nc
      do 165 j=1,Nc
      ij=ij+1
      if(ip.NE.jp)Fc(ij)=Fc(ij)+A(i,jp)*deltak*A(j,ip)
      Fc(ij)=Fc(ij)+A(i,ip)*deltak*A(j,jp)
165   continue
170   continue
      endif
200   continue
300   continue
      
      
      do 400 ii=1,nn
      ip=is(ii)
      Ic(ii)=Ic(ip)
      Esave(ii)=Esave(ip)
      do 350 i=1,Nc
      Xx(i,ii)=Xx(i,ip)
      Ff(i,ii)=Ff(i,ip)
350   continue
400   continue
      Np=nn
      write(Iout,99001)(Ic(nn-i+1),i=1,nn),Istep
      return
      
      end
C* :1 * 
      
