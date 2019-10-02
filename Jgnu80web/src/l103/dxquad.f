
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dxquad"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dxquad.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "dxquad.web"
      subroutine dxquad(FTEMP,XNEW,VEC,DXRMS,DXMAX,OK)
      implicit none
      double precision A,Convf,ddx,dx,DXMAX,Dxmaxt,DXRMS,eigen,Eiglim,Ei
     &gmax,Eigmin,Energy,Es,F,Fc,Ff,Fmaxt,Fnccnv,Fncerr,Frcnst
      double precision Fs,Fswtch,FTEMP,gabs,Grderr,gsign,gsqrt,Rlim,Rmax
     &,Rmin,rx,VEC,X,Xname,XNEW,Xx,xxx,zero
      integer i,Ic,ii,ij,In,Iout,Ipunch,Istep,j,ji,jj,k,Nc,Ndum,Ndum2,Ne
     &g,Nmax,Np,Nstep,Nvar
      logical OK
      logical Prnt,Exit
      dimension VEC(50,50),xxx(50),A(50,50),eigen(50),XNEW(50)
      dimension FTEMP(50)
      common/grdnt/Energy,F(50),Frcnst(1275),Nvar,Ndum
      common/optgrd/X(100),Xname(100),Fc(2500),Es,Fs(50),Xx(50,50),Ff(50
     &,50),Convf,Fmaxt,Dxmaxt,Rmax,Rmin,Rlim,Eigmax,Eigmin,Eiglim,Fswtch
     &,Fncerr,Grderr,Fnccnv,Ic(50),Nstep,Istep,Nmax,Np,Neg,Prnt,Exit,Ndu
     &m2
      common/io/In,Iout,Ipunch
      equivalence(A(1,1),Fc(1))
      equivalence(Nvar,Nc)
      data zero/0.D0/
      
      
      
      
      
      
      
      
99001 format(' EIGENVECTORS OF THE SECOND DERIVATIVE MATRIX:')
99002 format(' EIGENVECTORS REQUIRED TO HAVE NEGATIVE EIGENVALUES:')
99003 format(' EIGENVALUE',i3,' OUT OF RANGE, NEW VALUE =',f12.6,', EIGE
     &NVECTOR =')
99004 format(4x,17H EIGENVALUES --- ,4F12.5)
      
      ij=Nc**2+1
      do 100 j=1,Nc
      do 50 i=1,Nc
      ii=Nc+1-i
      jj=Nc+1-j
      ij=ij-1
      A(ii,jj)=Fc(ij)
50    continue
100   continue
      call diag(Nc,50,A,VEC,eigen,xxx)
      if(Prnt)write(Iout,99001)
      if(Prnt)call matprt(VEC,50,50,Nc,Nc,2,0,Xname,Xname,0,eigen,1)
      if(.NOT.Prnt)write(Iout,99004)(eigen(k),k=1,Nc)
      if((.NOT.Prnt).AND.(Neg.NE.0))write(Iout,99002)
      if((.NOT.Prnt).AND.(Neg.NE.0))call matprt(VEC,50,50,Nc,Neg,1,0,Xna
     &me,Xname,0,eigen,0)
      
      
      do 200 i=1,Nc
      ii=i
      rx=eigen(i)
      if(gabs(eigen(i)).LT.Eigmin)eigen(i)=gsign(Eigmin,eigen(i))
      if(gabs(eigen(i)).GT.Eigmax)eigen(i)=gsign(Eigmax,eigen(i))
      if(((eigen(i).GT.zero).AND.(i.LE.Neg)).OR.((eigen(i).LT.zero).AND.
     &(i.GT.Neg)))eigen(i)=-eigen(i)
      if(eigen(i).NE.rx)then
      write(Iout,99003)i,eigen(i)
      call matprt(VEC,50,50,Nc,Nc,0,1,Xname,Xname,-i,0,0)
      endif
200   continue
      
      
      do 400 i=1,Nc
      do 250 j=i,Nc
      ij=i+(j-1)*Nc
      ji=j+(i-1)*Nc
      rx=zero
      ddx=zero
      do 220 k=1,Nc
      rx=rx+VEC(i,k)*VEC(j,k)/eigen(k)
      ddx=ddx+VEC(i,k)*VEC(j,k)*eigen(k)
220   continue
      Fc(ij)=ddx
      Fc(ji)=ddx
      xxx(j)=rx
250   continue
      do 300 j=1,Nc
      if(j.LT.i)VEC(i,j)=VEC(j,i)
      if(j.GE.i)VEC(i,j)=xxx(j)
300   continue
400   continue
      DXRMS=zero
      DXMAX=zero
      do 500 i=1,Nc
      dx=zero
      do 450 j=1,Nc
      dx=dx+VEC(j,i)*FTEMP(j)
450   continue
      XNEW(i)=XNEW(i)+dx
      DXRMS=DXRMS+dx**2
      if(gabs(dx).GT.DXMAX)DXMAX=gabs(dx)
500   continue
      DXRMS=gsqrt(DXRMS/Nc)
      return
      
      end
C* :1 * 
      
