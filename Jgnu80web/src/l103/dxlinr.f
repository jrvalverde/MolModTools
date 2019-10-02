
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dxlinr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dxlinr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "dxlinr.web"
      subroutine dxlinr(F,XNEW,DXRMS,DXMAX,NVAR,OK)
      implicit none
      double precision a1,a2,a3,a4,Convf,cutoff,ddx,dx,DXMAX,Dxmaxt,DXRM
     &S,Eiglim,Eigmax,Eigmin,Es,F,Fc,Ff,Fmaxt,fmid
      double precision Fnccnv,Fncerr,fnew,fold,four,Fs,Fswtch,g,gabs,Grd
     &err,gsqrt,half,p,Rlim,Rmax,Rmin,root,rx,s,six
      double precision ssqm4p,three,twlv,two,x,Xc,xkcube,Xname,XNEW,xtem
     &p,Xx,zero
      integer i,Ic,ij,In,Iout,Ipunch,Istep,Ndum2,Neg,Nmax,Np,Nstep,NVAR
      logical OK
      logical Prnt,Exit
      dimension XNEW(50),F(50)
      common/optgrd/Xc(100),Xname(100),Fc(2500),Es,Fs(50),Xx(50,50),Ff(5
     &0,50),Convf,Fmaxt,Dxmaxt,Rmax,Rmin,Rlim,Eigmax,Eigmin,Eiglim,Fswtc
     &h,Fncerr,Grderr,Fnccnv,Ic(50),Nstep,Istep,Nmax,Np,Neg,Prnt,Exit,Nd
     &um2
      common/io/In,Iout,Ipunch
      data zero,two,four,six,twlv/0.D0,2.D0,4.D0,6.D0,12.D0/
      data three,half/3.D0,0.5D0/
      data cutoff/1.D-10/
      
      
      
      
      
      
      
99001 format(' LINEAR SEARCH NOT ATTEMPTED, NEGATIVE SECOND',' DERIVATIV
     &E IN SEARCH DIRECTION')
      
      
      OK=.TRUE.
      ij=0
      fold=zero
      fnew=zero
      xkcube=(Fs(1)-Es)*Fnccnv
      
      
      do 100 i=1,NVAR
      dx=Xx(i,2)-Xx(i,1)
      fold=fold+Ff(i,2)*dx
      fnew=fnew+Ff(i,1)*dx
100   continue
      fmid=(fold+fnew)/two
      a1=-(three*xkcube+fmid)/two
      a3=twlv*(xkcube+fmid)
      s=fnew-fold
      
      
      if(s.GE.zero)then
      
      
      
      p=a3**2/(four*twlv)
      ssqm4p=s**2-four*p
      
      
      if(ssqm4p.LT.zero)then
      
      x=-a1/s
      else
      root=gsqrt(ssqm4p)
      a2=(s+root)/two
      a4=(s-root)*twlv
      
      
      dx=-a1/(twlv*a2)
      x=dx
120   g=a1+a2*x+a3*x**2/two+a4*x**3/six
      if(g.GT.zero)then
      
      x=x-dx
      dx=dx/twlv
      if(gabs(dx).GE.cutoff)goto 120
      else
      x=x+dx
      goto 120
      endif
      endif
      else
      write(Iout,99001)
      OK=.FALSE.
      return
      endif
      xtemp=x-half
      DXRMS=zero
      DXMAX=zero
      fnew=zero
      rx=zero
      do 200 i=1,NVAR
      ddx=(Xx(i,2)-Xx(i,1))
      dx=xtemp*ddx
      rx=rx+ddx**2
      XNEW(i)=XNEW(i)-dx
      DXRMS=DXRMS+dx**2
      if(gabs(dx).GT.DXMAX)DXMAX=gabs(dx)
      F(i)=F(i)-xtemp*(Ff(i,2)-Ff(i,1))
      fnew=fnew+F(i)*ddx
200   continue
      fnew=fnew/rx
      do 300 i=1,NVAR
      F(i)=F(i)-fnew*(Xx(i,2)-Xx(i,1))
300   continue
      DXRMS=gsqrt(DXRMS/NVAR)
      return
      
      end
C* :1 * 
      
