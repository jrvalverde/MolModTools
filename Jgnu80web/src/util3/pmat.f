
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 pmat"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "pmat.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "pmat.web"
      subroutine pmat(NBASIS,A,B)
      implicit none
      double precision A,a0,ai,amu,anu,ar,B,Big,bmu,bnu,Four,One,Onept5,
     &Pt5,Small,Three,Two,Zero
      integer i,imdim,imtt,In,Iout,Ipunch,k,Ksm,Kspin,Ksw,Mdim,Mdsq,Mshi
     &fs,Mtt,mu,NBASIS,Nesk,Nest,Nest1,Nse
      integer Nsep,Ntt,nu
      logical Cmp,Rhf
      dimension A(Mdim,*),B(*)
      common/const/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/maxdm/Mdim,Mtt,Ntt,Mdsq,Mshifs
      common/io/In,Iout,Ipunch
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      
      
      k=(Kspin-1)*Mshifs
      if(.NOT.(Cmp))then
      
      do 50 mu=1,NBASIS
      do 20 nu=1,mu
      a0=Zero
      do 10 i=1,Nse
      a0=a0+A(mu,i)*A(nu,i)
10    continue
      k=k+1
      B(k)=a0
20    continue
50    continue
      if(.NOT.Rhf)return
      do 100 i=1,Ntt
      B(i)=Two*B(i)
100   continue
      return
      endif
      
      do 200 nu=1,NBASIS
      do 150 mu=1,nu
      ar=Zero
      ai=Zero
      do 120 i=1,Nse
      imdim=i+Mdim
      amu=A(mu,i)
      anu=A(nu,i)
      bmu=A(mu,imdim)
      bnu=A(nu,imdim)
      ar=ar+amu*anu+bmu*bnu
      ai=ai+amu*bnu-bmu*anu
120   continue
      k=k+1
      B(k)=ar
      B(k+Mtt)=ai
150   continue
200   continue
      
      if(.NOT.Rhf)return
      do 300 i=1,Ntt
      imtt=i+Mtt
      B(i)=B(i)*Two
      B(imtt)=B(imtt)*Two
300   continue
      return
      
      end
C* :1 * 
      
