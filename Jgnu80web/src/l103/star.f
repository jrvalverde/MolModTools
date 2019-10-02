
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 star"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "star.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 16 "star.web"
      subroutine star
      implicit none
      double precision Convf,Dxmaxt,Eiglim,Eigmax,Eigmin,Energy,Es,F,Fc,
     &Ff,Fmaxt,Fnccnv,Fncerr,frc,Frcnst,Fs,Fswtch,gabs,Grderr,gsign
      integer i,Ic,icalce,icalcf,icalff,ii,ij,ijs,In,Iout,Ipunch,Istep,j
     &,ji,jj,Ndum,Ndum2,Neg,Nmax,nn
      integer Np,Nstep,Nvar
      double precision Rlim,Rmax,Rmin,Stardl,two,X,Xname,Xx
      logical Prnt,Exit
      dimension Stardl(50)
      common/grdnt/Energy,F(50),Frcnst(1275),Nvar,Ndum
      common/optgrd/X(100),Xname(100),Fc(2500),Es,Fs(50),Xx(50,50),Ff(50
     &,50),Convf,Fmaxt,Dxmaxt,Rmax,Rmin,Rlim,Eigmax,Eigmin,Eiglim,Fswtch
     &,Fncerr,Grderr,Fnccnv,Ic(50),Nstep,Istep,Nmax,Np,Neg,Prnt,Exit,Ndu
     &m2
      common/io/In,Iout,Ipunch
      equivalence(Stardl(1),Xx(1,2))
      data two/2.D0/
      data icalce/2/,icalcf/3/,icalff/-1/
      
      
      
      
      
      
      
      
      
      if(Istep.NE.0)then
      
      
      Stardl(Istep)=X(Istep)-Xx(Istep,1)
      X(Istep)=Xx(Istep,1)
      call ilsw(1,24,0)
      jj=Istep+(Istep-1)*Nvar
      if(Ic(Istep).EQ.icalcf)then
      ij=(Istep-1)*Nvar
      do 20 i=1,Nvar
      ij=ij+1
      Fc(ij)=-(F(i)-Fs(i))/Stardl(Istep)
20    continue
      endif
      if(Ic(Istep).EQ.icalce)Fc(jj)=two*((Energy-Es)*Fnccnv/Stardl(Istep
     &)+Fs(Istep))/Stardl(Istep)
      else
      Es=Energy
      do 50 i=1,Nvar
      Fs(i)=F(i)
      Xx(i,1)=X(i)
      ii=i+(i-1)*Nvar
      Stardl(i)=gsign(Fc(ii),F(i))
50    continue
      call estm
      nn=Nvar*(Nvar+1)/2
      ijs=0
      do 100 i=1,Nvar
      do 60 j=1,i
      ijs=ijs+1
      if((Ic(i).EQ.icalff).AND.(Ic(j).EQ.icalff))then
      ij=Nvar*(j-1)+i
      ji=Nvar*(i-1)+j
      Fc(ij)=Frcnst(ijs)
      Fc(ji)=Frcnst(ijs)
      endif
60    continue
100   continue
      endif
      
      
200   Istep=Istep+1
      if(Istep.LE.Nvar)then
      if((Ic(Istep).NE.icalce).AND.(Ic(Istep).NE.icalcf))goto 200
      X(Istep)=X(Istep)+Stardl(Istep)
      if(Ic(Istep).EQ.icalce)call ilsw(1,24,1)
      return
      endif
      
      do 300 i=1,Nvar
      do 250 j=i,Nvar
      ij=i+(j-1)*Nvar
      ji=j+(i-1)*Nvar
      frc=Fc(ij)
      if(gabs(Fc(ji)).GT.dabs(frc))frc=Fc(ji)
      Fc(ij)=frc
      Fc(ji)=frc
250   continue
300   continue
      Energy=Es
      do 400 i=1,Nvar
      F(i)=Fs(i)
400   continue
      Istep=0
      Np=0
      Exit=.TRUE.
      return
      
      end
C* :1 * 
      
