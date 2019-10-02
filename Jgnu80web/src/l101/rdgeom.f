
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rdgeom"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rdgeom.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 386 "rdgeom.web"
      subroutine rdgeom(JUMP)
      implicit none
      double precision Alpha,Anames,Atmchg,Beta,Bl,C,Fpvec,Phycon,Toang,
     &Values
      integer i,Ian,Ianz,Icharg,Intvec,Iop,iozmat,iozsub,Iz,j,JUMP,Lalph
     &a,Lbeta,Lbl,Multip,Nae,Natoms,Nbasis,Nbe,Ne
      integer Nvar,Nz,ii
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alpha(50),Beta(50),Lbl(50),La
     &lpha(50),Lbeta(50),Nz,Nvar
      common/zsubst/Anames(50),Values(50),Intvec(50),Fpvec(50)
      common/phycon/Toang,Phycon(29)
      
      data iozmat/507/,iozsub/570/
      
      
      call drum
      call rdtitl
      if(Iop(29).EQ.0)then
      
      call zget(Iop,Icharg,Multip,Toang)
      else
      call rcoord(Natoms,Multip,Icharg,Ian,C,Iop,Toang)
      nz=natoms
      do 1 ii=1,nz
1     ianz(ii)=ian(ii)
      endif
      call ilsw(1,18,1)
      call chgmlt(Icharg,Multip)
      call twrite(iozmat,Ianz,351,1,351,1,0)
      if(Nvar.NE.0)call twrite(iozsub,Anames,175,1,175,1,0)
      if(Iop(34).NE.0)write(6,99001)((Iz(i,j),j=1,3),i=1,3)
      
99001 format(' IZ=',3I10)
      
      JUMP=0
      return
      
      end
C* :1 * 
      
