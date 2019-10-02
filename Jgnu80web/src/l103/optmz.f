
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 optmz"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "optmz.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 85 "optmz.web"
      subroutine optmz(JUMP)
      implicit none
      double precision Alpha,Beta,Bl,Convf,cputm,Dxmaxt,Eiglim,Eigmax,Ei
     &gmin,Energy,Es,F,Fc,Ff,Fmaxt,Fnccnv,Fncerr,Frcnst,Fs,Fswtch
      double precision Grderr,Pcon,Rlim,Rmax,Rmin,Toang,X,Xname,Xx
      integer i,Ianz,Ic,icnv,igrd,igrdnt,ij,ijs,In,Iop,ioptgr,Iout,iozma
     &t,Ipunch,Istep,Iz,j,JUMP,Lalpha,Lbeta
      integer Lbl,lgrdnt,loptgr,lzmat,Ndum,Ndum2,Neg,nel,Nmax,Np,Nstep,N
     &var,Nvarrd,Nz
      logical Prnt,Exit
      save
      common/iop/Iop(50)
      common/io/In,Iout,Ipunch
      common/phycon/Toang,Pcon(29)
      common/grdnt/Energy,F(50),Frcnst(1275),Nvar,Ndum
      common/optgrd/X(100),Xname(100),Fc(2500),Es,Fs(50),Xx(50,50),Ff(50
     &,50),Convf,Fmaxt,Dxmaxt,Rmax,Rmin,Rlim,Eigmax,Eigmin,Eiglim,Fswtch
     &,Fncerr,Grderr,Fnccnv,Ic(50),Nstep,Istep,Nmax,Np,Neg,Prnt,Exit,Ndu
     &m2
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alpha(50),Beta(50),Lbl(50),La
     &lpha(50),Lbeta(50),Nz,Nvarrd
      equivalence(cputm,icnv)
      data igrdnt,lgrdnt,ioptgr,loptgr,iozmat,lzmat/511,1327,575,7793,50
     &7,351/
      
      
      
99001 format(1x,18(4HGRAD))
99002 format(' BERNY OPTIMIZATION')
99003 format(' INITIALIZATION PASS')
99004 format(' NUMERICALLY ESTIMATING SECOND DERIVATIVES')
99005 format(' NUMERICAL SECOND DERIVATIVE COMPUTATION COMPLETE')
      
      
      
      call drum
      write(Iout,99001)
      write(Iout,99002)
      Exit=.FALSE.
      call ilsw(2,23,igrd)
      call ilsw(1,23,1)
      if(igrd.EQ.1)then
      
      
      call tread(iozmat,Ianz,lzmat,1,lzmat,1,0)
      call tread(igrdnt,Energy,lgrdnt,1,lgrdnt,1,0)
      call tread(ioptgr,X,loptgr,1,loptgr,1,0)
      
      
      
      if(Np.LT.0)then
      call star
      if(.NOT.Exit)write(Iout,99004)
      if(.NOT.Exit)goto 100
      endif
      
      
      call grdopt(Iop,Toang)
      
      
      if(Iop(18).NE.0)then
      call ilsw(1,25,0)
      write(Iout,99005)
      Exit=.TRUE.
      endif
      else
      
      
      
      write(Iout,99003)
      call tread(iozmat,Ianz,lzmat,1,lzmat,1,0)
      call initbs(Iop,Toang)
      call twrite(ioptgr,X,loptgr,1,loptgr,1,0)
      call twrite(igrdnt,Energy,lgrdnt,1,lgrdnt,1,0)
      write(Iout,99001)
      JUMP=0
      return
      endif
      
      
100   if(Iop(32).EQ.1)then
      if(Exit.OR.Iop(30).NE.0)then
      nel=200
      call binwt(Xname,nel,16HVARIABLE NAMES  ,0)
      
      
      if(Iop(30).EQ.0)then
      ijs=0
      do 110 i=1,Nvar
      do 105 j=1,i
      ijs=ijs+1
      ij=Nvar*(j-1)+i
      Frcnst(ijs)=Fc(ij)
105   continue
110   continue
      endif
      nel=Nvar*(Nvar+1)
      call binwt(Frcnst,nel,16HFORCE CONSTANTS ,0)
      endif
      endif
      
      
      write(Iout,99001)
      JUMP=1
      if(Exit.OR.Iop(30).NE.0)return
      call udbs(X)
      call twrite(ioptgr,X,loptgr,1,loptgr,1,0)
      call twrite(igrdnt,Energy,lgrdnt,1,lgrdnt,1,0)
      JUMP=0
      return
      
      end
C* :1 * 
      
