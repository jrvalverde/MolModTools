
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 frmtmo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "frmtmo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "frmtmo.web"
      subroutine frmtmo(T,TMO,C,SCR,INDEX,IFLG)
      implicit none
      double precision C,eps,SCR,T,tmax,TMO,zero,zertol
      integer ierr,IFLG,INDEX,Iprin,Ispin,it,jrow,kcol,lfn0,Lfnao,Lfnarc
     &,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho
      integer Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Ltyp,MAXAT
     &M,MAXBAS,maxit,Munit,Mxao,Mxaolm,Mxbo,Naoa,Naoc,Natoms,Nbas,Ndim
      character*80 title
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbnao/Naoc(MAXBAS),Naoa(MAXBAS),Ltyp(MAXBAS),Iprin(MAXBAS)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      dimension T(Ndim,Ndim),TMO(Ndim,Ndim),C(Ndim,Ndim),SCR(Ndim*(Ndim+
     &5))
      character*4 basis
      dimension basis(4)
      
      data basis/' NAO',' NHO',' NBO','NLMO'/
      data zero/0.0D0/
      
      
      
      call feaomo(C,it)
      if(it.EQ.0)return
      
      
      eps=1.0E-8
      maxit=10
      lfn0=0
      call lineq(T,TMO,C,SCR,Nbas,Nbas,Ndim,Ndim,zertol,eps,maxit,lfn0,i
     &err)
      if(ierr.NE.0)then
      write(Lfnpr,99001)basis(INDEX-1)
      if(ierr.EQ.1)write(Lfnpr,99002)basis(INDEX-1)
      stop
      endif
      
      
      do 100 kcol=1,Nbas
      tmax=zero
      do 50 jrow=1,Nbas
      if(abs(TMO(jrow,kcol)).GT.abs(tmax))tmax=TMO(jrow,kcol)
50    continue
      if(tmax.LT.zero)then
      do 60 jrow=1,Nbas
      TMO(jrow,kcol)=-TMO(jrow,kcol)
60    continue
      endif
100   continue
      
      
      if(INDEX.EQ.2)title='MOs in the NAO basis:'
      if(INDEX.EQ.3)title='MOs in the NHO basis:'
      if(INDEX.EQ.4)title='MOs in the NBO basis:'
      if(INDEX.EQ.5)title='MOs in the NLMO basis:'
      call aout(TMO,Ndim,Nbas,Nbas,title,INDEX,IFLG)
      return
      
99001 format(/1x,'Error calculating the ',a4,' to MO transformation')
99002 format(1x,'The AO to ',a4,' transformation is not invertible')
      end
C* :1 * 
      
