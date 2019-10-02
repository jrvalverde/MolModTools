
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 delscf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "delscf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "delscf.web"
      subroutine delscf(CORE,ICORE,NBOOPT)
      implicit none
      double precision CORE
      integer ICORE,Ispin,l2,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnm
     &o,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,L
     &fnppa
      integer Lfnpr,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,NBOOPT,Ndim
      dimension CORE(*),ICORE(*),NBOOPT(10)
      logical new,error
      
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      
      Lfndaf=48
      
      
      if(NBOOPT(1).EQ.2)then
      new=.FALSE.
      call nbopen(new,error)
      if(error)then
      write(Lfnpr,99001)
      stop
      endif
      l2=Ndim*(Ndim+1)/2
      if(Open)then
      Alpha=.TRUE.
      Beta=.FALSE.
      call fenewd(CORE)
      call twrite(528,CORE,l2,1,l2,1,0)
      Alpha=.FALSE.
      Beta=.TRUE.
      call fenewd(CORE)
      call twrite(530,CORE,l2,1,l2,1,0)
      else
      Alpha=.FALSE.
      Beta=.FALSE.
      call fenewd(CORE)
      call twrite(528,CORE,l2,1,l2,1,0)
      endif
      call nbclos
      endif
      
      
      if(NBOOPT(1).EQ.3)then
      new=.FALSE.
      call nbopen(new,error)
      if(error)then
      write(Lfnpr,99001)
      stop
      endif
      call tread(501,CORE,32,1,32,1,0)
      call sve0(CORE(32))
      call nbclos
      endif
      return
      
99001 format(/1x,'Error opening the NBO direct access file in ','subrout
     &ine DELSCF.')
      end
C* :1 * 
      
