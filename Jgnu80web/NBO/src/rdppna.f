
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rdppna"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rdppna.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "rdppna.web"
      subroutine rdppna(T,OCC,IFLG)
      implicit none
      integer IFLG,Ispin,j,job,Label,Larc,Lbl,lfn,Lfnao,Lfnarc,Lfndaf,Lf
     &ndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm
      integer Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Lorb,Lorbc,Lstemt
     &,Lstocc,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Naoctr,Naol,Natoms,Nb
     &as
      integer Ndim
      double precision OCC,T
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      dimension T(Ndim,Ndim),OCC(Ndim)
      dimension job(20)
      logical error
      
      
      
      lfn=abs(IFLG/1000)
      write(Lfnpr,99001)
      
      if(Ispin.GE.0)rewind(lfn)
      call aread(T,Ndim,Nbas,Nbas,job,lfn,error)
      if(error)then
      
      write(Lfnpr,99004)lfn
      stop
      else
      if(Ispin.GE.0)write(Lfnpr,99002)job
      if(Ispin.LT.0)write(Lfnpr,99003)
      
      
      read(lfn,99007,end=100)(Naoctr(j),j=1,Nbas)
      read(lfn,99007,end=100)(Naol(j),j=1,Nbas)
      read(lfn,99007,end=100)(Lstocc(j),j=1,Nbas)
      
      
      read(lfn,99008,end=200)(OCC(j),j=1,Nbas)
      return
      endif
      
100   write(Lfnpr,99005)lfn
      stop
      
200   write(Lfnpr,99006)lfn
      stop
      
99001 format(/1x,'PNAO basis set from a previous calculation used:')
99002 format(1x,20A4)
99003 format(/1x,'See alpha NBO output for title of the transformation')
99004 format(/1x,'Error reading PAO to PNAO transformation from LFN',i3)
99005 format(/1x,'Error reading PNAO orbital labels from LFN',i3)
99006 format(/1x,'Error reading PNAO orbital occupancies from LFN',i3)
99007 format(1x,20I4)
99008 format(1x,5F15.9)
      end
C* :1 * 
      
