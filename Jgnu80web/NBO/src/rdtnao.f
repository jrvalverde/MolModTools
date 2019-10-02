
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rdtnao"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rdtnao.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "rdtnao.web"
      subroutine rdtnao(DM,T,SCR,IFLG)
      implicit none
      double precision DM,SCR,T
      integer IFLG,Ispin,j,job,Label,Larc,Lbl,lfn,Lfnao,Lfnarc,Lfndaf,Lf
     &ndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm
      integer Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Lorb,Lorbc,Lstemt
     &,Lstocc,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Naoctr,Naol,Natoms,Nb
     &as
      integer Ndim
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      dimension T(Ndim,Ndim),DM(Ndim,Ndim),SCR(Ndim)
      dimension job(20)
      logical error
      
      
      
      lfn=abs(IFLG/1000)
      write(Lfnpr,99001)
      
      rewind(lfn)
      call aread(T,Ndim,Nbas,Nbas,job,lfn,error)
      if(error)then
      
      write(Lfnpr,99003)lfn
      stop
      else
      write(Lfnpr,99002)job
      call svtnao(T)
      
      
      call simtrs(DM,T,SCR,Ndim,Nbas)
      
      
      read(lfn,99006,end=100)(Naoctr(j),j=1,Nbas)
      read(lfn,99006,end=100)(Naol(j),j=1,Nbas)
      read(lfn,99006,end=100)(Lstocc(j),j=1,Nbas)
      
      
      call aread(T,Ndim,-Nbas,Nbas,job,lfn,error)
      if(error)then
      
      write(Lfnpr,99005)lfn
      stop
      
99001 format(/1x,'NAO basis set from a previous calculation used:')
99002 format(1x,20A4)
99003 format(/1x,'Error reading AO to NAO transformation from LFN',i3)
99004 format(/1x,'Error reading NAO orbital labels from LFN',i3)
99005 format(/1x,'Error reading PNAO overlap matrix from LFN',i3)
99006 format(1x,20I4)
      else
      call svsnao(T)
      return
      endif
      endif
      
100   write(Lfnpr,99004)lfn
      stop
      end
C* :1 * 
      
