
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rdtnab"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rdtnab.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "rdtnab.web"
      subroutine rdtnab(T,DM,BNDOCC,SCR,IFLG)
      implicit none
      double precision BNDOCC,DM,SCR,T
      integer i,Iathy,Ibxm,IFLG,Ispin,j,job,Label,Larc,lfn,Lfnao,Lfnarc,
     &Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo
      integer Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Lst
     &occ,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Naoctr,Naol,Natoms,Nbas,N
     &dim
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      dimension T(Ndim,Ndim),DM(Ndim,Ndim),BNDOCC(Ndim),SCR(Ndim)
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
      
      
      do 50 i=1,Nbas
      read(lfn,99006,end=200)(Label(i,j),j=1,6),Ibxm(i)
50    continue
      
      
      call simtrs(DM,T,SCR,Ndim,Nbas)
      do 100 i=1,Nbas
      BNDOCC(i)=DM(i,i)
100   continue
      return
      endif
      
200   write(Lfnpr,99005)lfn
      stop
      
99001 format(/1x,'NAO to NBO transformation from a previous ','calculati
     &on will be used:')
99002 format(1x,20A4)
99003 format(/1x,'See alpha NBO output for title of the transformation')
99004 format(/1x,'Error reading NAO to NBO transformation from LFN',i3)
99005 format(/1x,'Error reading NBO orbital labels from LFN',i3)
99006 format(1x,a2,a1,4I3,3x,i3)
      end
C* :1 * 
      
