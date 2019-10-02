
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 wrtnbo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:06."
C  WEB FILE:     "wrtnbo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "wrtnbo.web"
      subroutine wrtnbo(T,BNDOCC,IFLG)
      implicit none
      double precision BNDOCC,T
      integer Iatcr,Iatno,Ibxm,IFLG,Ino,Ispin,Iznuc,j,Label,Larc,Lbl,lfn
     &,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab
      integer Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lf
     &nppa,Lfnpr,Ll,Lorb,Lorbc,Lstocc,Lu,MAXATM,MAXBAS,Munit,Mxao,Mxaolm
      integer Mxbo,Natoms,Nbas,Nbotyp,Nbouni,Ndim,Norbs
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      dimension T(Ndim,Ndim),BNDOCC(1)
      character*80 title
      
      
      title='NBOs in the AO basis:'
      call aout(T,Ndim,Nbas,Nbas,title,1,IFLG)
      
      
      lfn=abs(IFLG)
      write(lfn,99001)(BNDOCC(j),j=1,Nbas)
      
      
      write(lfn,99002)(Nbouni(j),j=1,Nbas)
      write(lfn,99002)(Nbotyp(j),j=1,Nbas)
      write(lfn,99003)(Label(j,1),j=1,Nbas)
      write(lfn,99003)(Label(j,2),j=1,Nbas)
      write(lfn,99002)(Label(j,3),j=1,Nbas)
      write(lfn,99002)(Label(j,4),j=1,Nbas)
      write(lfn,99002)(Label(j,5),j=1,Nbas)
      write(lfn,99002)(Label(j,6),j=1,Nbas)
      write(lfn,99002)(Ibxm(j),j=1,Nbas)
      write(lfn,99002)(Iatno(j),j=1,Natoms)
      return
      
99001 format(1x,5F15.9)
99002 format(1x,20I3)
99003 format(1x,20A3)
      end
C* :1 * 
      
