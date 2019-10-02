
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 naosim"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "naosim.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "naosim.web"
      subroutine naosim(DM,T,A)
      implicit none
      double precision A,DM,one,T,zero
      integer i,i1,i2,i3,i4,i5,Ichoos,io,ioinqr,Iprint,iprnt,Ipseud,Ispi
     &n,it,Iw3c,Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfock
      integer Iwhybs,Iwmulp,Iwpnao,iwrit,Iwtnab,Iwtnao,Iwtnbo,ix,j,Jcore
     &,Jprint,Kopt,Label,Lang,Larc,Lbl,Lctr,Lfnao,Lfnarc,Lfndaf
      integer Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnl
     &m,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Lorb,Lorbc,Lstemt,Lstoc
     &c,MAXATM
      integer MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Naoctr,Naol,Natoms,Nbas,Ndim
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      common/nbao/Lctr(MAXBAS),Lang(MAXBAS)
      
      dimension DM(Ndim,Ndim),T(Ndim,Ndim),A(1)
      character*80 title
      
      data zero,one/0.0D0,1.0D0/
      data iprnt,iwrit/4HPRNT,4HWRIT/
      
      
      
      call lblao
      
      
      do 100 i=1,Nbas
      Lbl(i)=Lctr(i)
      Lorbc(i)=Lang(i)
100   continue
      
      
      if(Jprint(22).GT.0)call wrbas(A,A,Jprint(22))
      
      
      if(Jprint(7).NE.0)call wrarc(A,A,Jprint(7))
      
      
      io=ioinqr(Jprint(26))
      if(.NOT.Open.AND.(io.EQ.iprnt.OR.io.EQ.iwrit))then
      call feaomo(A,it)
      if(it.NE.0)then
      title='MOs in the AO basis:'
      call aout(A,Ndim,Nbas,Nbas,title,1,Jprint(26))
      endif
      endif
      
      
      io=ioinqr(Jprint(40))
      if(.NOT.Open.AND.(io.EQ.iprnt.OR.io.EQ.iwrit))then
      call fefao(A,Iwfock)
      if(Iwfock.NE.0)then
      title='AO Fock matrix:'
      call aout(A,Ndim,-Nbas,Nbas,title,1,Jprint(40))
      endif
      endif
      
      
      io=ioinqr(Jprint(27))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      title='Spinless AO density matrix:'
      call aout(DM,Ndim,-Nbas,Nbas,title,1,Jprint(27))
      endif
      
      
      io=ioinqr(Jprint(50))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      ix=1
      call fedxyz(A,ix)
      if(ix.NE.0)then
      title='AO x dipole integrals:'
      call aout(A,Ndim,-Nbas,Nbas,title,1,Jprint(50))
      endif
      ix=2
      call fedxyz(A,ix)
      if(ix.NE.0)then
      title='AO y dipole integrals:'
      call aout(A,Ndim,-Nbas,Nbas,title,1,Jprint(50))
      endif
      ix=3
      call fedxyz(A,ix)
      if(ix.NE.0)then
      title='AO z dipole integrals:'
      call aout(A,Ndim,-Nbas,Nbas,title,1,Jprint(50))
      endif
      endif
      
      
      do 200 j=1,Nbas
      do 150 i=1,Nbas
      T(i,j)=zero
150   continue
      T(j,j)=one
200   continue
      
      
      call svtnao(T)
      
      
      do 300 i=1,Nbas
      Naoctr(i)=Lctr(i)
      Naol(i)=Lang(i)
      Lstocc(i)=1
300   continue
      
      
      i1=1
      i2=i1+Natoms*Natoms
      i3=i2+Natoms
      i4=i3+Natoms*Natoms
      i5=i4+Ndim*Ndim
      call naoanl(DM,T,A(i1),A(i2),A(i3),A(i4),A(i5))
      
      
      
      call svdnao(DM)
      
      
      call lblnao
      
      
      if(Open)return
      
      
      io=ioinqr(Jprint(9))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      i1=1
      i2=i1+Natoms*Natoms
      i3=i2+Ndim*Ndim
      i4=i3+Ndim*Ndim
      call frmtmo(T,A(i2),A(i3),A(i4),2,Jprint(9))
      endif
      
      
      i1=1
      i2=i1+Natoms*Natoms
      i3=i2+Ndim*Ndim
      
      
      io=ioinqr(Jprint(31))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call fefao(A(i2),Iwfock)
      if(Iwfock.NE.0)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      title='NAO Fock matrix:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,2,Jprint(31))
      endif
      endif
      
      
      io=ioinqr(Jprint(35))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      title='NAO density matrix:'
      call aout(DM,Ndim,-Nbas,Nbas,title,2,Jprint(35))
      endif
      return
      end
C* :1 * 
      
