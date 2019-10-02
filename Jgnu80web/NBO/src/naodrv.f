
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 naodrv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "naodrv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "naodrv.web"
      
      
      
      subroutine naodrv(DM,T,A)
      implicit none
      double precision A,Accthr,Athr,Crtset,DM,Dthr,E2thr,Ethr,one,Prjse
     &t,Pthr,T,Thrset
      integer i,i1,i2,i3,i4,i5,i6,i7,i8,i9,Iatcr,Iatno,Ichoos,ictran,idt
     &ran,Ino,io,ioinqr,Iprint,iprnt
      integer Ipseud,iread,Ispin,it,Iw3c,Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfoc
     &k,Iwhybs,Iwmulp,Iwpnao,iwrit,Iwtnab,Iwtnao,Iwtnbo,ix,Iznuc,Jcore
      integer Jprint,Kopt,Label,Lang,Larc,Lbl,Lctr,Lfnao,Lfnarc,Lfndaf,L
     &fndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna
      integer Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Ll,Lorb,Lorbc,Lstemt,Lst
     &occ,Lu,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Naoctr,Naol,Natoms
      integer Nbas,nblock,Ndim,Norbs
      
      
      
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      common/nbthr/Thrset,Prjset,Accthr,Crtset,E2thr,Athr,Pthr,Ethr,Dthr
      common/nbao/Lctr(MAXBAS),Lang(MAXBAS)
      
      dimension T(Ndim,Ndim),DM(Ndim,Ndim),A(1)
      character*80 title
      
      data one/1.0D0/
      data iprnt,iwrit,iread/4HPRNT,4HWRIT,4HREAD/
      
      
      call lblao
      
      
      do 100 i=1,Nbas
      Lbl(i)=Lctr(i)
      Lorbc(i)=Lang(i)
100   continue
      
      
      if(Jprint(22).GT.0)call wrbas(A,A,Jprint(22))
      
      
      if(Jprint(7).NE.0)call wrarc(A,A,Jprint(7))
      
      
      io=ioinqr(Jprint(39))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      title='AO overlap matrix:'
      call aout(T,Ndim,-Nbas,Nbas,title,1,Jprint(39))
      endif
      
      
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
      
      
      io=ioinqr(Jprint(42))
      if(Iwdm.EQ.1.AND.(io.EQ.iprnt.OR.io.EQ.iwrit))then
      title='Spinless AO bond-order matrix:'
      call aout(DM,Ndim,-Nbas,Nbas,title,1,Jprint(42))
      endif
      
      
      if(Iwdm.NE.0)call simtrm(DM,T,A,Ndim,Nbas,Iwmulp,Iwcubf)
      
      
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
      
      
      
      nblock=Mxaolm*Mxaolm
      i1=1
      i2=i1+Ndim
      i3=i2+Ndim
      i4=i3+Ndim*Ndim
      i5=i4+nblock
      i6=i5+Ndim
      i7=i6+nblock
      i8=i7+nblock
      i9=i8+Ndim
      
      
      if(ioinqr(Iwtnao).EQ.iread)then
      call rdtnao(DM,T,A(i1),Iwtnao)
      goto 300
      endif
      
      
      call dfgorb(A(i2),DM,T,ictran,Iwcubf,0,Lfnpr)
      
      
      call svppao(DM)
      
      
      call consol(DM,T,Ndim,Nbas)
      
      
      call nao(T,DM,A(i1),A(i3),A(i4),A(i5),A(i6),A(i7),A(i8),A(i9),nblo
     &ck)
      
      
      if(ictran.NE.0)call dfgorb(A(i2),DM,T,idtran,Iwcubf,1,Lfnpr)
      
      
      call svtnao(T)
      
      
      call fepnao(A(i3))
      
      
      do 200 i=0,Nbas-1
      A(i4+i)=-one
200   continue
      
      if(ictran.NE.0)call dfgorb(A(i2),DM,A(i3),idtran,Iwcubf,1,Lfnpr)
      
      
      call fesraw(T)
      call simtrs(T,A(i3),A(i4),Ndim,Nbas)
      call svsnao(T)
      
      
      if(ioinqr(Iwtnao).EQ.iwrit)call wrtnao(T,Iwtnao)
      
      
300   i1=1
      i2=i1+Natoms*Natoms
      i3=i2+Natoms
      i4=i3+Natoms*Natoms
      i5=i4+Ndim*Ndim
      call naoanl(DM,T,A(i1),A(i2),A(i3),A(i4),A(i5))
      
      
      
      call svdnao(DM)
      
      
      call lblnao
      
      
      i1=1
      i2=i1+Natoms*Natoms
      
      
      io=ioinqr(Jprint(44))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call fepnao(T)
      title='PNAOs in the AO basis:'
      call aout(T,Ndim,Nbas,Nbas,title,1,Jprint(44))
      endif
      
      
      io=ioinqr(Jprint(19))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call fesnao(A(i2))
      title='PNAO overlap matrix:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,2,Jprint(19))
      endif
      
      
      call fetnao(T)
      
      
      if(ioinqr(Iwtnao).EQ.iprnt)then
      title='NAOs in the AO basis:'
      call aout(T,Ndim,Nbas,Nbas,title,1,Iwtnao)
      endif
      
      
      io=ioinqr(Jprint(51))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      ix=1
      call fedxyz(A(i2),ix)
      if(ix.NE.0)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      title='NAO x dipole integrals:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,2,Jprint(51))
      endif
      ix=2
      call fedxyz(A(i2),ix)
      if(ix.NE.0)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      title='NAO y dipole integrals:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,2,Jprint(51))
      endif
      ix=3
      call fedxyz(A(i2),ix)
      if(ix.NE.0)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      title='NAO z dipole integrals:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,2,Jprint(51))
      endif
      endif
      
      
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
      
