
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nbodrv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nbodrv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "nbodrv.web"
      subroutine nbodrv(DM,T,A)
      
      
      
      
      implicit none
      double precision A,Accthr,Athr,Crtset,DM,Dthr,E2thr,Ethr,Prjset,Pt
     &hr,T,Thrset
      integer i0,i1,i10,i11,i12,i13,i14,i2,i3,i4,i5,i6,i7,i8,i9,ialarm,I
     &atcr,Iatno,Ichoos,Ino
      integer io,ioinqr,Iprint,iprnt,Ipseud,iread,Ispin,Iw3c,Iwapol,Iwcu
     &bf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao,iwrit,Iwtnab,Iwtnao,Iwt
     &nbo
      integer ix,Iznuc,Jcore,jo,Jprint,Kopt,Label,Larc,Lbl,Lfnao,Lfnarc,
     &Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho
      integer Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Ll,Lorb,Lo
     &rbc,Lstemt,Lstocc,Lu,MAXATM,MAXBAS,Molat,Molata,Molec,Moleca,Munit
      integer Mxao,Mxaolm,Mxbo,nab,Naoctr,Naol,Natoms,Nbas,Ndim,Nmola,Nm
     &olec,nocc,Norbs
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      character*80 title
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
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
      common/nbmol/Nmolec,Molat(MAXATM),Molec(MAXATM,MAXATM),Nmola,Molat
     &a(MAXATM),Moleca(MAXATM,MAXATM)
      common/nbthr/Thrset,Prjset,Accthr,Crtset,E2thr,Athr,Pthr,Ethr,Dthr
      
      dimension T(Ndim,Ndim),DM(Ndim,Ndim),A(1)
      
      data iprnt,iwrit,iread/4HPRNT,4HWRIT,4HREAD/
      
      
      if(Jprint(1).GT.0)then
      write(Lfnpr,99004)
      return
      endif
      
      
      
      i0=1
      i1=i0+Natoms*Natoms
      i2=i1+Ndim
      i3=i2+3*Ndim
      i4=i3+Mxao*Ndim
      i5=i4+Ndim
      i6=i5+Mxbo*Mxbo
      i7=i6+Mxbo*Mxbo
      i8=i7+Mxbo
      i9=i8+Mxbo
      i10=i9+Mxao*Mxao
      i11=i10+Mxao*Mxao
      i12=i11+Mxao
      i13=i12+Mxao
      i14=i13+Mxao
      
      if(Jprint(5).NE.0.AND.Ispin.EQ.0)write(Lfnpr,99001)
      if(Jprint(5).NE.0.AND.Ispin.EQ.2)write(Lfnpr,99002)
      if(Jprint(5).NE.0.AND.Ispin.EQ.-2)write(Lfnpr,99003)
      
      
      if(ioinqr(Iwtnab).EQ.iread)then
      call rdtnab(T,DM,A(i1),A(i2),Iwtnab)
      else
      
      
      if(.NOT.Beta)then
      call corinp(Jprint(2),Jcore)
      call rdcore(Jcore)
      endif
      
      
      if(.NOT.Beta)call chsinp(Jprint(2),Ichoos)
      
      
      if(Ichoos.NE.1)call nathyb(DM,T,A(i0),A(i1),A(i2),A(i3),A(i4),A(i5
     &),A(i6),A(i7),A(i8),A(i9),A(i10),A(i11),A(i12),A(i13),A(i14))
      if(Ichoos.EQ.1)call chsdrv(DM,T,A(i0),A(i1),A(i2),A(i3),A(i4),A(i5
     &),A(i6),A(i7),A(i8),A(i9),A(i10),A(i11),A(i12),A(i13),A(i14))
      
      
      if(Jprint(1).LT.0)return
      
      
      call srtnbo(T,A(i1))
      
      
      call simtrs(DM,T,A(i2),Ndim,Nbas)
      
      
      if(.NOT.Ortho)then
      i0=1
      i1=i0+Natoms*Natoms
      i2=i1+Ndim
      i3=i2+Mxao
      i4=i3+Ndim*Ndim
      i5=i4+Ndim*Ndim
      i6=i5+Ndim
      call xcited(DM,T,A(i2),A(i3),A(i4),A(i5),A(i6),A(i6))
      endif
      endif
      
      
      
      call svtnab(T)
      
      
      call lblnbo
      
      
      i0=1
      i1=i0+Natoms*Natoms
      i2=i1+Ndim
      i3=i2+Ndim
      i4=i3+Ndim
      call anlyze(T,A(i1),A(i2),A(i3),A(i4))
      
      
      if(Jprint(36).NE.0)then
      i0=1
      i1=i0+Natoms*Natoms
      i2=i1+Ndim
      i3=i2+3*Natoms
      i4=i3+Ndim*Ndim
      i5=i4+Ndim*Ndim
      call hybdir(A(i1),A(i2),A(i3),A(i4),A(i5))
      endif
      
      
      call fndmol(A(i2))
      
      
      call nbocla(A(i1),Accthr)
      
      
      
      
      i0=1
      i1=i0+Natoms*Natoms
      i2=i1+Ndim
      i3=i2+Ndim*Ndim
      i4=i3+Ndim*Ndim
      
      
      io=ioinqr(Jprint(20))
      jo=ioinqr(Jprint(30))
      if((io.EQ.iprnt.OR.io.EQ.iwrit).OR.(jo.EQ.iprnt.OR.jo.EQ.iwrit))th
     &en
      call fepnao(T)
      call fetnho(A(i2))
      call matmlt(T,A(i2),A(i3),Ndim,Nbas)
      call fesraw(A(i2))
      call normlz(T,A(i2),Ndim,Nbas)
      if(jo.EQ.iprnt.OR.jo.EQ.iwrit)then
      title='PNHOs in the AO basis:'
      call aout(T,Ndim,Nbas,Nbas,title,1,Jprint(30))
      endif
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      title='PNHO overlap matrix:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,3,Jprint(20))
      endif
      endif
      
      
      call fetnao(T)
      call fetnho(A(i2))
      call matmlt(T,A(i2),A(i3),Ndim,Nbas)
      
      
      io=ioinqr(Jprint(28))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      title='NHOs in the AO basis:'
      call aout(T,Ndim,Nbas,Nbas,title,1,Jprint(28))
      endif
      
      
      io=ioinqr(Jprint(33))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call fetnho(A(i2))
      title='NHOs in the NAO basis:'
      call aout(A(i2),Ndim,Nbas,Nbas,title,2,Jprint(33))
      endif
      
      
      io=ioinqr(Jprint(38))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)call frmtmo(T,A(i2),A(i3),A(i4),3,Jp
     &rint(38))
      
      
      io=ioinqr(Jprint(34))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call fedraw(DM,A(i2))
      if(Iwdm.EQ.1)then
      call fesraw(A(i2))
      call simtrs(DM,A(i2),A(i3),Ndim,Nbas)
      endif
      call simtrs(DM,T,A(i2),Ndim,Nbas)
      title='NHO density matrix:'
      call aout(DM,Ndim,-Nbas,Nbas,title,3,Jprint(34))
      endif
      
      
      io=ioinqr(Jprint(29))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call fefao(A(i2),Iwfock)
      if(Iwfock.NE.0)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      title='NHO Fock matrix:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,3,Jprint(29))
      endif
      endif
      
      
      io=ioinqr(Jprint(52))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      ix=1
      call fedxyz(A(i2),ix)
      if(ix.NE.0)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      title='NHO x dipole integrals:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,3,Jprint(52))
      endif
      ix=2
      call fedxyz(A(i2),ix)
      if(ix.NE.0)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      title='NHO y dipole integrals:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,3,Jprint(52))
      endif
      ix=3
      call fedxyz(A(i2),ix)
      if(ix.NE.0)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      title='NHO z dipole integrals:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,3,Jprint(52))
      endif
      endif
      
      
      
      
      i0=1
      i1=i0+Natoms*Natoms
      i2=i1+Ndim
      i3=i2+Ndim*Ndim
      i4=i3+Ndim*Ndim
      
      
      io=ioinqr(Jprint(21))
      jo=ioinqr(Jprint(25))
      if((io.EQ.iprnt.OR.io.EQ.iwrit).OR.(jo.EQ.iprnt.OR.jo.EQ.iwrit))th
     &en
      call fepnao(T)
      call fetnab(A(i2))
      call matmlt(T,A(i2),A(i3),Ndim,Nbas)
      call fesraw(A(i2))
      call normlz(T,A(i2),Ndim,Nbas)
      if(jo.EQ.iprnt.OR.jo.EQ.iwrit)then
      title='PNBOs in the AO basis:'
      call aout(T,Ndim,Nbas,Nbas,title,1,Jprint(25))
      endif
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      title='PNBO overlap matrix:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,4,Jprint(21))
      endif
      endif
      
      
      call fetnao(T)
      call fetnab(A(i2))
      call matmlt(T,A(i2),A(i3),Ndim,Nbas)
      
      
      call svnbo(T,A(i1),A(i2))
      
      
      if(ioinqr(Iwtnbo).EQ.iwrit)call wrtnbo(T,A(i1),Iwtnbo)
      
      
      if(ioinqr(Iwtnbo).EQ.iprnt)then
      title='NBOs in the AO basis:'
      call aout(T,Ndim,Nbas,Nbas,title,1,Iwtnbo)
      endif
      
      
      if(ioinqr(Iwtnab).EQ.iwrit)then
      call fetnab(A(i2))
      call wrtnab(A(i2),Iwtnab)
      endif
      
      
      if(ioinqr(Iwtnab).EQ.iprnt)then
      call fetnab(A(i2))
      title='NBOs in the NAO basis:'
      call aout(A(i2),Ndim,Nbas,Nbas,title,2,Iwtnab)
      endif
      
      
      io=ioinqr(Jprint(41))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call fetnho(A(i2))
      call trnspo(A(i2),Ndim,Nbas)
      call fetnab(A(i3))
      call matmlt(A(i2),A(i3),A(i4),Ndim,Nbas)
      title='NBOs in the NHO basis:'
      call aout(A(i2),Ndim,Nbas,Nbas,title,3,Jprint(41))
      endif
      
      
      io=ioinqr(Jprint(45))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)call frmtmo(T,A(i2),A(i3),A(i4),4,Jp
     &rint(45))
      
      
      call fedraw(DM,A(i2))
      if(Iwdm.EQ.1.AND..NOT.Ortho)then
      call fesraw(A(i2))
      call simtrs(DM,A(i2),A(i3),Ndim,Nbas)
      endif
      call simtrs(DM,T,A(i2),Ndim,Nbas)
      
      
      io=ioinqr(Jprint(16))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      title='NBO density matrix:'
      call aout(DM,Ndim,-Nbas,Nbas,title,4,Jprint(16))
      endif
      
      
      call fefao(A(i2),Iwfock)
      if(Iwfock.NE.0)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      call svfnbo(A(i2))
      io=ioinqr(Jprint(37))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      title='NBO Fock matrix:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,4,Jprint(37))
      endif
      endif
      
      
      io=ioinqr(Jprint(53))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      ix=1
      call fedxyz(A(i2),ix)
      if(ix.NE.0)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      title='NBO x dipole integrals:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,4,Jprint(53))
      endif
      ix=2
      call fedxyz(A(i2),ix)
      if(ix.NE.0)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      title='NBO y dipole integrals:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,4,Jprint(53))
      endif
      ix=3
      call fedxyz(A(i2),ix)
      if(ix.NE.0)then
      call simtrs(A(i2),T,A(i3),Ndim,Nbas)
      title='NBO z dipole integrals:'
      call aout(A(i2),Ndim,-Nbas,Nbas,title,4,Jprint(53))
      endif
      endif
      
      
      if(Jprint(3).EQ.1.AND.Iwfock.NE.0)call fnboan(A(i1),A(i2),A(i3))
      
      
      if(Jprint(6).EQ.1)then
      i0=1
      i1=i0+Natoms*Natoms
      i2=i1+Ndim
      i3=i2+Ndim*Ndim
      i4=i3+Ndim
      i5=i4+Natoms
      call nbosum(A(i2),A(i1),A(i3),A(i4),A(i5))
      endif
      
      
      
      
      if(Jprint(8).EQ.0)return
      
      
      
      i0=1
      i1=i0+Ndim
      i2=i1+Ndim
      i3=i2+Ndim*Ndim
      call nlmo(Nbas,DM,A(i1),A(i2),A(i3),A(i0),nocc,ialarm)
      if(ialarm.NE.0)return
      
      
      call svtlmo(A(i2))
      
      
      call fetnab(T)
      call matmlt(T,A(i2),A(i3),Ndim,Nbas)
      
      
      
      nab=Natoms*(Natoms-1)/2
      if(Natoms.EQ.1)nab=1
      i0=1
      i1=i0+Ndim
      i2=i1+Ndim
      i3=i2+Ndim
      i4=i3+Natoms*Natoms
      i5=i4+Natoms*Natoms
      i6=i5+nocc*Natoms
      i7=i6+nocc*nab
      call copy(DM,A(i7),Ndim,Nbas,Nbas)
      call lmoanl(T,A(i7),A(i0),A(i1),A(i2),A(i3),A(i4),A(i5),A(i6),nocc
     &,nab)
      
      
      
      i0=1
      i1=i0+Ndim*Ndim
      i2=i1+Ndim*Ndim
      
      
      io=ioinqr(Jprint(48))
      jo=ioinqr(Jprint(49))
      if((io.EQ.iprnt.OR.io.EQ.iwrit).OR.(jo.EQ.iprnt.OR.jo.EQ.iwrit))th
     &en
      call fepnao(T)
      call fetnab(A(i0))
      call matmlt(T,A(i0),A(i1),Ndim,Nbas)
      call fetlmo(A(i0))
      call matmlt(T,A(i0),A(i1),Ndim,Nbas)
      call fesraw(A(i0))
      call normlz(T,A(i0),Ndim,Nbas)
      if(jo.EQ.iprnt.OR.jo.EQ.iwrit)then
      title='PNLMOs in the AO basis:'
      call aout(T,Ndim,Nbas,Nbas,title,1,Jprint(49))
      endif
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call simtrs(A(i0),T,A(i1),Ndim,Nbas)
      title='PNLMO overlap matrix:'
      call aout(A(i0),Ndim,-Nbas,Nbas,title,5,Jprint(48))
      endif
      endif
      
      
      call fetnao(T)
      call fetnab(A(i0))
      call matmlt(T,A(i0),A(i1),Ndim,Nbas)
      call fetlmo(A(i0))
      call matmlt(T,A(i0),A(i1),Ndim,Nbas)
      
      
      call svnlmo(T)
      
      
      io=ioinqr(Jprint(23))
      if(io.EQ.iwrit)call wrnlmo(T,DM,Jprint(23))
      
      
      if(io.EQ.iprnt)then
      title='NLMOs in the AO basis:'
      call aout(T,Ndim,Nbas,Nbas,title,1,Jprint(23))
      endif
      
      
      io=ioinqr(Jprint(18))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call fetnab(A(i0))
      call fetlmo(A(i1))
      call matmlt(A(i0),A(i1),A(i2),Ndim,Nbas)
      title='NLMOs in the NAO basis:'
      call aout(T,Ndim,Nbas,Nbas,title,2,Jprint(18))
      endif
      
      
      io=ioinqr(Jprint(24))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call fetnho(A(i0))
      call trnspo(A(i0),Ndim,Nbas)
      call fetnab(A(i1))
      call matmlt(A(i0),A(i1),A(i2),Ndim,Nbas)
      call fetlmo(A(i1))
      call matmlt(A(i0),A(i1),A(i2),Ndim,Nbas)
      title='NLMOs in the NHO basis:'
      call aout(A(i0),Ndim,Nbas,Nbas,title,3,Jprint(24))
      endif
      
      
      io=ioinqr(Jprint(47))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call fetlmo(A(i0))
      title='NLMOs in the NBO basis:'
      call aout(A(i0),Ndim,Nbas,Nbas,title,4,Jprint(47))
      endif
      
      
      io=ioinqr(Jprint(13))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)call frmtmo(T,A(i0),A(i1),A(i2),5,Jp
     &rint(13))
      
      
      io=ioinqr(Jprint(17))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      title='NLMO density matrix:'
      call aout(DM,Ndim,-Nbas,Nbas,title,5,Jprint(17))
      endif
      
      
      io=ioinqr(Jprint(15))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call fefao(A(i0),Iwfock)
      if(Iwfock.NE.0)then
      call simtrs(A(i0),T,A(i1),Ndim,Nbas)
      title='NLMO Fock matrix:'
      call aout(A(i0),Ndim,-Nbas,Nbas,title,5,Jprint(15))
      endif
      endif
      
      
      io=ioinqr(Jprint(54))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      ix=1
      call fedxyz(A(i0),ix)
      if(ix.NE.0)then
      call simtrs(A(i0),T,A(i1),Ndim,Nbas)
      title='NLMO x dipole integrals:'
      call aout(A(i0),Ndim,-Nbas,Nbas,title,5,Jprint(54))
      endif
      ix=2
      call fedxyz(A(i0),ix)
      if(ix.NE.0)then
      call simtrs(A(i0),T,A(i1),Ndim,Nbas)
      title='NLMO y dipole integrals:'
      call aout(A(i0),Ndim,-Nbas,Nbas,title,5,Jprint(54))
      endif
      ix=3
      call fedxyz(A(i0),ix)
      if(ix.NE.0)then
      call simtrs(A(i0),T,A(i1),Ndim,Nbas)
      title='NLMO z dipole integrals:'
      call aout(A(i0),Ndim,-Nbas,Nbas,title,5,Jprint(54))
      endif
      endif
      
      
      
      if(Jprint(46).NE.0)then
      i1=1
      i2=i1+Ndim*Ndim
      i3=i2+Ndim*Ndim
      i4=i3+Ndim*Ndim
      i5=i4+Ndim*Ndim
      i6=i5+Ndim*Ndim
      i7=i6+Ndim*Ndim
      call dipanl(DM,T,A(i1),A(i2),A(i3),A(i4),A(i5),A(i6),A(i7))
      endif
      return
      
99001 format(//1x,'NATURAL BOND ORBITAL ANALYSIS:')
99002 format(//1x,'NATURAL BOND ORBITAL ANALYSIS,',' alpha spin orbitals
     &:')
99003 format(//1x,'NATURAL BOND ORBITAL ANALYSIS,',' beta spin orbitals:
     &')
99004 format(//1x,'NBO analysis skipped by request.')
      end
C* :1 * 
      
