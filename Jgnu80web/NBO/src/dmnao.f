
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dmnao"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dmnao.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "dmnao.web"
      subroutine dmnao(DM,T,A)
      implicit none
      double precision A,Accthr,Athr,Crtset,DM,Dthr,E2thr,Ethr,Prjset,Pt
     &hr,T,Thrset
      integer i,i1,i2,i3,i4,i5,Ichoos,io,ioinqr,Iprin,Iprint,iprnt,Ipseu
     &d,Ispin,it,Iw3c,Iwapol,Iwcubf,Iwdetl,Iwdm
      integer Iwfock,Iwhybs,Iwmulp,Iwpnao,iwrit,Iwtnab,Iwtnao,Iwtnbo,Jco
     &re,Jprint,Kopt,Label,Lang,Larc,Lbl,Lctr,Lfnao,Lfnarc,Lfndaf,Lfndef
      integer Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpn
     &a,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Lorb,Lorbc,Lstemt,Lstocc,Ltyp,
     &MAXATM
      integer MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Naoa,Naoc,Naoctr,Naol,Natoms
     &,Nbas,Ndim
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbthr/Thrset,Prjset,Accthr,Crtset,E2thr,Athr,Pthr,Ethr,Dthr
      common/nbao/Lctr(MAXBAS),Lang(MAXBAS)
      common/nbnao/Naoc(MAXBAS),Naoa(MAXBAS),Ltyp(MAXBAS),Iprin(MAXBAS)
      
      dimension DM(Ndim,Ndim),T(Ndim,Ndim),A(1)
      character*80 title
      
      data iprnt,iwrit/4HPRNT,4HWRIT/
      
      
      if(Alpha)then
      if(Jprint(4).NE.0)write(Lfnpr,99001)
      else
      do 50 i=1,Nbas
      Naoctr(i)=Naoc(i)
      Naol(i)=Naoa(i)
      Lbl(i)=Lctr(i)
      Lorbc(i)=Lang(i)
50    continue
      call fetnao(T)
      if(Jprint(4).NE.0)write(Lfnpr,99002)
      endif
      
      
      io=ioinqr(Jprint(26))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call feaomo(A,it)
      if(it.NE.0)then
      title='MOs in the AO basis:'
      call aout(A,Ndim,Nbas,Nbas,title,1,Jprint(26))
      endif
      endif
      
      
      io=ioinqr(Jprint(40))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      call fefao(A,Iwfock)
      if(Iwfock.NE.0)then
      title='AO Fock matrix:'
      call aout(A,Ndim,-Nbas,Nbas,title,1,Jprint(40))
      endif
      endif
      
      
      call fedraw(DM,A)
      
      
      io=ioinqr(Jprint(42))
      if(Iwdm.NE.0.AND.(io.EQ.iprnt.OR.io.EQ.iwrit))then
      title='AO bond-order matrix:'
      call aout(DM,Ndim,-Nbas,Nbas,title,1,Jprint(42))
      endif
      
      
      if(Iwdm.NE.0)then
      i1=1
      i2=i1+Ndim*Ndim
      call fesraw(A(i1))
      call simtrm(DM,A(i1),A(i2),Ndim,Nbas,Iwmulp,Iwcubf)
      endif
      
      
      io=ioinqr(Jprint(27))
      if(io.EQ.iprnt.OR.io.EQ.iwrit)then
      title='AO density matrix:'
      call aout(DM,Ndim,-Nbas,Nbas,title,1,Jprint(27))
      endif
      
      
      call simtrs(DM,T,A,Ndim,Nbas)
      
      
      call svdnao(DM)
      
      
      i1=1
      i2=i1+Natoms*Natoms
      i3=i2+Natoms
      i4=i3+Natoms*Natoms
      i5=i4+Ndim*Ndim
      call fesnao(T)
      call naoanl(DM,T,A(i1),A(i2),A(i3),A(i4),A(i5))
      
      
      
      
      call lblnao
      
      
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
      
99001 format(//1x,'***************************************************',
     &/1x,'*******         Alpha spin orbitals         *******',/1x,'***
     &************************************************')
99002 format(//1x,'***************************************************',
     &/1x,'*******         Beta  spin orbitals         *******',/1x,'***
     &************************************************')
      end
C* :1 * 
      
