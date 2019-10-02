
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lmoanl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lmoanl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "lmoanl.web"
      subroutine lmoanl(T,S,RESON,OCC,TS,BORDER,OWBORD,ATLMO,SIAB,NOCC,N
     &AB)
      
      implicit none
      double precision alama2,alamb2,anorm,ATLMO,bo,BORDER,bothr,coef,hu
     &ndrd,hundth,OCC,ovp,owbo,OWBORD,owsum,pct,pctpol,pol,pow,RESON
      double precision S,SIAB,std,sum,T,t99,t99p,tenth,thr,TS,two,zero
      integer i,ia,iab,iahigh,ialow,iat,Iatcr,Iathy,Iatno,ib,Ibxm,ich,Ic
     &hoos,ictr,Ino,Iprin,Iprint,Ipseud,isp,Ispin
      integer Iw3c,Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao
     &,Iwtnab,Iwtnao,Iwtnbo,Iznuc,j,jahigh,jalow,jat,jat0,Jcore,jhigh
      integer jlow,Jprint,k,Kopt,l,l2blnk,l3c,Label,lbd,lbl,lblnk,lcr,Lf
     &nao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab
      integer Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lf
     &nppa,Lfnpr,lhyp,Ll,llp,lname,lry,lstd,Lstocc,Ltyp,Ltyp1,MAXATM
      integer MAXBAS,Munit,Mxao,Mxaolm,Mxbo,NAB,nam,nameat,Naoctr,Naol,n
     &atm1,Natoms,Nbas,Nbotyp,Nbouni,nctr,Ndim,nel,nl,nl1
      integer NLMO,NOCC,Norbs
      integer Ul
      logical closed
      
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbnao/Naoctr(MAXBAS),Naol(MAXBAS),Ltyp1(MAXBAS),Iprin(MAXBA
     &S)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),U
     &l(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Ltyp(MAXBAS),Iathy(MAXBAS,3)
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      dimension T(Ndim,Ndim),S(Ndim,Ndim),OCC(Ndim),RESON(Ndim),TS(Ndim)
     &,SIAB(NOCC,NAB),ATLMO(NOCC,Natoms),BORDER(Natoms,Natoms),OWBORD(Na
     &toms,Natoms),pct(5),pow(5),lname(5),isp(3),nam(3),ich(3,2)
      character*80 title
      data llp,lbd,l3c,lcr,lry/'LP','BD','3C','CR','RY'/
      data lname/'s','p','d','f','g'/
      data zero,hundth,t99,t99p/0.0D0,1.D-2,99.99D0,99.995D0/
      data two,tenth,hundrd,thr/2.0D0,0.1D0,100.0D0,1.0D-6/
      data lhyp,lblnk,l2blnk/'-',' ','  '/
      data bothr/2.0D-3/
      
      closed=.TRUE.
      if(Ispin.NE.0)closed=.FALSE.
      if(Ispin.EQ.0)write(Lfnpr,99006)
      if(Ispin.EQ.2)write(Lfnpr,99007)
      if(Ispin.EQ.-2)write(Lfnpr,99008)
      write(Lfnpr,99001)
      write(Lfnpr,99002)(lhyp,j=1,79)
      do 100 NLMO=1,Nbas
      if(OCC(nlmo).GE.tenth)then
      ib=Ibxm(nlmo)
      lbl=Label(ib,1)
      if(lbl.EQ.llp.OR.lbl.EQ.lcr.OR.lbl.EQ.lry)nctr=1
      if(lbl.EQ.lbd)nctr=2
      if(lbl.EQ.l3c)nctr=3
      do 20 i=1,3
      ia=Label(ib,i+3)
      call convrt(ia,ich(i,1),ich(i,2))
      nam(i)=l2blnk
      if(ia.GT.0)nam(i)=nameat(Iatno(ia))
      isp(i)=lhyp
      if(i.GE.nctr)isp(i)=lblnk
20    continue
      do 40 ictr=1,nctr
      isp(ictr)=lhyp
      if(ictr.EQ.nctr)isp(ictr)=lblnk
      i=Label(ib,ictr+3)
      nel=nameat(Iatno(i))
40    continue
      write(Lfnpr,99003)NLMO,OCC(nlmo),RESON(nlmo),(Label(ib,k),k=1,3),(
     &nam(k),ich(k,1),ich(k,2),isp(k),k=1,3)
      if(OCC(nlmo).GE.tenth.OR.lbl.NE.lry)then
      do 60 iat=1,Natoms
      nl=0
      do 45 l=1,5
      pct(l)=zero
45    continue
      jlow=Ll(iat)
      jhigh=Ul(iat)
      do 50 j=jlow,jhigh
      l=Naol(j)/100+1
      coef=T(j,nlmo)
      pct(l)=pct(l)+coef*coef
50    continue
      nl=l
      pol=zero
      do 55 l=1,5
      pol=pol+pct(l)
55    continue
      if(nlmo.LE.NOCC)ATLMO(nlmo,iat)=pol
      pctpol=pol*hundrd
      if(pctpol.GE.hundth)then
      do 56 l=1,5
      pct(l)=hundrd*pct(l)/pol
56    continue
      lstd=0
      do 58 l=1,nl
      if(lstd.LE.0)then
      pow(l)=zero
      std=pct(l)
      if(std.LT.hundth)goto 58
      lstd=l
      endif
      pow(l)=pct(l)/std
      if(pow(l).GT.t99p)pow(l)=t99
58    continue
      nl1=nl
      nel=nameat(Iatno(iat))
      if(nl1.GT.3)nl1=3
      write(Lfnpr,99004)pctpol,nel,iat,pct(1),(lname(l),pow(l),pct(l),l=
     &2,nl1)
      if(nl.GT.3)write(Lfnpr,99005)(lname(l),pow(l),pct(l),l=4,nl)
      endif
60    continue
      endif
      endif
100   continue
      
      
      if(.NOT.(Ortho))then
      call fesnao(S)
      do 150 NLMO=1,NOCC
      iab=0
      natm1=Natoms-1
      do 140 iat=1,natm1
      ialow=Ll(iat)
      iahigh=Ul(iat)
      do 110 l=1,Nbas
      if(l.LT.ialow.OR.l.GT.iahigh)then
      TS(l)=zero
      do 102 k=ialow,iahigh
      TS(l)=TS(l)+T(k,nlmo)*S(k,l)
102   continue
      endif
110   continue
      jat0=iat+1
      do 120 jat=jat0,Natoms
      iab=iab+1
      ovp=zero
      jalow=Ll(jat)
      jahigh=Ul(jat)
      do 115 l=jalow,jahigh
      ovp=ovp+TS(l)*T(l,nlmo)
115   continue
      anorm=dsqrt(ATLMO(nlmo,iat)*ATLMO(nlmo,jat))
      if(anorm.LT.thr)then
      SIAB(nlmo,iab)=zero
      else
      SIAB(nlmo,iab)=ovp/anorm
      endif
120   continue
140   continue
150   continue
      if(Jprint(12).NE.0)then
      iab=0
      natm1=Natoms-1
      write(Lfnpr,99009)
      do 180 iat=1,natm1
      jat0=iat+1
      do 160 jat=jat0,Natoms
      iab=iab+1
      sum=zero
      owsum=zero
      do 155 NLMO=1,NOCC
      alama2=ATLMO(nlmo,iat)
      alamb2=ATLMO(nlmo,jat)
      ovp=SIAB(nlmo,iab)
      bo=alama2
      if(alamb2.LT.alama2)bo=alamb2
      if(closed)bo=bo*two
      owbo=bo*ovp
      if(ovp.LT.zero)bo=-bo
      if(dabs(bo).GT.bothr)write(Lfnpr,99010)iat,jat,NLMO,bo,ovp
      sum=sum+bo
      owsum=owsum+owbo
155   continue
      BORDER(iat,jat)=sum
      BORDER(jat,iat)=sum
      OWBORD(iat,jat)=owsum
      OWBORD(jat,iat)=owsum
160   continue
180   continue
      do 200 iat=1,Natoms
      BORDER(iat,iat)=zero
      OWBORD(iat,iat)=zero
200   continue
      do 220 iat=1,Natoms
      sum=zero
      do 210 jat=1,Natoms
      sum=sum+BORDER(iat,jat)
210   continue
      TS(iat)=sum
220   continue
      title='Atom-Atom Net Linear NLMO/NPA Bond Orders:'
      call aout(BORDER,Natoms,Natoms,Natoms,title,0,Natoms)
      endif
      endif
      return
      
99001 format(1x,'NLMO/Occupancy/Percent from Parent NBO/ Atomic ','Hybri
     &d Contributions')
99002 format(1x,80A1)
99003 format(1x,i3,'. (',f7.5,') ',f8.4,'%  ',a2,a1,'(',i2,')',3(a2,3A1)
     &)
99004 format(26x,f7.3,'% ',a2,i2,' s(',f6.2,'%)',2(a1,f5.2,'(',f6.2,'%)'
     &))
99005 format(50x,2(a1,f5.2,'(',f6.2,'%)'))
99006 format(/1x,'Hybridization/Polarization Analysis of NLMOs ','in NAO
     & Basis:')
99007 format(/1x,'Hybridization/Polarization Analysis of NLMOs ','in NAO
     & Basis, Alpha Spin:')
99008 format(/1x,'Hybridization/Polarization Analysis of NLMOs ','in NAO
     & Basis, Beta Spin:')
99009 format(/1x,'Individual LMO bond orders greater than 0.002',' in ma
     &gnitude,',/,1x,' with the overlap between the hybrids in the NLMO 
     &given:',//,1x,'Atom I / Atom J / NLMO / Bond Order / Hybrid Overla
     &p /')
99010 format(1x,i4,i8,2x,i6,f14.7,f16.7)
99011 format(1x,'** TOTALS **',f14.7,14x,f14.7)
      end
C* :1 * 
      
