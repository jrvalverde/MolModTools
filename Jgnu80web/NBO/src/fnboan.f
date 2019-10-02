
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fnboan"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fnboan.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "fnboan.web"
      subroutine fnboan(BNDOCC,F,MOLNBO)
      implicit none
      double precision absfij,Accthr,Athr,aukcal,BNDOCC,conv,Crtset,de,D
     &thr,E2thr,ekcal,epert,Ethr,ethr1,ethr2,ethrsh,evkcal,F,fulloc,hund
     &th
      integer i,ia,Iatcr,Iathy,Iatno,ib,ibas,Ibxm,ich,Ichoos,imol,inam,I
     &no,iocc,Iprint,Ipseud,isp,Ispin,Iw3c,Iwapol
      integer Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao,Iwtnab,Iwtn
     &ao,Iwtnbo,Iznuc,j,ja,jb,jbas,jch,Jcore,jmol,jnam,Jprint
      integer jsp,jstar,k,Kopt,l3c,Label,lbd,lbl,lblnk1,lblnk2,Lfnao,Lfn
     &arc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo
      integer Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,lhy
     &p,Ll,Lstocc,Lu,MAXATM,MAXBAS,Molat,Molata,Molec,Moleca,Mollst,MOLN
     &BO
      integer Munit,Mxao,Mxaolm,Mxbo,nameat,Natoms,Nbas,Nbotyp,Nbouni,nc
     &tr,Ndim,nele,Nmola,Nmolec,nocc,Norbs,nstar,ntri
      double precision occfac,one,Prjset,Pthr,ten,Thrset,totocc,two,zero
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Mollst(MAXBAS),Iathy(MAXBAS,3)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbmol/Nmolec,Molat(MAXATM),Molec(MAXATM,MAXATM),Nmola,Molat
     &a(MAXATM),Moleca(MAXATM,MAXATM)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbthr/Thrset,Prjset,Accthr,Crtset,E2thr,Athr,Pthr,Ethr,Dthr
      dimension BNDOCC(Nbas),F(Ndim,Ndim),MOLNBO(2,Nbas,Nmolec)
      dimension inam(3),jnam(3),ich(3,2),jch(3,2),isp(3),jsp(3)
      
      data lbd/2HBD/,l3c/2H3C/,lblnk1/1H /,lblnk2/2H  /,lhyp/1H-/
      data hundth/0.01D0/
      data aukcal/627.51D0/,evkcal/23.060D0/
      data zero,one,two,ten/0.0D0,1.0D0,2.0D0,1.0D1/
      
      
      
      ethr1=dabs(E2thr)
      if(Ispin.NE.0.AND.E2thr.LT.zero)ethr1=ethr1/two
      ethr2=dabs(E2thr)/ten
      if(Ispin.NE.0.AND.E2thr.LT.zero)ethr2=ethr2/two
      
      
      ntri=Ndim*(Ndim+1)/2
      call fefnbo(F)
      call unpack(F,Ndim,Nbas,ntri)
      
      
      
      do 200 imol=1,Nmolec
      nocc=0
      nstar=0
      do 50 ibas=1,Nbas
      do 20 i=1,2
      MOLNBO(i,ibas,imol)=0
20    continue
50    continue
      do 100 ibas=1,Nbas
      if(imol.EQ.Nbouni(ibas))then
      if(Nbotyp(ibas).LE.20)then
      nocc=nocc+1
      MOLNBO(1,nocc,imol)=ibas
      if(Nbotyp(ibas).LT.10)goto 100
      endif
      nstar=nstar+1
      MOLNBO(2,nstar,imol)=ibas
      endif
100   continue
200   continue
      
      
      if(Munit.EQ.0)then
      conv=aukcal
      elseif(Munit.EQ.1)then
      conv=evkcal
      else
      conv=one
      endif
      
      
      write(Lfnpr,99004)ethr1
      if(Nmolec.GT.1)write(Lfnpr,99005)ethr2
      if(Munit.EQ.0)then
      write(Lfnpr,99006)
      elseif(Munit.EQ.1)then
      write(Lfnpr,99007)
      else
      write(Lfnpr,99008)
      endif
      do 300 imol=1,Nmolec
      do 250 jmol=1,Nmolec
      if(imol.EQ.jmol)write(Lfnpr,99001)imol
      if(imol.NE.jmol)write(Lfnpr,99002)imol,jmol
      ethrsh=ethr1
      if(imol.NE.jmol)ethrsh=ethr2
      nele=0
      do 220 iocc=1,Nbas
      ibas=MOLNBO(1,iocc,imol)
      if(ibas.NE.0)then
      ib=Ibxm(ibas)
      lbl=Label(ib,1)
      nctr=1
      if(lbl.EQ.lbd)nctr=2
      if(lbl.EQ.l3c)nctr=3
      do 205 i=1,3
      ia=Label(ib,i+3)
      call convrt(ia,ich(i,1),ich(i,2))
      inam(i)=lblnk2
      if(ia.GT.0)inam(i)=nameat(Iatno(ia))
      isp(i)=lhyp
      if(i.GE.nctr)isp(i)=lblnk1
205   continue
      do 210 jstar=1,Nbas
      jbas=MOLNBO(2,jstar,jmol)
      if(jbas.NE.0)then
      if(ibas.NE.jbas)then
      de=F(jbas,jbas)-F(ibas,ibas)
      if(de.GE.hundth)then
      absfij=abs(F(ibas,jbas))
      epert=(absfij**2)/de
      
      
      totocc=BNDOCC(ibas)+BNDOCC(jbas)
      fulloc=two
      if(Ispin.NE.0)fulloc=one
      occfac=totocc
      if(totocc.GT.fulloc)occfac=two*fulloc-totocc
      
      
      epert=epert*occfac
      ekcal=epert*conv
      if(ekcal.GE.ethrsh)then
      nele=nele+1
      jb=Ibxm(jbas)
      lbl=Label(jb,1)
      nctr=1
      if(lbl.EQ.lbd)nctr=2
      if(lbl.EQ.l3c)nctr=3
      do 206 j=1,3
      ja=Label(jb,j+3)
      call convrt(ja,jch(j,1),jch(j,2))
      jnam(j)=lblnk2
      if(ja.GT.0)jnam(j)=nameat(Iatno(ja))
      jsp(j)=lhyp
      if(j.GE.nctr)jsp(j)=lblnk1
206   continue
      write(Lfnpr,99009)ibas,(Label(ib,k),k=1,3),(inam(k),ich(k,1),ich(k
     &,2),isp(k),k=1,2),inam(3),ich(3,1),ich(3,2),jbas,(Label(jb,k),k=1,
     &3),(jnam(k),jch(k,1),jch(k,2),jsp(k),k=1,2),jnam(3),jch(3,1),jch(3
     &,2),ekcal,de,absfij
      endif
      endif
      endif
      endif
210   continue
      endif
220   continue
      if(nele.EQ.0)write(Lfnpr,99003)
250   continue
300   continue
      return
      
99001 format(/1x,'within unit ',i2)
99002 format(/1x,'from unit ',i2,' to unit ',i2)
99003 format(1x,'      None above threshold')
99004 format(//,1x,'Second Order Perturbation Theory Analysis ','of Fock
     & Matrix in NBO Basis'//,1x,'    Threshold for printing:  ',f5.2,' 
     &kcal/mol')
99005 format(1x,'   (Intermolecular threshold:',f5.2,' kcal/mol)')
99006 format(56x,'  E(2)  E(j)-E(i) F(i,j)'/6x,'Donor NBO (i)',14x,'Acce
     &ptor NBO (j)',7x,'kcal/mol   a.u.    a.u. ',/1x,79('='))
99007 format(56x,'  E(2)  E(j)-E(i) F(i,j)'/6x,'Donor NBO (i)',14x,'Acce
     &ptor NBO (j)',7x,'kcal/mol   e.V.    e.V. ',/1x,79('='))
99008 format(56x,'  E(2)  E(j)-E(i) F(i,j)'/6x,'Donor NBO (i)',14x,'Acce
     &ptor NBO (j)',7x,'kcal/mol   kcal    kcal ',/1x,79('='))
99009 format(1x,i3,'. ',a2,a1,'(',i2,')',a2,3A1,a2,3A1,a2,2A1,'/',i3,'. 
     &',a2,a1,'(',i2,')',a2,3A1,a2,3A1,a2,2A1,f8.2,f8.2,f9.3)
      end
C* :1 * 
      
