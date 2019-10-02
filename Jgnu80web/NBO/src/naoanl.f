
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 naoanl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "naoanl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "naoanl.web"
      subroutine naoanl(DM,SPNAO,BINDEX,BINDT,OVPOP,F,ENAO)
      implicit none
      double precision Accthr,allow,allow2,angl,Athr,BINDEX,BINDT,chg,cr
     &thrs,Crtset,cubicf,DM,dmij,dmij2,dmsij,Dthr,E2thr,ecp,ENAO,enrg
      double precision Ethr,F,occ,occec,OVPOP,pcent,Prjset,Pthr,sij,SPNA
     &O,suma,sumac,sumar,sumav,sumc,sumr,sumtf,sumti,sumtt,sumv
      double precision tenth,test,test2,thold,Thrset,tot,two,zero
      integer i,iang,iat,Iatcr,Iatno,ichcor,Ichoos,ichryd,ichval,icore,i
     &ct,ictr,iecp,ifock,ii,il,ilm,inao,Ino,Iprin
      integer Iprint,Ipseud,iryd,Ispin,ityp,ival,Iw3c,Iwapol,Iwcubf,Iwde
     &tl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao,Iwtnab,Iwtnao,Iwtnbo,Iznuc,j
      integer jat,Jcore,jct,jctr,jmax,jprin,Jprint,k,kct,Kopt,l,la,labec
     &,Label,lang,Larc,Lbl,Lbls,Lfnao,Lfnarc
      integer Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnh
     &o,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Ll,lm,lnum,Lorb
      integer Lorbc,Lstemt,Lstocc,Ltyp,Lu,m,MAXATM,MAXBAS,mmax,morb,Muni
     &t,Mxao,Mxaolm,Mxbo,n,na,nam,nameat,Naoa,Naoc
      integer Naoctr,Naol,Natoms,Nbas,nctr,Ndim,nel,Nlew,noma,nomac,noma
     &v,norb,Norbs,npl,Nval,nwarn
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      
      
      logical first,core,allzer
      parameter(MAXATM=99,MAXBAS=500)
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbnao/Naoc(MAXBAS),Naoa(MAXBAS),Ltyp(MAXBAS),Iprin(MAXBAS)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbthr/Thrset,Prjset,Accthr,Crtset,E2thr,Athr,Pthr,Ethr,Dthr
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      common/nblbl/Nlew,Nval,Lbls(10,MAXBAS,4)
      dimension DM(Ndim,Ndim),SPNAO(Ndim,Ndim),BINDEX(Natoms,Natoms),BIN
     &DT(Natoms),OVPOP(Natoms,Natoms),F(Ndim,Ndim),ENAO(Ndim),jprin(MAXB
     &AS),icore(4),ival(4),nwarn(MAXATM),labec(20,2),occec(20)
      dimension iang(5),angl(25),lang(25),cubicf(7)
      character*80 title
      data iryd/'Ryd'/
      data iang/'s','p','d','f','g'/
      data lang/51,151,152,153,251,252,253,254,255,351,352,353,354,355,3
     &56,357,451,452,453,454,455,456,457,458,459/
      data angl/4H    ,4Hx   ,4Hy   ,4Hz   ,4Hxy  ,4Hxz  ,4Hyz  ,4Hx2y2,
     &4Hz2  ,4H(0) ,4H(C1),4H(S1),4H(C2),4H(S2),4H(C3),4H(S3),4H(0) ,4H(
     &C1),4H(S1),4H(C2),4H(S2),4H(C3),4H(S3),4H(C4),4H(S4)/
      data cubicf/4H(D1),4H(D2),4H(D3),4H(B) ,4H(E1),4H(E2),4H(E3)/
      data zero,tenth,two/0.0D0,0.1D0,2.0D0/
      
      
      data test,test2/1.0D-5,2.1D-5/
      data allow,allow2/1.0D-3,2.1D-3/
      data ichcor,ichval,ichryd/'Cor','Val','Ryd'/
      
      
      if(Iwcubf.NE.0)then
      do 50 i=1,7
      ii=i+9
      angl(ii)=cubicf(i)
50    continue
      endif
      
      
      do 200 j=1,Natoms
      do 100 i=1,Natoms
      OVPOP(i,j)=zero
      BINDEX(i,j)=zero
100   continue
200   continue
      do 300 i=1,Nbas
      iat=Naoctr(i)
      do 250 j=1,Nbas
      jat=Naoctr(j)
      if(jat.NE.iat)then
      sij=SPNAO(i,j)
      dmij=DM(i,j)
      dmij2=dmij*dmij
      dmsij=dmij*sij
      BINDEX(jat,iat)=BINDEX(jat,iat)+dmij2
      OVPOP(jat,iat)=OVPOP(jat,iat)+dmsij
      endif
250   continue
300   continue
      
      
      call fetnao(SPNAO)
      ifock=Iwfock
      if(Open.AND..NOT.(Alpha.OR.Beta))ifock=0
      if(ifock.EQ.1)then
      call fefao(F,Iwfock)
      if(Iwfock.NE.0)then
      do 320 i=1,Nbas
      enrg=zero
      do 310 j=1,Nbas
      do 305 k=1,Nbas
      enrg=enrg+SPNAO(j,i)*F(j,k)*SPNAO(k,i)
305   continue
310   continue
      ENAO(i)=enrg
320   continue
      endif
      endif
      
      
      do 400 i=1,Nbas
      Ltyp(i)=iryd
400   continue
      iecp=0
      do 500 nctr=1,Natoms
      call cortbl(nctr,icore,iecp)
      call valtbl(nctr,ival)
      do 450 l=0,3
      ityp=iang(l+1)
      lnum=2*l+1
      if(icore(l+1).GT.0)then
      do 410 m=1,icore(l+1)
      do 405 la=1,lnum
      morb=0
      occ=-1.0
      do 402 n=1,Nbas
      lm=Naol(n)
      norb=lm/100
      il=iang(norb+1)
      na=mod(Naol(n),50)
      if(Naoctr(n).EQ.nctr.AND.il.EQ.ityp.AND.DM(n,n).GT.occ.AND.Ltyp(n)
     &.EQ.iryd.AND.la.EQ.na)then
      morb=n
      occ=DM(n,n)
      endif
402   continue
      if(morb.EQ.0)then
      write(Lfnpr,99025)ityp,nameat(Iatno(nctr)),nctr,(icore(i),i=1,4),m
     &,la
      stop
      endif
      Ltyp(morb)=ichcor
405   continue
410   continue
      endif
      if(ival(l+1).GT.0)then
      do 420 m=1,ival(l+1)
      do 415 la=1,lnum
      morb=0
      occ=-1.0
      do 412 n=1,Nbas
      lm=Naol(n)
      norb=lm/100
      il=iang(norb+1)
      na=mod(Naol(n),50)
      if(Naoctr(n).EQ.nctr.AND.il.EQ.ityp.AND.DM(n,n).GT.occ.AND.Ltyp(n)
     &.EQ.iryd.AND.la.EQ.na)then
      morb=n
      occ=DM(n,n)
      endif
412   continue
      if(morb.EQ.0)then
      write(Lfnpr,99026)ityp,nameat(Iatno(nctr)),nctr,(ival(i),i=1,4),m,
     &la
      stop
      endif
      Ltyp(morb)=ichval
415   continue
420   continue
      endif
450   continue
500   continue
      
      
      do 600 i=1,Nbas
      Iprin(i)=0
600   continue
      do 700 nctr=1,Natoms
      iecp=1
      call cortbl(nctr,ival,iecp)
      iecp=0
      call cortbl(nctr,icore,iecp)
      do 650 l=0,4
      ityp=iang(l+1)
      mmax=2*l+1
      do 640 m=1,mmax
      if(l.EQ.4)then
      n=3
      else
      n=ival(l+1)-icore(l+1)+l
      endif
610   morb=0
      occ=-1.0
      do 620 j=1,Nbas
      lm=Naol(j)
      norb=lm/100
      il=iang(norb+1)
      na=mod(Naol(j),50)
      if(Naoctr(j).EQ.nctr.AND.il.EQ.ityp.AND.DM(j,j).GT.occ.AND.Iprin(j
     &).EQ.0.AND.m.EQ.na)then
      morb=j
      occ=DM(j,j)
      endif
620   continue
      if(morb.NE.0)then
      n=n+1
      Iprin(morb)=n
      goto 610
      endif
640   continue
650   continue
700   continue
      
      
      if(ifock.NE.0)then
      do 750 i=1,Nbas
      jprin(i)=0
750   continue
      do 800 nctr=1,Natoms
      iecp=1
      call cortbl(nctr,ival,iecp)
      iecp=0
      call cortbl(nctr,icore,iecp)
      do 780 l=0,4
      ityp=iang(l+1)
      mmax=2*l+1
      do 770 m=1,mmax
      if(l.EQ.4)then
      n=3
      else
      n=ival(l+1)-icore(l+1)+l
      endif
755   morb=0
      enrg=1.0D6
      do 760 j=1,Nbas
      lm=Naol(j)
      norb=lm/100
      il=iang(norb+1)
      na=mod(Naol(j),50)
      if(Naoctr(j).EQ.nctr.AND.il.EQ.ityp.AND.ENAO(j).LT.enrg.AND.jprin(
     &j).EQ.0.AND.m.EQ.na)then
      morb=j
      enrg=ENAO(j)
      endif
760   continue
      if(morb.NE.0)then
      n=n+1
      jprin(morb)=n
      goto 755
      endif
770   continue
780   continue
800   continue
      endif
      
      
      tot=zero
      do 900 inao=1,Nbas
      tot=tot+DM(inao,inao)
900   continue
      nel=tot+tenth
      
      
      Nlew=nel
      
      
      if(tot.GE.zero)then
      sumtt=tot+test
      sumti=aint(sumtt)
      sumtf=sumtt-sumti
      if(sumtf.GT.test2)then
      sumtt=tot+allow
      sumti=aint(sumtt)
      sumtf=sumtt-sumti
      if(sumtf.GT.allow2)then
      write(Lfnpr,99014)
      Jprint(4)=-1
      else
      write(Lfnpr,99015)
      endif
      endif
      else
      write(Lfnpr,99014)
      Jprint(4)=-1
      endif
      
      
      if(Jprint(4).NE.0)then
      if(ifock.EQ.1)then
      write(Lfnpr,99001)
      else
      write(Lfnpr,99002)
      endif
      jctr=1
      do 950 i=1,Nbas
      nctr=Naoctr(i)
      if(nctr.NE.jctr)then
      write(Lfnpr,*)
      jctr=nctr
      endif
      iat=Iatno(nctr)
      nam=nameat(iat)
      lm=Naol(i)
      l=lm/100
      il=iang(l+1)
      do 920 ilm=1,25
      if(lm.EQ.lang(ilm))goto 940
920   continue
940   occ=DM(i,i)
      if(occ.LT.zero)occ=zero
      if(ifock.EQ.1)then
      write(Lfnpr,99003)i,nam,nctr,il,angl(ilm),Ltyp(i),jprin(i),il,occ,
     &ENAO(i)
      else
      write(Lfnpr,99003)i,nam,nctr,il,angl(ilm),Ltyp(i),Iprin(i),il,occ
      endif
950   continue
      
      
      iecp=0
      do 1000 i=1,Natoms
      iecp=iecp+Iatno(i)-Iznuc(i)
1000  continue
      if(Ipseud.NE.0)then
      if(Alpha.OR.Beta)iecp=iecp/2
      write(Lfnpr,99004)iecp
      endif
      
      
      crthrs=Crtset
      if(Alpha.OR.Beta)crthrs=crthrs-1.0
      do 1050 n=1,Natoms
      nwarn(n)=0
1050  continue
      do 1100 i=1,Nbas
      ictr=Naoctr(i)
      if(Ltyp(i).EQ.ichcor.AND.DM(i,i).LT.crthrs)nwarn(ictr)=nwarn(ictr)
     &+1
1100  continue
      first=.TRUE.
      do 1150 n=1,Natoms
      nam=nameat(Iatno(n))
      if(nwarn(n).EQ.1)then
      if(first)then
      write(Lfnpr,99005)crthrs,nam,n
      first=.FALSE.
      else
      write(Lfnpr,99006)crthrs,nam,n
      endif
      elseif(nwarn(n).GT.1)then
      if(first)then
      write(Lfnpr,99007)nwarn(n),crthrs,nam,n
      first=.FALSE.
      else
      write(Lfnpr,99008)nwarn(n),crthrs,nam,n
      endif
      endif
1150  continue
      
      
      if(ifock.EQ.1)then
      do 1160 n=1,Natoms
      nwarn(n)=0
1160  continue
      do 1180 i=1,Nbas
      ictr=Naoctr(i)
      if(Iprin(i).NE.jprin(i))nwarn(ictr)=1
      Iprin(i)=jprin(i)
1180  continue
      first=.TRUE.
      do 1200 n=1,Natoms
      nam=nameat(Iatno(n))
      if(nwarn(n).GT.0)then
      if(first)then
      write(Lfnpr,99009)nam,n
      first=.FALSE.
      else
      write(Lfnpr,99010)nam,n
      endif
      endif
1200  continue
      endif
      
      
      write(Lfnpr,99011)
      sumac=zero
      sumav=zero
      sumar=zero
      nomac=0
      do 1250 i=1,Natoms
      sumc=zero
      sumv=zero
      sumr=zero
      nam=nameat(Iatno(i))
      do 1220 j=1,Nbas
      if(Naoctr(j).EQ.i)then
      occ=DM(j,j)
      if(occ.LT.zero)occ=zero
      if(Ltyp(j).EQ.ichcor)sumc=sumc+occ
      if(Ltyp(j).EQ.ichval)sumv=sumv+occ
      if(Ltyp(j).EQ.ichryd)sumr=sumr+occ
      if(Ltyp(j).EQ.ichcor)nomac=nomac+2
      endif
1220  continue
      tot=sumc+sumv+sumr
      if(Alpha.OR.Beta)then
      chg=Iznuc(i)/2.0-tot
      else
      chg=Iznuc(i)-tot
      endif
      ecp=dble(Iatno(i)-Iznuc(i))
      if(Alpha.OR.Beta)ecp=ecp/two
      write(Lfnpr,99012)nam,i,chg,sumc+ecp,sumv,sumr,tot+ecp
      sumac=sumac+sumc
      sumav=sumav+sumv
      sumar=sumar+sumr
1250  continue
      tot=sumac+sumav+sumar
      chg=-1.0*tot
      if(Alpha.OR.Beta)then
      nomac=nomac/2
      do 1260 i=1,Natoms
      chg=chg+Iznuc(i)/2.0
1260  continue
      else
      do 1280 i=1,Natoms
      chg=chg+Iznuc(i)
1280  continue
      endif
      write(Lfnpr,99013)chg,sumac+dble(iecp),sumav,sumar,tot+dble(iecp)
      
      
      write(Lfnpr,99016)
      noma=nel
      nomav=noma-nomac
      suma=sumac+sumav
      if(Ipseud.NE.0)then
      ecp=iecp
      suma=suma+ecp
      noma=noma+iecp
      write(Lfnpr,99017)ecp
      endif
      if(nomac.NE.0)then
      pcent=sumac/nomac*100.0
      write(Lfnpr,99018)sumac,pcent,nomac
      elseif(sumac.NE.zero)then
      pcent=zero
      write(Lfnpr,99018)sumac,pcent,nomac
      endif
      if(nomav.NE.0)then
      pcent=sumav/nomav*100.0
      write(Lfnpr,99019)sumav,pcent,nomav
      elseif(sumav.NE.zero)then
      pcent=zero
      write(Lfnpr,99019)sumav,pcent,nomav
      endif
      if(noma.NE.0)then
      pcent=suma/noma*100.0
      else
      pcent=zero
      endif
      write(Lfnpr,99020)suma,pcent,noma
      if(noma.NE.0)then
      pcent=sumar/noma*100.0
      write(Lfnpr,99021)sumar,pcent,noma
      elseif(sumar.NE.zero)then
      pcent=0
      write(Lfnpr,99021)sumar,pcent,noma
      endif
      
      
      write(Lfnpr,99022)
      do 1400 nctr=1,Natoms
      ict=0
      iecp=1
      call cortbl(nctr,icore,iecp)
      do 1300 npl=1,8
      do 1290 n=1,npl
      l=npl-n
      if(l.GE.0.AND.l.LT.n)then
      if(n.GT.icore(l+1)+l)then
      ict=ict+1
      labec(ict,1)=n
      labec(ict,2)=iang(l+1)
      occec(ict)=zero
      endif
      endif
1290  continue
1300  continue
      do 1320 i=1,Nbas
      ictr=Naoctr(i)
      if(ictr.EQ.nctr.AND.Ltyp(i).NE.ichcor)then
      norb=Naol(i)/100
      il=iang(norb+1)
      do 1305 j=1,ict
      if(Iprin(i).EQ.labec(j,1).AND.il.EQ.labec(j,2))then
      occec(j)=occec(j)+DM(i,i)
      goto 1320
      endif
1305  continue
      endif
1320  continue
      if(labec(1,1).NE.1)then
      core=.TRUE.
      else
      core=.FALSE.
      endif
      thold=5.0D-3
      jmax=ict
      do 1340 jct=1,ict
1330  if(occec(jct).LT.thold)then
      allzer=.TRUE.
      do 1335 kct=jct,ict-1
      labec(kct,1)=labec(kct+1,1)
      labec(kct,2)=labec(kct+1,2)
      occec(kct)=occec(kct+1)
      if(occec(kct).GE.thold)allzer=.FALSE.
1335  continue
      occec(ict)=zero
      if(allzer)then
      jmax=jct-1
      goto 1360
      endif
      goto 1330
      endif
1340  continue
1360  nam=nameat(Iatno(nctr))
      if(jmax.EQ.0)then
      if(.NOT.core)then
      write(Lfnpr,99023)nam,nctr
      else
      write(Lfnpr,99024)nam,nctr
      endif
      elseif(.NOT.core)then
      write(Lfnpr,99023)nam,nctr,((labec(k,j),j=1,2),occec(k),k=1,jmax)
      else
      write(Lfnpr,99024)nam,nctr,((labec(k,j),j=1,2),occec(k),k=1,jmax)
      endif
1400  continue
      endif
      if(Jprint(4).LT.0)stop
      
      
      if(Jprint(12).NE.0)then
      title='Wiberg bond index matrix in the NAO basis:'
      call aout(BINDEX,Natoms,Natoms,Natoms,title,0,Natoms)
      do 1450 iat=1,Natoms
      BINDT(iat)=zero
      do 1420 jat=1,Natoms
      if(iat.NE.jat)BINDT(iat)=BINDT(iat)+BINDEX(jat,iat)
1420  continue
1450  continue
      title='Wiberg bond index, Totals by atom:'
      call aout(BINDT,Natoms,Natoms,1,title,0,1)
      
      
      title='Atom-atom overlap-weighted NAO bond order:'
      call aout(OVPOP,Natoms,Natoms,Natoms,title,0,Natoms)
      do 1500 iat=1,Natoms
      BINDT(iat)=zero
      do 1460 jat=1,Natoms
      if(iat.NE.jat)BINDT(iat)=BINDT(iat)+OVPOP(jat,iat)
1460  continue
1500  continue
      title(1:43)='Atom-atom overlap-weighted NAO bond order, '
      title(44:58)='Totals by atom:'
      call aout(BINDT,Natoms,Natoms,1,title,0,1)
      endif
      
      
      do 1600 i=1,Nbas
      Naoc(i)=Naoctr(i)
      Naoa(i)=Naol(i)
1600  continue
      return
      
99001 format(//,1x,'NATURAL POPULATIONS:  Natural atomic orbital occupan
     &cies ',/,1x,'                                                     
     &    ',/,1x,' NAO Atom  #  lang   Type(AO)    Occupancy      Energy
     &    ',/,1x,'------------------------------------------------------
     &---')
99002 format(//,1x,'NATURAL POPULATIONS:  Natural atomic orbital occupan
     &cies ',/,1x,'                                                     
     &    ',/,1x,' NAO Atom  #  lang   Type(AO)    Occupancy            
     &    ',/,1x,'-------------------------------------------           
     &   ')
99003 format(1x,i3,3x,a2,i3,2x,a1,a4,2x,a3,'(',i2,a1,')',4x,f8.5,4x,f10.
     &5)
99004 format(/,1x,'[',i3,' electrons found in the effective core potenti
     &al]')
99005 format(/,1x,'WARNING:  1 low occupancy (<',f6.4,'e) core orbital  
     &found ','on ',a2,i2)
99006 format(1x,'          1 low occupancy (<',f6.4,'e) core orbital  fo
     &und ','on ',a2,i2)
99007 format(/,1x,'WARNING:',i3,' low occupancy (<',f6.4,'e) core orbita
     &ls found',' on ',a2,i2)
99008 format(1x,'        ',i3,' low occupancy (<',f6.4,'e) core orbitals
     & found',' on ',a2,i2)
99009 format(/,1x,'WARNING:  Population inversion found on atom ',a2,i2)
99010 format(1x,'          Population inversion found on atom ',a2,i2)
99011 format(//,1x,'Summary of Natural Population Analysis:             
     &     ',/,1x,'                                                     
     &    ',/,1x,'                                      Natural Populati
     &on ',/,1x,'              Natural   ',47('-'),/,1x,3x,'Atom  #',5x,
     &'Charge',8x,'Core',6x,'Valence',4x,'Rydberg',6x,'Total',/,1x,71('-
     &'))
99012 format(1x,4x,a2,i3,2x,f9.5,4x,f9.5,3x,f9.5,2x,f9.5,3x,f9.5)
99013 format(1x,71('='),/,1x,'  * Total *',f9.5,4x,f9.5,3x,f9.5,2x,f9.5,
     &3x,f9.5)
99014 format(/1x,'Number of electrons is not an integer!  Please check y
     &ou data.'/)
99015 format(/1x,'WARNING: Number of electrons is not within 1.0D-5 of a
     &n',' integer.'/)
99016 format(/,1x,'                                Natural Population   
     &   ',/,1x,'-------------------------------------------------------
     &-')
99017 format(1x,'  Effective Core          ',f10.5)
99018 format(1x,'  Core                    ',f10.5,' (',f8.4,'% of ',i3,
     &')')
99019 format(1x,'  Valence                 ',f10.5,' (',f8.4,'% of ',i3,
     &')')
99020 format(1x,'  Natural Minimal Basis   ',f10.5,' (',f8.4,'% of ',i3,
     &')')
99021 format(1x,'  Natural Rydberg Basis   ',f10.5,' (',f8.4,'% of ',i3,
     &')',/,1x,'--------------------------------------------------------
     &')
99022 format(/1x,'   Atom  #          Natural Electron Configuration',/,
     &1x,76('-'))
99023 format(1x,4x,a2,i3,6x,6x,(13(i1,a1,'(',f5.2,')')))
99024 format(1x,4x,a2,i3,6x,'[core]',(13(i1,a1,'(',f5.2,')')))
99025 format(/1x,'Subroutine NAOANL could not find a ',a1,'-type ','core
     & orbital on atom ',a2,i2,'.',/,1x,'ICORE :',4I3,'     M :',i3,'   
     &  LA :',i3)
99026 format(/1x,'Subroutine NAOANL could not find a ',a1,'-type ','vale
     &nce orbital on atom ',a2,i2,'.',/,1x,'IVAL :',4I3,'     M :',i3,' 
     &    LA :',i3)
      end
C* :1 * 
      
