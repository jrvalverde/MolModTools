
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ciprm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ciprm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 44 "ciprm.web"
      subroutine ciprm
      implicit none
      double precision a0,a00,a01,a1,a10,a2,ad,anorm,anorm1,Aoi,as,Atmch
     &g,C,cimu0,cimu1,cjnu0,cjnu1,ckla0,ckla1,clis0
      double precision clis1,Cme,Cmo,d3,de,de3,delmax,dv,dvar,e,e1t,e2t,
     &e4dq,e4sdq,e4sdtq,e4t,ehf,Eig,Epair,equad4
      double precision esav,etot,eucl4,f,Fil801,four,gabs,gsqrt,Omi,one,
     &pmo,pt25,pt5,Pti,scc,sixtn,sum,sum1,sumd,suml
      double precision sumq,t,Tij,two,Valint,w0,w1,wd,ws,X,zero
      integer i,i1,ia,Ian,ib,Ibasd,Ibase,Ibfpad,ic,iccd,Icharg,Icheck,ic
     &n,Icon,Icount,id,ie,Ieval,Ifil,iflst
      integer ii,Iia,iii,ij,ik,il,im,In,inforb,intape,Intcnt,Ioab,Iop,Io
     &ut,iprint,Ipunch,Iq,iqbufr,iqproc,ir
      integer Ireset,irwibf,is,Ismode,Ispect,Istat,iter,Itotal,Iux,Ix,j,
     &j1,Ja,jb,jj,jjj,jq,k,k1,kk
      integer kkk,Kntt1,Kntt2,l,l1,la,Last,lenibf,Limint,ll,lll,lnforb,L
     &oab,lq,Lspect,m,Maxbuc,maxit,mci,method
      integer Mindx,mm,mm1,Mode,mu,Multip,Nae,naep,Natoms,nb1,nb2,Nbasis
     &,nbb,nbbb,Nbe,nbsq,nbss,Ne,nfile,nnae
      integer nnva,Noa,Noa2,Noa3,Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobva,N
     &obvb,Novaa,Novab,Novbb,Nrorb,Nrpext,Ntx,nu,nv
      integer Nva,Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3,Nwiib,Nwpi
      character*8 dat1,dat2,dat31,dat32,dat4,dat5,dat6
      character*8 dat71,dat72,dat73,dat81,dat82,dat83
      character*8 dat91,dat92,dat93,dat101,dat102,dat103
      character dat11,dat12,dat131,dat132,dat133
      character*8 dat14,dat15,dat16,dat171,dat172,dat18
      integer P,Q,R,S,Sindx
      integer Dbase,Dbasd,Dcount
      dimension Aoi(12,12,12,12),Omi(12,12,12,12),Cme(12,12,12,12)
      dimension X(4760),Ix(11264)
      dimension ir(12),e(12)
      dimension as(6,9),ws(6,9),ad(6,6,9,9),wd(6,6,9,9)
      dimension Pti(6,6,6,6)
      dimension Epair(6,6),Tij(6,6)
      dimension pmo(12,12)
      dimension Iia(2)
      dimension dv(47)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/memry/Omi,Cme,Pti,Epair,Tij,Fil801(7160)
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Ibfpad
      common/packed/P,Q,R,S,Valint,Ja
      common/io/In,Iout,Ipunch
      common/inttap/Icheck(33)
      common/cmo801/Cmo(3042),Eig(78)
      equivalence(X(1),Ix(1),Omi(1,1,1,1))
      equivalence(Aoi(1,1,1,1),Cme(1,1,1,1))
      equivalence(S,Sindx),(R,Mindx),(Valint,Iia(1))
      equivalence(Icount,Kntt1),(Dcount,Kntt2)
      data nfile/0/
      data four/4.D0/
      data sixtn/16.D0/
      data zero,pt25,one,two/0.D0,.25D0,1.D0,2.D0/
      data pt5/.5D0/
      data inforb,lnforb/45,14/
      data irwibf,lenibf/8,15/
      data dat1/'CME ERR:'/
      data dat2/' AD ERR:'/
      data dat31,dat32/' W(3,2,6',',1)     '/
      data dat4/' MP4(D) '/
      data dat5/' E3=    '/
      data dat6/' MP4(T) '/
      data dat71,dat72,dat73/' MP4(T) ','FROM AAA',' AAA=   '/
      data dat81,dat82,dat83/' MP4(T) ','FROM AAB',' AAB=   '/
      data dat91,dat92,dat93/' MP4(T) ','FROM ABB',' ABB=   '/
      data dat101,dat102,dat103/' MP4(T) ','FROM BBB',' BBB=   '/
      data dat11,dat12/'SUMD    ','SUML    '/
      data dat131,dat132,dat133/' MP4(T) ','SUM OF T','ERMS=   '/
      data dat14/' MP4(DQ)'/
      data dat15/' ANORM  '/
      data dat16/'MP4(SDQ)'/
      data dat171,dat172/'MP4(SDTQ',')       '/
      data dat18/' AD     '/
      
99001 format(///' ITERATION STEP #',i4/1x,130(1H*)//)
99002 format(i2,f20.0)
99003 format(//' SS3:')
99004 format(10D13.7)
99005 format(//' DS4:')
99006 format(//' SD5:')
99007 format(//' DD1:')
99008 format(//' SD4:')
99009 format(//' DD2:')
99010 format(//' DD3:')
99011 format(//' DS5:')
99012 format(' CYCLE',i3,5x,'ANORM=',d15.8,5x,'W0=',d15.8,5x,'W1=',d15.8
     &,5x,'DVAR=',d15.8/14x,'E1T=',d15.8,5x,'E2T=',d15.8,5x,'E4=',d15.8)
99013 format(' CORRECTED AS:')
99014 format(' CORRECTED AD:')
99015 format(' W0=',f20.8)
99016 format(' E(UCL4)=',d17.7)
99017 format(' THE MO DENSITY MATRIX '/)
99018 format(10I13)
      method=Iop(5)+1
      mci=Iop(6)
      iccd=Iop(7)
      iprint=Iop(33)
      
      call tread(inforb,Ispect,lnforb,1,lnforb,1,0)
      
      read(In,99002)maxit,delmax
      if(maxit.LE.0)maxit=30
      if(delmax.LE.1.D-10)delmax=1.D-8
      if(iccd.EQ.1)maxit=1
      nb2=Nbasis*2
      do 100 i=1,nb2
      do 50 j=1,nb2
      do 20 k=1,nb2
      do 10 l=1,nb2
      Aoi(l,k,j,i)=zero
10    continue
20    continue
50    continue
100   continue
      
      mm=0
      do 200 i=1,Nae
      mm=mm+1
      ir(mm)=i
200   continue
      nb1=Nbasis+1
      nbb=Nbasis+Nbe
      if(nbb.GE.nb1)then
      do 250 i=nb1,nbb
      mm=mm+1
      ir(mm)=i
250   continue
      endif
      if(Nae.NE.Nbasis)then
      naep=Nae+1
      do 300 i=naep,Nbasis
      mm=mm+1
      ir(mm)=i
300   continue
      endif
      nbbb=nb1+Nbe
      nb2=2*Nbasis
      do 400 i=nbbb,nb2
      mm=mm+1
      ir(mm)=i
400   continue
      
      nbss=Nbasis*Nbasis
      
      
      call tread(irwibf,Ismode,lenibf,1,lenibf,1,0)
      
      iqbufr=1
      iqproc=2
      Intcnt=0
      
      Ntx=1
      intape=Iux(2)
      call iwind(intape)
      call iread(intape,iqbufr,X)
      Ifil=1
500   call iwait(intape)
      iqbufr=iabs(iqbufr-2)+1
      iqproc=iabs(iqproc-2)+1
      Ibase=Ibasd(iqproc)
      Dbase=Dbasd(iqproc)
      call labscf(Ix(Ibase),iflst)
      if(iflst.EQ.0)then
      if(Ifil.EQ.(nfile+Ntx*Icon))then
      call iwind(intape)
      Ntx=Ntx+1
      intape=Iux(Ntx+1)
      call iwind(intape)
      endif
      call iread(intape,iqbufr,X)
      Ifil=Ifil+1
      endif
      
      
      if(Mode.NE.1)call lnk1e
      if(Kntt1.GT.0)then
      jq=Ireset(1)+Ibase
      lq=jq+(Kntt1-1)*Nwpi
      do 550 m=jq,lq,Nwpi
      Ja=Ix(m)
      Iia(1)=Ix(m+1)
      Iia(2)=Ix(m+2)
      call unpck4
      i=P
      j=Q
      k=R
      l=S
      Aoi(i,j,k,l)=Valint
      Aoi(j,i,k,l)=Valint
      Aoi(i,j,l,k)=Valint
      Aoi(j,i,l,k)=Valint
      Aoi(k,l,i,j)=Valint
      Aoi(k,l,j,i)=Valint
      Aoi(l,k,i,j)=Valint
      Aoi(l,k,j,i)=Valint
550   continue
      Intcnt=Intcnt+Kntt1
      endif
      
      if(Kntt2.GT.0)then
      lq=Ireset(2)+Ibase
      jq=lq-(Kntt2-1)*Nwpi
      do 600 m=jq,lq,Nwpi
      Ja=Ix(m)
      Iia(1)=Ix(m+1)
      Iia(2)=Ix(m+2)
      call unpck4
      Sindx=Sindx+1
      if(Sindx.EQ.1)goto 560
      if(Sindx.EQ.2)then
      
      i=P
      j=Q
      k=Q
      l=R
      elseif(Sindx.EQ.3)then
      
      i=P
      j=Q
      k=R
      l=Q
      elseif(Sindx.EQ.4)then
      
      i=P
      j=Q
      k=P
      l=Q
      elseif(Sindx.EQ.5)then
      
      i=P
      j=P
      k=Q
      l=R
      elseif(Sindx.EQ.6)then
      
      i=P
      j=Q
      k=R
      l=R
      elseif(Sindx.EQ.8)then
      
      i=P
      j=P
      k=P
      l=P
      
      elseif(Mindx.EQ.1)then
      
      i=P
      j=Q
      k=Q
      l=Q
      elseif(Mindx.EQ.2)then
      
      i=P
      j=P
      k=Q
      l=Q
      elseif(Mindx.EQ.3)then
      
      i=P
      j=P
      k=P
      l=Q
      else
      goto 560
      endif
      goto 580
      
560   i=P
      j=Q
      k=P
      l=R
      
580   Aoi(i,j,k,l)=Valint
      Aoi(j,i,k,l)=Valint
      Aoi(i,j,l,k)=Valint
      Aoi(j,i,l,k)=Valint
      Aoi(k,l,i,j)=Valint
      Aoi(k,l,j,i)=Valint
      Aoi(l,k,i,j)=Valint
      Aoi(l,k,j,i)=Valint
      
600   continue
      Intcnt=Intcnt+Kntt2
      endif
      if(iflst.LE.0)goto 500
      call iwind(intape)
      
      
      do 700 i=1,nb2
      do 650 j=1,nb2
      do 620 k=1,nb2
      do 610 l=1,nb2
      Omi(l,k,j,i)=zero
610   continue
620   continue
650   continue
700   continue
      
      mm1=Nbasis*(Nbasis+1)+1
      nb2=2*Nbasis
      nbsq=Nbasis*nb2
      
      call tread(Ispect,Cmo,Lspect,1,Lspect,1,0)
      
      call tread(Ieval,Eig,nb2,1,nb2,1,0)
      
      do 800 i=1,Nbasis
      i1=i+Nbasis
      ii=(i-1)*Nbasis
      iii=ii+nbss
      do 750 j=1,Nbasis
      j1=j+Nbasis
      jj=(j-1)*Nbasis
      jjj=jj+nbss
      do 740 k=1,Nbasis
      k1=k+Nbasis
      kk=(k-1)*Nbasis
      kkk=kk+nbss
      do 720 l=1,Nbasis
      l1=l+Nbasis
      ll=(l-1)*Nbasis
      lll=ll+nbss
      
      a0=zero
      a01=zero
      a10=zero
      a1=zero
      
      do 710 mu=1,Nbasis
      cimu0=Cmo(ii+mu)
      cimu1=Cmo(iii+mu)
      do 706 nu=1,Nbasis
      cjnu0=Cmo(jj+nu)
      cjnu1=Cmo(jjj+nu)
      do 704 la=1,Nbasis
      ckla0=Cmo(kk+la)
      ckla1=Cmo(kkk+la)
      do 702 is=1,Nbasis
      clis0=Cmo(ll+is)
      clis1=Cmo(lll+is)
      
      Valint=Aoi(mu,la,nu,is)
      a0=a0+cimu0*cjnu0*ckla0*clis0*Valint
      a01=a01+cimu0*cjnu1*ckla0*clis1*Valint
      a10=a10+cimu1*cjnu0*ckla1*clis0*Valint
      a1=a1+cimu1*cjnu1*ckla1*clis1*Valint
702   continue
704   continue
706   continue
710   continue
      
      Omi(i,j,k,l)=a0
      Omi(i,j1,k,l1)=a01
      Omi(i1,j,k1,l)=a10
      Omi(i1,j1,k1,l1)=a1
720   continue
740   continue
750   continue
800   continue
      
      naep=Nae+1
      m=0
      do 900 i=1,Ne
      if(i.EQ.naep)m=nbss
      do 850 is=1,Nbasis
      do 820 la=1,Nbasis
      do 810 nu=1,Nbasis
      a0=zero
      do 805 mu=1,Nbasis
      a0=a0+Cmo(m+mu)*Aoi(mu,nu,la,is)
805   continue
      Pti(i,nu,la,is)=a0
810   continue
820   continue
850   continue
      if(iprint.NE.0)then
      write(Iout,99002)i
      write(Iout,99004)(((Pti(i,nu,la,is),is=1,la),la=1,Nbasis),nu=1,Nba
     &sis)
      endif
      m=m+Nbasis
900   continue
      
      
      do 1000 i=1,nb2
      ii=ir(i)
      do 950 j=1,nb2
      jj=ir(j)
      do 920 k=1,nb2
      kk=ir(k)
      do 910 l=1,nb2
      ll=ir(l)
      Cme(i,j,k,l)=Omi(ii,jj,kk,ll)-Omi(ii,jj,ll,kk)
      if(Cme(i,j,k,l).GT.one)write(6,*)dat1,i,j,k,l,Cme(i,j,k,l),Omi(ii,
     &jj,kk,ll),Omi(ii,jj,ll,kk)
910   continue
920   continue
950   continue
1000  continue
      
      do 1100 i=1,nb2
      ii=ir(i)
      e(i)=Eig(ii)
1100  continue
      
      
      call tread(1,dv,47,1,47,1,0)
      ehf=(dv(43))
      nv=nb2-Ne
      do 1200 j=1,nv
      do 1150 i=1,Ne
      as(i,j)=zero
1150  continue
1200  continue
      
      do 1300 ib=1,nv
      do 1250 ia=1,nv
      do 1220 ij=1,Ne
      do 1210 ii=1,Ne
      ad(ii,ij,ia,ib)=Cme(ii,ij,ia+Ne,ib+Ne)/(e(ii)+e(ij)-e(ia+Ne)-e(ib+
     &Ne))
      if(ad(ii,jj,ia,ib).GT.one)write(6,*)dat2,ii,ij,ia,ib,ad(ii,ij,ia,i
     &b),Cme(ii,ij,ia+Ne,ib+Ne)
1210  continue
1220  continue
1250  continue
1300  continue
      a00=one
      
      esav=ehf
      do 2700 iter=1,maxit
      if(iprint.NE.0)write(Iout,99001)iter
      
      w0=zero
      do 1350 ij=1,Ne
      do 1320 ii=1,Ne
      Tij(ii,ij)=zero
      Epair(ii,ij)=zero
      do 1310 ib=1,nv
      do 1305 ia=1,nv
      Tij(ii,ij)=Tij(ii,ij)+ad(ii,ij,ia,ib)**2
      Epair(ii,ij)=Epair(ii,ij)+Cme(ii,ij,ia+Ne,ib+Ne)*ad(ii,ij,ia,ib)
1305  continue
1310  continue
      w0=w0+Epair(ii,ij)
1320  continue
1350  continue
      w0=w0*pt25
      sumq=zero
      do 1400 ij=1,Ne
      do 1360 ii=1,ij
      sumq=sumq+Tij(ii,ij)*Epair(ii,ij)
1360  continue
1400  continue
      sumq=sumq/8.D0
      if(iter.EQ.1)then
      
      do 1420 i=1,nv
      do 1410 j=1,nv
      do 1405 k=1,Ne
      do 1402 l=1,Ne
      wd(l,k,j,i)=zero
1402  continue
1405  continue
1410  continue
1420  continue
      write(6,*)dat31,dat32,wd(3,2,6,1)
      do 1440 i=1,nv
      do 1430 j=1,Ne
      ws(j,i)=zero
1430  continue
1440  continue
      else
      
      if(iprint.NE.0)write(Iout,99003)
      do 1460 ia=1,nv
      do 1450 ii=1,Ne
      a0=zero
      do 1445 ib=1,nv
      do 1442 ij=1,Ne
      a0=a0+Cme(ij,ia+Ne,ii,ib+Ne)*as(ij,ib)
1442  continue
1445  continue
      ws(ii,ia)=-a0
1450  continue
1460  continue
      call writes(ws)
      
      if(iprint.NE.0)write(Iout,99005)
      do 1480 ib=1,nv
      do 1470 ia=1,nv
      do 1465 ij=1,Ne
      do 1464 ii=1,Ne
      a0=zero
      do 1462 ic=1,nv
      icn=ic+Ne
      a0=a0+Cme(ia+Ne,ib+Ne,icn,ij)*as(ii,ic)-Cme(ia+Ne,ib+Ne,icn,ii)*as
     &(ij,ic)
1462  continue
      wd(ii,ij,ia,ib)=a0
1464  continue
1465  continue
1470  continue
1480  continue
      write(6,*)dat31,dat32,wd(3,2,6,1)
      call writed(wd)
      endif
      
      if(iprint.NE.0)write(Iout,99006)
      do 1550 ia=1,nv
      do 1500 ii=1,Ne
      a0=zero
      do 1490 ib=1,nv
      do 1485 ik=1,Ne
      do 1482 ij=1,Ne
      a0=a0+Cme(ij,ik,ii,ib+Ne)*ad(ij,ik,ia,ib)
1482  continue
1485  continue
1490  continue
      ws(ii,ia)=ws(ii,ia)-pt5*a0
1500  continue
1550  continue
      call writes(ws)
      if(iter.NE.1)then
      
      if(iprint.NE.0)write(Iout,99011)
      do 1580 ib=1,nv
      do 1560 ia=1,nv
      do 1555 ij=1,Ne
      do 1554 ii=1,Ne
      a0=zero
      do 1552 ik=1,Ne
      a0=a0-Cme(ik,ib+Ne,ii,ij)*as(ik,ia)+Cme(ik,ia+Ne,ii,ij)*as(ik,ib)
1552  continue
      wd(ii,ij,ia,ib)=wd(ii,ij,ia,ib)+a0
1554  continue
1555  continue
1560  continue
1580  continue
      write(6,*)dat31,dat32,wd(3,2,6,1)
      call writed(wd)
      endif
      
      if(iprint.NE.0)write(Iout,99007)
      do 1650 ib=1,nv
      do 1600 ia=1,nv
      do 1590 ij=1,Ne
      do 1585 ii=1,Ne
      a0=zero
      do 1584 id=1,nv
      do 1582 ic=1,nv
      a0=a0+Cme(ia+Ne,ib+Ne,ic+Ne,id+Ne)*ad(ii,ij,ic,id)
1582  continue
1584  continue
      wd(ii,ij,ia,ib)=wd(ii,ij,ia,ib)+pt5*a0
1585  continue
1590  continue
1600  continue
1650  continue
      write(6,*)dat31,dat32,wd(3,2,6,1)
      call writed(wd)
      
      if(iprint.NE.0)write(Iout,99008)
      do 1700 ia=1,nv
      do 1680 ii=1,Ne
      a0=zero
      do 1660 ic=1,nv
      do 1655 ib=1,nv
      do 1652 ij=1,Ne
      a0=a0+Cme(ij,ia+Ne,ib+Ne,ic+Ne)*ad(ii,ij,ib,ic)
1652  continue
1655  continue
1660  continue
      ws(ii,ia)=ws(ii,ia)-pt5*a0
1680  continue
1700  continue
      call writes(ws)
      
      if(iprint.NE.0)write(Iout,99009)
      do 1750 ib=1,nv
      do 1720 ia=1,nv
      do 1710 ij=1,Ne
      do 1705 ii=1,Ne
      a0=zero
      do 1704 il=1,Ne
      do 1702 ik=1,Ne
      a0=a0+Cme(ik,il,ii,ij)*ad(ik,il,ia,ib)
1702  continue
1704  continue
      wd(ii,ij,ia,ib)=wd(ii,ij,ia,ib)+pt5*a0
1705  continue
1710  continue
1720  continue
1750  continue
      write(6,*)dat31,dat32,wd(3,2,6,1)
      call writed(wd)
      
      if(iprint.NE.0)write(Iout,99010)
      de3=zero
      eucl4=zero
      do 1800 ib=1,nv
      do 1780 ia=1,nv
      do 1760 ij=1,Ne
      do 1755 ii=1,Ne
      a0=zero
      do 1754 ic=1,nv
      do 1752 ik=1,Ne
      a0=a0+Cme(ik,ib+Ne,ij,ic+Ne)*ad(ii,ik,ia,ic)+Cme(ik,ia+Ne,ij,ic+Ne
     &)*ad(ii,ik,ic,ib)+Cme(ik,ib+Ne,ii,ic+Ne)*ad(ik,ij,ia,ic)+Cme(ik,ia
     &+Ne,ii,ic+Ne)*ad(ik,ij,ic,ib)
1752  continue
1754  continue
      wd(ii,ij,ia,ib)=wd(ii,ij,ia,ib)-a0
      eucl4=eucl4+wd(ii,ij,ia,ib)**2/(e(ii)+e(ij)-e(ia+Ne)-e(ib+Ne))
      de3=de3+wd(ii,ij,ia,ib)*ad(ii,ij,ia,ib)
1755  continue
1760  continue
1780  continue
1800  continue
      de3=de3*pt25
      eucl4=eucl4*pt25
      write(6,*)dat4,eucl4
      write(6,*)dat5,de3
      write(6,*)dat31,dat32,wd(3,2,6,1)
      call writed(wd)
      if(iccd.NE.0)then
      equad4=zero
      do 1840 ib=1,nv
      do 1820 ia=1,nv
      do 1815 ij=1,Ne
      do 1810 ii=1,Ne
      a0=zero
      do 1808 id=1,nv
      do 1806 ic=1,nv
      do 1804 il=1,Ne
      do 1802 ik=1,Ne
      a0=a0+four*ad(ii,ik,ia,ic)*ad(ij,il,ib,id)*Cme(ik,il,ic+Ne,id+Ne)
      a0=a0+four*ad(ii,ik,ib,id)*ad(ij,il,ia,ic)*Cme(ik,il,ic+Ne,id+Ne)
      a0=a0-two*ad(ii,ik,ia,ib)*ad(ij,il,ic,id)*Cme(ik,il,ic+Ne,id+Ne)
      a0=a0-two*ad(ii,ik,ic,id)*ad(ij,il,ia,ib)*Cme(ik,il,ic+Ne,id+Ne)
      a0=a0-two*ad(ii,ij,ia,ic)*ad(ik,il,ib,id)*Cme(ik,il,ic+Ne,id+Ne)
      a0=a0-two*ad(ii,ij,ib,id)*ad(ik,il,ia,ic)*Cme(ik,il,ic+Ne,id+Ne)
      a0=a0+ad(ii,ij,ic,id)*ad(ik,il,ia,ib)*Cme(ik,il,ic+Ne,id+Ne)
1802  continue
1804  continue
1806  continue
1808  continue
      wd(ii,ij,ia,ib)=wd(ii,ij,ia,ib)+a0*pt25
      equad4=equad4+wd(ii,ij,ia,ib)*ad(ii,ij,ia,ib)
1810  continue
1815  continue
1820  continue
1840  continue
      write(6,*)dat31,dat32,wd(3,2,6,1)
      equad4=equad4*pt25
      equad4=equad4-de3
      write(Iout,99019)equad4
      
99019 format(1x,'UMP4(Q)=',d18.9)
      
      call writed(wd)
      e4t=zero
      do 1880 ii=1,Ne
      do 1860 ij=1,Ne
      do 1855 ik=1,Ne
      do 1850 ia=1,nv
      do 1848 ib=1,nv
      do 1846 ic=1,nv
      a0=zero
      do 1842 ie=1,nv
      a0=a0-Cme(ik,ie+Ne,ib+Ne,ic+Ne)*ad(ii,ij,ia,ie)+Cme(ik,ie+Ne,ia+Ne
     &,ic+Ne)*ad(ii,ij,ib,ie)+Cme(ik,ie+Ne,ib+Ne,ia+Ne)*ad(ii,ij,ic,ie)+
     &Cme(ij,ie+Ne,ib+Ne,ic+Ne)*ad(ii,ik,ia,ie)-Cme(ij,ie+Ne,ia+Ne,ic+Ne
     &)*ad(ii,ik,ib,ie)-Cme(ij,ie+Ne,ib+Ne,ia+Ne)*ad(ii,ik,ic,ie)+Cme(ii
     &,ie+Ne,ib+Ne,ic+Ne)*ad(ik,ij,ia,ie)-Cme(ii,ie+Ne,ia+Ne,ic+Ne)*ad(i
     &k,ij,ib,ie)-Cme(ii,ie+Ne,ib+Ne,ia+Ne)*ad(ik,ij,ic,ie)
1842  continue
      do 1844 im=1,Ne
      a0=a0-Cme(ij,ik,im,ic+Ne)*ad(ii,im,ia,ib)+Cme(ij,ik,im,ia+Ne)*ad(i
     &i,im,ic,ib)+Cme(ij,ik,im,ib+Ne)*ad(ii,im,ia,ic)+Cme(ii,ik,im,ic+Ne
     &)*ad(ij,im,ia,ib)-Cme(ii,ik,im,ia+Ne)*ad(ij,im,ic,ib)-Cme(ii,ik,im
     &,ib+Ne)*ad(ij,im,ia,ic)+Cme(ij,ii,im,ic+Ne)*ad(ik,im,ia,ib)-Cme(ij
     &,ii,im,ia+Ne)*ad(ik,im,ic,ib)-Cme(ij,ii,im,ib+Ne)*ad(ik,im,ia,ic)
1844  continue
      e4t=e4t+a0*a0/(e(ii)+e(ij)+e(ik)-e(ia+Ne)-e(ib+Ne)-e(ic+Ne))
1846  continue
1848  continue
1850  continue
1855  continue
1860  continue
1880  continue
      e4t=e4t/36.0
      write(6,*)dat6,e4t
      
      
      sum=zero
      sumd=zero
      suml=zero
      sum1=zero
      
      do 1920 ii=1,Nae
      do 1900 ij=1,Nae
      do 1895 ik=1,Nae
      do 1890 ia=1,Nva
      do 1888 ib=1,Nva
      do 1886 ic=1,Nva
      a0=zero
      a2=zero
      do 1882 ie=1,nv
      a0=a0-Cme(ik,ie+Ne,ib+Ne,ic+Ne)*ad(ii,ij,ia,ie)+Cme(ik,ie+Ne,ia+Ne
     &,ic+Ne)*ad(ii,ij,ib,ie)+Cme(ik,ie+Ne,ib+Ne,ia+Ne)*ad(ii,ij,ic,ie)+
     &Cme(ij,ie+Ne,ib+Ne,ic+Ne)*ad(ii,ik,ia,ie)-Cme(ij,ie+Ne,ia+Ne,ic+Ne
     &)*ad(ii,ik,ib,ie)-Cme(ij,ie+Ne,ib+Ne,ia+Ne)*ad(ii,ik,ic,ie)+Cme(ii
     &,ie+Ne,ib+Ne,ic+Ne)*ad(ik,ij,ia,ie)-Cme(ii,ie+Ne,ia+Ne,ic+Ne)*ad(i
     &k,ij,ib,ie)-Cme(ii,ie+Ne,ib+Ne,ia+Ne)*ad(ik,ij,ic,ie)
1882  continue
      sumd=sumd+a0**2
      do 1884 im=1,Ne
      a2=a2-Cme(ij,ik,im,ic+Ne)*ad(ii,im,ia,ib)+Cme(ij,ik,im,ia+Ne)*ad(i
     &i,im,ic,ib)+Cme(ij,ik,im,ib+Ne)*ad(ii,im,ia,ic)+Cme(ii,ik,im,ic+Ne
     &)*ad(ij,im,ia,ib)-Cme(ii,ik,im,ia+Ne)*ad(ij,im,ic,ib)-Cme(ii,ik,im
     &,ib+Ne)*ad(ij,im,ia,ic)+Cme(ij,ii,im,ic+Ne)*ad(ik,im,ia,ib)-Cme(ij
     &,ii,im,ia+Ne)*ad(ik,im,ic,ib)-Cme(ij,ii,im,ib+Ne)*ad(ik,im,ia,ic)
1884  continue
      suml=suml+a2**2
      a0=a0+a2
      sum=sum+a0*a0/(e(ii)+e(ij)+e(ik)-e(ia+Ne)-e(ib+Ne)-e(ic+Ne))
1886  continue
1888  continue
1890  continue
1895  continue
1900  continue
1920  continue
      sum=sum/36.0
      sum1=sum1+sum
      write(6,*)dat71,dat72,dat73,sum,dat11,sumd,dat12,suml
      
      sum=zero
      sumd=zero
      suml=zero
      do 1960 ii=1,Nae
      do 1940 ij=1,Nae
      nnae=Nae+1
      do 1935 ik=nnae,Ne
      do 1930 ia=1,Nva
      do 1928 ib=1,Nva
      nnva=Nva+1
      do 1926 ic=nnva,nv
      a0=zero
      a2=zero
      do 1922 ie=1,nv
      a0=a0-Cme(ik,ie+Ne,ib+Ne,ic+Ne)*ad(ii,ij,ia,ie)+Cme(ik,ie+Ne,ia+Ne
     &,ic+Ne)*ad(ii,ij,ib,ie)+Cme(ik,ie+Ne,ib+Ne,ia+Ne)*ad(ii,ij,ic,ie)+
     &Cme(ij,ie+Ne,ib+Ne,ic+Ne)*ad(ii,ik,ia,ie)-Cme(ij,ie+Ne,ia+Ne,ic+Ne
     &)*ad(ii,ik,ib,ie)-Cme(ij,ie+Ne,ib+Ne,ia+Ne)*ad(ii,ik,ic,ie)+Cme(ii
     &,ie+Ne,ib+Ne,ic+Ne)*ad(ik,ij,ia,ie)-Cme(ii,ie+Ne,ia+Ne,ic+Ne)*ad(i
     &k,ij,ib,ie)-Cme(ii,ie+Ne,ib+Ne,ia+Ne)*ad(ik,ij,ic,ie)
1922  continue
      sumd=sumd+a0**2
      do 1924 im=1,Ne
      a2=a2-Cme(ij,ik,im,ic+Ne)*ad(ii,im,ia,ib)+Cme(ij,ik,im,ia+Ne)*ad(i
     &i,im,ic,ib)+Cme(ij,ik,im,ib+Ne)*ad(ii,im,ia,ic)+Cme(ii,ik,im,ic+Ne
     &)*ad(ij,im,ia,ib)-Cme(ii,ik,im,ia+Ne)*ad(ij,im,ic,ib)-Cme(ii,ik,im
     &,ib+Ne)*ad(ij,im,ia,ic)+Cme(ij,ii,im,ic+Ne)*ad(ik,im,ia,ib)-Cme(ij
     &,ii,im,ia+Ne)*ad(ik,im,ic,ib)-Cme(ij,ii,im,ib+Ne)*ad(ik,im,ia,ic)
1924  continue
      suml=suml+a2**2
      a0=a0+a2
      sum=sum+a0*a0/(e(ii)+e(ij)+e(ik)-e(ia+Ne)-e(ib+Ne)-e(ic+Ne))
1926  continue
1928  continue
1930  continue
1935  continue
1940  continue
1960  continue
      sum=sum/4.0
      sum1=sum1+sum
      write(6,*)dat81,dat82,dat83,sum,dat11,sumd,dat12,suml
      
      sum=zero
      sumd=zero
      suml=zero
      nnae=Nae+1
      nnva=Nva+1
      do 2000 ii=1,Nae
      do 1980 ij=nnae,Ne
      do 1975 ik=nnae,Ne
      do 1970 ia=1,Nva
      do 1968 ib=nnva,nv
      do 1966 ic=nnva,nv
      a0=zero
      a2=zero
      do 1962 ie=1,nv
      a0=a0-Cme(ik,ie+Ne,ib+Ne,ic+Ne)*ad(ii,ij,ia,ie)+Cme(ik,ie+Ne,ia+Ne
     &,ic+Ne)*ad(ii,ij,ib,ie)+Cme(ik,ie+Ne,ib+Ne,ia+Ne)*ad(ii,ij,ic,ie)+
     &Cme(ij,ie+Ne,ib+Ne,ic+Ne)*ad(ii,ik,ia,ie)-Cme(ij,ie+Ne,ia+Ne,ic+Ne
     &)*ad(ii,ik,ib,ie)-Cme(ij,ie+Ne,ib+Ne,ia+Ne)*ad(ii,ik,ic,ie)+Cme(ii
     &,ie+Ne,ib+Ne,ic+Ne)*ad(ik,ij,ia,ie)-Cme(ii,ie+Ne,ia+Ne,ic+Ne)*ad(i
     &k,ij,ib,ie)-Cme(ii,ie+Ne,ib+Ne,ia+Ne)*ad(ik,ij,ic,ie)
1962  continue
      sumd=sumd+a0**2
      do 1964 im=1,Ne
      a2=a2-Cme(ij,ik,im,ic+Ne)*ad(ii,im,ia,ib)+Cme(ij,ik,im,ia+Ne)*ad(i
     &i,im,ic,ib)+Cme(ij,ik,im,ib+Ne)*ad(ii,im,ia,ic)+Cme(ii,ik,im,ic+Ne
     &)*ad(ij,im,ia,ib)-Cme(ii,ik,im,ia+Ne)*ad(ij,im,ic,ib)-Cme(ii,ik,im
     &,ib+Ne)*ad(ij,im,ia,ic)+Cme(ij,ii,im,ic+Ne)*ad(ik,im,ia,ib)-Cme(ij
     &,ii,im,ia+Ne)*ad(ik,im,ic,ib)-Cme(ij,ii,im,ib+Ne)*ad(ik,im,ia,ic)
1964  continue
      suml=suml+a2**2
      a0=a0+a2
      sum=sum+a0*a0/(e(ii)+e(ij)+e(ik)-e(ia+Ne)-e(ib+Ne)-e(ic+Ne))
1966  continue
1968  continue
1970  continue
1975  continue
1980  continue
2000  continue
      sum=sum/4.0
      sum1=sum1+sum
      write(6,*)dat91,dat92,dat93,sum,dat11,sumd,dat12,suml
      
      sum=zero
      sumd=zero
      suml=zero
      nnae=Nae+1
      nnva=Nva+1
      do 2040 ii=nnae,Ne
      do 2020 ij=nnae,Ne
      do 2015 ik=nnae,Ne
      do 2010 ia=nnva,nv
      do 2008 ib=nnva,nv
      do 2006 ic=nnva,nv
      a0=zero
      a2=zero
      do 2002 ie=1,nv
      a0=a0-Cme(ik,ie+Ne,ib+Ne,ic+Ne)*ad(ii,ij,ia,ie)+Cme(ik,ie+Ne,ia+Ne
     &,ic+Ne)*ad(ii,ij,ib,ie)+Cme(ik,ie+Ne,ib+Ne,ia+Ne)*ad(ii,ij,ic,ie)+
     &Cme(ij,ie+Ne,ib+Ne,ic+Ne)*ad(ii,ik,ia,ie)-Cme(ij,ie+Ne,ia+Ne,ic+Ne
     &)*ad(ii,ik,ib,ie)-Cme(ij,ie+Ne,ib+Ne,ia+Ne)*ad(ii,ik,ic,ie)+Cme(ii
     &,ie+Ne,ib+Ne,ic+Ne)*ad(ik,ij,ia,ie)-Cme(ii,ie+Ne,ia+Ne,ic+Ne)*ad(i
     &k,ij,ib,ie)-Cme(ii,ie+Ne,ib+Ne,ia+Ne)*ad(ik,ij,ic,ie)
2002  continue
      sumd=sumd+a0**2
      do 2004 im=1,Ne
      a2=a2-Cme(ij,ik,im,ic+Ne)*ad(ii,im,ia,ib)+Cme(ij,ik,im,ia+Ne)*ad(i
     &i,im,ic,ib)+Cme(ij,ik,im,ib+Ne)*ad(ii,im,ia,ic)+Cme(ii,ik,im,ic+Ne
     &)*ad(ij,im,ia,ib)-Cme(ii,ik,im,ia+Ne)*ad(ij,im,ic,ib)-Cme(ii,ik,im
     &,ib+Ne)*ad(ij,im,ia,ic)+Cme(ij,ii,im,ic+Ne)*ad(ik,im,ia,ib)-Cme(ij
     &,ii,im,ia+Ne)*ad(ik,im,ic,ib)-Cme(ij,ii,im,ib+Ne)*ad(ik,im,ia,ic)
2004  continue
      suml=suml+a2**2
      a0=a0+a2
      sum=sum+a0*a0/(e(ii)+e(ij)+e(ik)-e(ia+Ne)-e(ib+Ne)-e(ic+Ne))
2006  continue
2008  continue
2010  continue
2015  continue
2020  continue
2040  continue
      sum=sum/36.0
      sum1=sum1+sum
      write(6,*)dat101,dat102,dat103,sum,dat11,sumd,dat12,suml
      write(6,*)dat131,dat132,dat133,sum1
      endif
      
      
      
      anorm=zero
      d3=zero
      f=zero
      do 2100 ib=1,nv
      do 2060 ia=1,nv
      do 2050 ij=1,Ne
      do 2045 ii=1,Ne
      anorm=anorm+ad(ii,ij,ia,ib)**2
      d3=d3+wd(ii,ij,ia,ib)*ad(ii,ij,ia,ib)
      f=f-ad(ii,ij,ia,ib)**2*(e(ii)+e(ij)-e(ia+Ne)-e(ib+Ne))
      wd(ii,ij,ia,ib)=wd(ii,ij,ia,ib)+Cme(ii,ij,ia+Ne,ib+Ne)*a00
2045  continue
2050  continue
2060  continue
2100  continue
      write(6,*)dat31,dat32,wd(3,2,6,1)
      anorm=anorm*pt25
      e4dq=equad4+eucl4
      write(6,*)dat14,e4dq
      d3=d3*pt25
      f=f*pt25
      a0=zero
      do 2150 ia=1,nv
      do 2120 ii=1,Ne
      d3=d3+ws(ii,ia)*as(ii,ia)
      a0=a0+ws(ii,ia)**2/(e(ii)-e(ia+Ne))
      f=f-as(ii,ia)**2*(e(ii)-e(ia+Ne))
      anorm=anorm+as(ii,ia)**2
2120  continue
2150  continue
      anorm=anorm+a00**2
      write(6,*)dat15,anorm
      f=f+d3+two*a00*w0
      dvar=f/anorm
      if(mci.EQ.0)eucl4=eucl4+a0
      e4sdq=equad4+eucl4
      write(6,*)dat16,e4sdq
      e4sdtq=e4sdq+e4t
      write(6,*)dat171,dat172,e4sdtq
      
      if(method.EQ.2)then
      de=0
      elseif(method.EQ.3)then
      de=w0*two/Ne
      elseif(method.EQ.4.OR.method.EQ.7)then
      de=0
      elseif(method.EQ.5.OR.method.EQ.6)then
      
      de=dvar
      else
      de=w0
      endif
      
      if(method.EQ.6)a00=w0/dvar
      do 2200 ib=1,nv
      do 2180 ia=1,nv
      do 2160 ij=1,Ne
      do 2155 ii=1,Ne
      if(method.EQ.4)de=Epair(ii,ij)/two
      if(method.EQ.7)de=Tij(ii,ij)*sumq
      ad(ii,ij,ia,ib)=wd(ii,ij,ia,ib)/(de+e(ii)+e(ij)-e(ia+Ne)-e(ib+Ne))
      if(gabs(ad(ii,ij,ia,ib)).GT.one)write(6,*)dat18,ii,ij,ia,ib,ad(ii,
     &ij,ia,ib),wd(ii,ij,ia,ib)
2155  continue
2160  continue
2180  continue
2200  continue
      if(iprint.NE.0)write(Iout,99013)
      call writed(ad)
      
      do 2250 ia=1,nv
      do 2220 ii=1,Ne
      as(ii,ia)=ws(ii,ia)/(de+e(ii)-e(ia+Ne))
2220  continue
2250  continue
      if(iprint.NE.0)write(Iout,99014)
      call writes(as)
      w1=zero
      anorm1=zero
      write(6,*)anorm1
      do 2300 ib=1,nv
      do 2280 ia=1,nv
      do 2260 ij=1,Ne
      do 2255 ii=1,Ne
      anorm1=anorm1+ad(ii,ij,ia,ib)**2
      w1=w1+Cme(ii,ij,ia+Ne,ib+Ne)*ad(ii,ij,ia,ib)
2255  continue
2260  continue
2280  continue
2300  continue
      write(6,*)anorm1
      w1=w1*pt25
      anorm1=anorm1*pt25
      e1t=ehf+w1
      e2t=ehf+dvar
      if(mci.NE.0)then
      do 2320 ii=1,Ne
      do 2310 ia=1,nv
      as(ii,ia)=zero
      ws(ii,ia)=zero
2310  continue
2320  continue
      endif
      etot=e1t
      if(method.EQ.1.OR.method.EQ.4.OR.method.EQ.5)etot=e2t
      do 2350 ia=1,nv
      do 2340 ii=1,Ne
      anorm1=anorm1+as(ii,ia)**2
2340  continue
2350  continue
      write(6,*)anorm1
      anorm1=anorm1+a00**2
      write(6,*)anorm1
      anorm=gsqrt(anorm1)
      write(Iout,99012)iter,anorm,w0,w1,dvar,e1t,e2t,eucl4
      scc=zero
      
      do 2400 ib=1,nv
      do 2380 ia=1,nv
      if(ia.NE.ib)then
      do 2365 ij=1,Ne
      do 2360 ii=1,Ne
      if(ij.NE.ii)then
      t=Cme(ii,ij,ia+Ne,ib+Ne)*ad(ii,ij,ia,ib)
      do 2358 id=1,nv
      if(id.NE.ia.AND.id.NE.ib)then
      do 2356 ic=1,nv
      if(ic.NE.id.AND.ic.NE.ia.AND.ic.NE.ib)then
      do 2354 il=1,Ne
      if(il.NE.ij.AND.il.NE.ii)then
      do 2352 ik=1,Ne
      if(ik.NE.ii.AND.ik.NE.ij.AND.ik.NE.il)scc=scc+t*ad(ik,il,ic,id)**2
2352  continue
      endif
2354  continue
      endif
2356  continue
      endif
2358  continue
      endif
2360  continue
2365  continue
      endif
2380  continue
2400  continue
      scc=scc/sixtn
      write(Iout,99020)scc
      
99020 format('  SCC',f20.10)
      
      do 2450 ii=1,Ne
      do 2440 ij=ii,Ne
      pmo(ii,ij)=zero
      if(ii.EQ.ij)pmo(ii,ij)=anorm1
      do 2410 ia=1,nv
      pmo(ii,ij)=pmo(ii,ij)-as(ii,ia)*as(ij,ia)
2410  continue
      do 2420 ik=1,Ne
      do 2415 ia=1,nv
      do 2412 ib=1,nv
      pmo(ii,ij)=pmo(ii,ij)-pt5*ad(ii,ik,ia,ib)*ad(ij,ik,ia,ib)
2412  continue
2415  continue
2420  continue
2440  continue
2450  continue
      do 2500 ia=1,nv
      do 2480 ib=ia,nv
      Ja=ia+Ne
      jb=ib+Ne
      pmo(Ja,jb)=zero
      do 2460 ii=1,Ne
      pmo(Ja,jb)=pmo(Ja,jb)+as(ii,ia)*as(ii,ib)
2460  continue
      do 2470 ii=1,Ne
      do 2465 ij=1,Ne
      do 2462 ic=1,nv
      pmo(Ja,jb)=pmo(Ja,jb)+pt5*ad(ii,ij,ia,ic)*ad(ii,ij,ib,ic)
2462  continue
2465  continue
2470  continue
2480  continue
2500  continue
      do 2550 ii=1,Ne
      do 2520 ia=1,nv
      Ja=Ne+ia
      pmo(ii,Ja)=as(ii,ia)*a00
      do 2510 ij=1,Ne
      do 2505 ib=1,nv
      pmo(ii,Ja)=pmo(ii,Ja)+ad(ii,ij,ia,ib)*as(ij,ib)
2505  continue
2510  continue
2520  continue
2550  continue
      do 2600 ii=1,nb2
      do 2560 ij=1,nb2
      pmo(ii,ij)=pmo(ii,ij)/anorm1
2560  continue
2600  continue
      if(gabs(esav-etot).LE.delmax)goto 2800
      esav=etot
2700  continue
      
2800  return
      
      end
C* :1 * 
      
