
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dlpint"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dlpint.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 37 "dlpint.web"
      subroutine dlpint(JUMP)
      
      
      implicit none
      double precision Aiab,Atmchg,Biab,C,C1,C2,C3,C4,Ca,Cb,Cc,Cd,Clp,co
     &re,crit,d1,D12,D12c,Dabx,Daby
      double precision Dabz,Ddvaa,Ddvab,Ddvac,Ddvbb,Ddvbc,Ddvcc,Dva,Dvb,
     &Dvc,E1xx,Epio2,Exx,F100,F1old,f1xyz,f20,F20i,F42,F6i
      double precision Fillip,Fnumb1,Fnumb2,Fnumb3,Fnumb4,Fnumb5,Fnumb6,
     &Fnumb7,Four,gatan,gfloat,gsqrt,Half,One,Onept5,Pcx,Pcy,Pcz,pi,pi3h
     &af
      double precision pt5,rootpi,Ten,Three,Two,twopi,vlp,X,Xa,Xb,Xc,Xin
     &t,Y,Ya,Yb,Yc,Z,Za,Zb,Zc
      double precision Zero,Zero1,Zlp
      integer i,iaind,Ian,Iatom,Icharg,icntr,Idp,idump,Iend,ieqatm,ieqsh
     &l,if1xyz,iffc,iffxyz,Ifilla,ifprt,Igbegn,Igdf,Igend,ij
      integer ilind,Imj,In,inao,Indjx,Indjy,Indjz,Indlp,Inew,intc,Iop,Io
     &ut,iprint,ipt,Ipunch,Ipurd,Ipurf,irad1,irad2,irad3
      integer Irange,irwb,ishell,ist,Istart,itest,Itype,iv,j,Jan,jcntr,J
     &end,Jgbegn,Jgdf,Jgend,jnd,Jnew,Jnktyp,Jrange,jshell
      integer jst,Jstart,Jtype,JUMP,k,Kfirst,Klast,Lamax,Lbmax,Lbound,LE
     &NB,lenpt,Lentq,Limitd,Lind,llim,Lmax,lmaxps,Lpmax,Lpskip
      integer lrad,MAXATM,MAXBAS,Maxdum,maxi2,maxint,maxl,maxp2,maxpri,m
     &axprj,MAXPRM,MAXS21,MAXSH1,MAXSHL,maxt1,maxt2,Maxtyp,mu,Multip,mun
     &u
      integer mus,mxcore,N10ord,N5ord,N6ord,N7ord,Nae,nat3,nat3tt,Natoms
     &,nbas6d,Nbasis,Nbe,nderiv,Ne,Nfroz,nlim,nlind,Nlp,Nordr
      integer nosymm,Nshell,ntlim,Ntpse,ntt,ntt6d,NTTMAX,nu
      parameter(MAXBAS=150,MAXATM=100,NTTMAX=(MAXBAS*(MAXBAS+1))/2)
      logical pure
      dimension d1(NTTMAX),f1xyz(105)
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      integer scona,sconb
      integer Ubound,Ulpure
      dimension ifprt(8)
      dimension core(59999)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      integer jjan(MAXSHL)
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Limit
     &d(11)
      common/lp2/Nlp(400),Clp(400),Zlp(400),Kfirst(35,5),Klast(35,5),Lma
     &x(35),Lpskip(35),Nfroz(35)
      common/fnumbs/Fnumb1,Fnumb2,Fnumb3,Fnumb4,Fnumb5,Fnumb6,Fnumb7
      common/ia/Lind(164),Ifilla(92)
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/io/In,Iout,Ipunch
      common/contr/Ca(20),Cb(20),Cc(20),Cd(20)
      common/int/Zero1,Xint(12)
      common/new/Inew,Jnew
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/force/E1xx,F1old(105)
      common/ipdrv/Aiab,Biab,Epio2,Pcx,Pcy,Pcz,Dabx,Daby,Dabz,Fillip(54)
      common/ipure/Ipurd,Ipurf
      common/intcon/F6i,F20i,F100
      common/ndex/Indjx(35),Indjy(35),Indjz(35),Indlp(20)
      common/dndex/Idp(5,5,5)
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/type/Itype,Jtype,Jnktyp(10)
      common/pseud/Ntpse(7,MAXATM)
      common/centre/Xa,Ya,Za,Xb,Yb,Zb,Xc,Yc,Zc,Iatom
      common/dens/D12(100),D12c(100)
      common/dint/Dvc(3),Dva(3),Dvb(3)
      common/ddint/Ddvcc(6),Ddvaa(6),Ddvbb(6),Ddvac(9),Ddvbc(9),Ddvab(9)
      common/prims/Igbegn,Igend,Jgbegn,Jgend,Igdf,Jgdf
      save irwb
      data ifprt/0,1,2,0,0,1,1,2/
      data f20/20.D0/
      data irwb/506/
      mxcore=59999
      
      
      
      crit=1.0D-06
      pi=Four*gatan(One)
      twopi=pi+pi
      rootpi=gsqrt(pi)
      pi3haf=pi*rootpi
      F6i=One/Xint(6)
      F20i=One/f20
      Fnumb1=Zero
      Fnumb2=One/Ten
      Fnumb3=pi
      Fnumb4=twopi+twopi
      Fnumb5=gsqrt(Fnumb4)
      Fnumb6=Fnumb4*Fnumb4
      Fnumb7=rootpi
      pt5=gfloat(1)/gfloat(2)
      F100=gfloat(100)
      
      
      iprint=Iop(33)+1
      iprint=ifprt(iprint)
      call ilsw(2,26,nosymm)
      call ilsw(2,2,Ipurd)
      call ilsw(2,16,Ipurf)
      idump=Iop(34)
      if(idump.GE.2)iprint=2
      
      
      call tread(irwb,Exx,LENB,1,LENB,1,0)
      if(idump.GE.2)call bdump(2)
      
      ntt=Nbasis*(Nbasis+1)/2
      
      call tread(512,Nlp,1210,1,1210,1,0)
      
      
      
      
      vlp=Zero
      call tread(532,d1,ntt,1,ntt,1,0)
      
      
      iffc=0
      nat3=3*Natoms
      nat3tt=0
      
      
      nbas6d=Nbasis
      ntt6d=ntt
      pure=(Ipurd.EQ.0.AND.Maxtyp.GE.2).OR.(Ipurf.EQ.0.AND.Maxtyp.GE.3)
      if(pure)then
      call setord
      call getnb6(nbas6d)
      ntt6d=(nbas6d*(nbas6d+1))/2
      endif
      
      ieqatm=1
      ieqshl=ieqatm+MAXATM*8
      if1xyz=ieqshl+MAXSHL*8
      iffxyz=if1xyz+nat3
      ipt=iffxyz+nat3tt
      lenpt=max0(4*ntt,ntt6d)
      ilind=ipt+max0(lenpt,nat3tt)
      nlind=max0(Nbasis+1,nbas6d+1,nat3)
      Iend=ilind+nlind
      if(pure)then
      inao=Iend+1
      itest=inao+nbas6d-1
      call redob(Nbasis,core(inao),iprint)
      call redop(Nbasis,nbas6d,core(inao),d1,iprint)
      Ipurd=1
      Ipurf=1
      endif
      
      do 100 i=1,nlind
      Lind(i)=(i*(i-1))/2
100   continue
      
      call setord
      nderiv=1
      maxpri=0
      maxint=0
      maxl=0
      do 200 ishell=1,Nshell
      maxpri=max0(maxpri,Shelln(ishell))
      scona=Shellc(ishell)
      Itype=Shellt(ishell)
      Lamax=Itype+1+nderiv
      if(Lamax.LE.4)then
      Irange=Ubound(Lamax)
      else
      Irange=35
      endif
      maxl=max0(Itype,maxl)
      maxint=max0(maxint,Irange)
200   continue
      maxp2=maxpri*maxpri
      maxi2=maxint*maxint
      ntt=(Nbasis*(Nbasis+1))/2
      lrad=maxp2*maxi2
      call iclear(7*MAXATM,Ntpse)
      maxprj=0
      do 300 i=1,Natoms
      lmaxps=Lmax(i)
      maxprj=max0(maxprj,lmaxps-1)
      do 250 j=1,lmaxps
      Ntpse(j,i)=Kfirst(i,j+1)
250   continue
      Ntpse(lmaxps+1,i)=Klast(i,lmaxps+1)+1
      Ntpse(lmaxps+2,i)=Kfirst(i,1)
      Ntpse(lmaxps+3,i)=Klast(i,1)+1
300   continue
      
      maxt1=2*maxl+nderiv
      maxt2=maxl+maxprj+nderiv
      write(Iout,99001)maxl,maxprj,nderiv,maxt1,maxt2
      if(maxl.GE.3.OR.maxt1.GT.6.OR.maxt2.GT.6)then
      write(Iout,99002)
99001 format(' MAXL=',i1,' MAXP=',i1,' NDERIV=',i1,' MAXT=',2I1)
99002 format(' L705 CANNOT EVALUATE DERIVATIVES IN THIS BASIS.')
      call lnk1e
      endif
      iv=1
      irad1=iv+lrad
      irad2=irad1+lrad
      irad3=irad2+343*maxp2-1
      if(irad3.GT.mxcore)then
      write(Iout,*)'  Not Enough Scratch Space in LINK705 ',irad3
      call lnk1e
      endif
      
      
      nlim=4
      llim=4
      ntlim=(nlim*(nlim+1)*(nlim+2))/6
      call ldata
      call ztab
      
      nat3=3*Natoms
      nat3tt=nat3*(nat3+1)/2
      call aclear(nat3,f1xyz)
      
      call iclear(5*5*5,Idp)
      do 400 i=1,35
      Idp(Indjx(i),Indjy(i),Indjz(i))=i
400   continue
      
      
      do 500 ishell=1,Nshell
      Xa=X(ishell)
      Ya=Y(ishell)
      Za=Z(ishell)
      do 450 Iatom=1,Natoms
      iaind=3*(Iatom-1)
      Xc=C(1+iaind)
      Yc=C(2+iaind)
      Zc=C(3+iaind)
      if((abs(Xa-Xc).LT.crit).AND.(abs(Ya-Yc).LT.crit).AND.(abs(Za-Zc).L
     &T.crit))jjan(ishell)=Iatom
450   continue
500   continue
      
      
      do 600 ishell=1,Nshell
      
      Inew=ishell
      Xa=X(ishell)
      Ya=Y(ishell)
      Za=Z(ishell)
      Igbegn=Shella(ishell)
      Igend=Igbegn+Shelln(ishell)-1
      icntr=jjan(ishell)
      Itype=Shellt(ishell)
      Lamax=Itype+1
      scona=Shellc(ishell)
      Iend=Ubound(Lamax)
      Istart=Lbound(Lamax,scona+1)
      Irange=Iend-Istart+1
      Igdf=Shladf(Inew)
      
      do 550 jshell=1,ishell
      
      Jnew=jshell
      Xb=X(jshell)
      Yb=Y(jshell)
      Zb=Z(jshell)
      Jgbegn=Shella(jshell)
      Jgend=Jgbegn+Shelln(jshell)-1
      jcntr=jjan(jshell)
      Jtype=Shellt(jshell)
      Lbmax=Jtype+1
      sconb=Shellc(jshell)
      Jstart=Lbound(Lbmax,sconb+1)
      Jend=Ubound(Lbmax)
      Jrange=Jend-Jstart+1
      Jgdf=Shladf(Jnew)
      
      Lpmax=Lamax+Lbmax-1
      Lentq=Irange*Jrange
      Imj=iabs(ishell-jshell)
      
      ist=Aos(ishell)-1
      jst=Aos(jshell)-1
      intc=0
      do 520 i=Istart,Iend
      mu=ist+Nordr(i)
      mus=Lind(mu)
      jnd=Jend
      if(Imj.EQ.0)jnd=i
      do 510 j=Jstart,jnd
      nu=jst+Nordr(j)
      munu=mus+nu
      intc=intc+1
      D12(intc)=d1(munu)
      if(mu.NE.nu)D12(intc)=D12(intc)+D12(intc)
510   continue
520   continue
      
      do 540 Iatom=1,Natoms
      if(Lpskip(Iatom).NE.1)then
      
      iaind=3*(Iatom-1)
      Xc=C(1+iaind)
      Yc=C(2+iaind)
      Zc=C(3+iaind)
      call cntlpd(core(iv),core(irad1),core(irad2),maxi2,maxp2,iffc)
      
      do 525 k=1,3
      Dvc(k)=-Dva(k)-Dvb(k)
525   continue
      
      do 530 k=1,3
      f1xyz(3*(icntr-1)+k)=f1xyz(3*(icntr-1)+k)+Dva(k)
      f1xyz(3*(jcntr-1)+k)=f1xyz(3*(jcntr-1)+k)+Dvb(k)
      f1xyz(3*(Iatom-1)+k)=f1xyz(3*(Iatom-1)+k)+Dvc(k)
530   continue
      endif
      
540   continue
550   continue
600   continue
      
      
      call tread(521,E1xx,106,1,106,1,0)
      E1xx=E1xx+vlp
      ij=0
      do 700 i=1,Natoms
      ij=ij+1
      F1old(ij)=F1old(ij)+f1xyz(ij)
      ij=ij+1
      F1old(ij)=F1old(ij)+f1xyz(ij)
      ij=ij+1
      F1old(ij)=F1old(ij)+f1xyz(ij)
      if(iprint.NE.0)then
      write(Iout,99003)i
      write(Iout,99004)f1xyz(ij-2),f1xyz(ij-1),f1xyz(ij)
99003 format(i8)
99004 format(3F20.10)
      endif
700   continue
      call twrite(521,E1xx,106,1,106,1,0)
      
      JUMP=0
      return
      
      end
C* :1 * 
      
