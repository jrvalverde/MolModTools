
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gbasis"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gbasis.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 73 "gbasis.web"
      subroutine gbasis(IOP,C,IAN,NATOMS,NBASIS)
      implicit none
      double precision C,C1,C2,C3,C4,c4temp,cd,cp,cs,diff,e,Exx,rzero,sc
     &,Scal1,Scal1x,Scal2,Scal2x,Scal3,Scal3x
      double precision Scal4,Scal4x,Scale,thr,X,x1,Y,y1,Z,z1
      integer i,I2edsc,I2esf,I5d6d,ia,IAN,Iaos,Iatom,Ibas,ibasis,Ibmod,I
     &bpr,icc,icent,icntr,Icount,id,idx,ifd,iff
      integer In,iolbl,IOP,Iosc,ioscal,Iout,Ipt,Ipunch,Irot,Irtcrd,irwbv
     &,Iscal,iscon,ish,ishell,ispd,itipe,Ititle,itype,j
      integer j1,Jan,jbasis,jcent,Jpunch,kn,Label,LENB,Llink,maxcon,MAXP
     &RM,MAXS21,MAXSH1,MAXSHL,Maxtyp,mm,mmdf,NATOMS,NBASIS,ncent
      integer ndtype,nf,nftype,ngauss,Ngic,norb,nptype,ns,Nshell,nstart,
     &nstype,Numd
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      integer stype,scon
      integer Psave
      double precision iorb,jorb
      dimension C(*),IAN(*),IOP(50)
      dimension icent(35),jcent(35),jbasis(4),jorb(17),ishell(80)
      dimension e(10),cs(10),cp(10),cd(10)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/scalsp/Scal1(35),Scal1x(35),Scal2(35),Scal2x(35),Scal3(35),
     &Scal3x(35),Scal4(35),Scal4x(35),Iscal(35),Icount
      common/scale/Scale(MAXSHL)
      common/ops301/Ibas,Ngic,Ipt,I5d6d,Iosc,Ibmod,Ibpr,Llink,I2edsc,Iro
     &t,Jpunch,I2esf
      common/iatom/Iatom(36,2)
      common/io/In,Iout,Ipunch
      common/iaos/Iaos(MAXSHL)
      common/numd/Numd
      common/psave/Psave
      common/label/Label(1000),Ititle(100),Irtcrd(100)
      data maxcon/10/
      data jbasis/4H    ,4H STO,4HHFAO,4H****/
      data jorb/6H    1S,6H    2S,6H    2P,6H   2SP,6H    3S,6H    3P,6H
     &   3SP,6H    3D,6H  3SPD,6H    4S,6H    4P,6H   4SP,6H3D,4SP,6H   
     & 4D,6H    5S,6H    5P,6H   5SP/
      data rzero/0.0D0/
      data thr/1.0D-08/,irwbv/550/,ioscal/505/
      data iolbl/502/
99001 format(80I1)
99002 format(35I2)
99003 format(a4,a6,a4,i2,f12.6)
99004 format(4G20.10)
99005 format(43H ILLEGAL BASIS FUNCTION TYPE   $$$ STOP $$$)
99006 format(17H INPUT TO GBASIS:)
99007 format(1x,80I1)
99008 format(1x,35I2)
99009 format(1x,a4,a6,a4,i2,f12.6)
99010 format(1x,4G20.10)
99011 format(1H1)
99012 format(' UNACCEPTABLE VALUE OF IOP(10) IN GBASIS: ',i2)
      
      
      if(IOP(6).EQ.1)then
      
      
      
      ia=0
      call tread(iolbl,Label,600,1,600,1,0)
      call tread(irwbv,Exx(1),LENB,1,LENB,1,0)
      call tread(ioscal,Scale(1),MAXSHL,1,MAXSHL,1,0)
      
      do 50 ish=1,Nshell
      if(ish.GT.1)then
      
      diff=dabs(X(ish)-x1)+dabs(Y(ish)-y1)+dabs(Z(ish)-z1)
      if(diff.LE.thr)goto 20
      endif
      
      ia=ia+1
      if(ia.GT.NATOMS)call lnk1e
      x1=X(ish)
      y1=Y(ish)
      z1=Z(ish)
      
20    idx=3*(ia-1)
      X(ish)=C(idx+1)
      Y(ish)=C(idx+2)
      Z(ish)=C(idx+3)
50    continue
      
      if(IOP(10).LE.0)goto 1100
      if(IOP(10).NE.1)goto 800
      call sfopt
      goto 1100
      else
      ncent=1
      Nshell=0
      mm=1
      mmdf=1
      do 100 i=1,80
      Shladf(i)=0
100   continue
      jcent(1)=1
      if(Psave.EQ.0)write(Iout,99006)
150   read(In,99001)(ishell(i),i=1,80)
      do 200 i=1,80
      if(ishell(i).EQ.-0)ishell(i)=0
200   continue
      if(Psave.EQ.0)write(Iout,99007)(ishell(i),i=1,80)
      do 250 i=1,80
      if(ishell(i).GE.9)goto 300
      if(ishell(i).NE.0)then
      
      Nshell=Nshell+1
      Shella(Nshell)=mm
      Shelln(Nshell)=ishell(i)
      else
      ncent=ncent+1
      jcent(ncent)=Nshell+1
      endif
      mm=mm+ishell(i)
250   continue
      goto 150
      endif
      
300   read(In,99002)(icent(i),i=1,35)
      do 400 i=1,35
      if(icent(i).EQ.-0)icent(i)=0
400   continue
      if(Psave.EQ.0)write(Iout,99008)(icent(i),i=1,35)
      if(icent(1).NE.0)then
      ncent=0
      do 450 i=1,35
      if(icent(i).EQ.0)goto 500
      ncent=ncent+1
450   continue
500   kn=0
550   read(In,99003)ibasis,iorb,itype,ngauss,sc
      if(Psave.EQ.0)write(Iout,99009)ibasis,iorb,itype,ngauss,sc
      if(ibasis.EQ.jbasis(4))goto 300
      call typcon(itype,stype,scon)
      if(stype.EQ.3.AND.ngauss.NE.1)call berror(8)
      if(ibasis.NE.jbasis(1))then
      
      do 560 i=1,17
      if(iorb.EQ.jorb(i))goto 580
560   continue
      write(Iout,99005)
      call lnk1e
580   norb=i
      do 600 i=1,ngauss
      e(i)=rzero
      cs(i)=rzero
      cp(i)=rzero
      cd(i)=rzero
600   continue
      c4temp=rzero
      if(ibasis.NE.jbasis(2))call lnk1e
      if(norb.EQ.2)then
      
      call s2s(e,cs,ngauss)
      elseif(norb.EQ.3)then
      
      call s2p(e,cp,ngauss)
      elseif(norb.EQ.4)then
      
      call s2sp(e,cs,cp,ngauss)
      elseif(norb.EQ.5)then
      
      call s3s(e,cs,ngauss)
      elseif(norb.EQ.6)then
      
      call s3p(e,cp,ngauss)
      elseif(norb.EQ.7)then
      
      call s3sp(e,cs,cp,ngauss)
      elseif(norb.EQ.8)then
      
      call s3d(e,cd,ngauss)
      elseif(norb.EQ.9)then
      
      call s4sp(e,cs,cp,ngauss)
      elseif(norb.EQ.10)then
      call s4sp(e,cs,cp,ngauss)
      elseif(norb.EQ.11)then
      call s4sp(e,cs,cp,ngauss)
      elseif(norb.EQ.12)then
      call s4sp(e,cs,cp,ngauss)
      elseif(norb.EQ.13)then
      
      call berror(8)
      
      call s4d(e,cd,ngauss)
      elseif(norb.EQ.14)then
      call s4d(e,cd,ngauss)
      elseif(norb.EQ.15)then
      
      call s5sp(e,cs,cp,ngauss)
      elseif(norb.EQ.16)then
      call s5sp(e,cs,cp,ngauss)
      elseif(norb.EQ.17)then
      call s5sp(e,cs,cp,ngauss)
      else
      
      call s1s(e,cs,ngauss)
      endif
      else
      do 620 i=1,maxcon
      cs(i)=rzero
      cp(i)=rzero
      cd(i)=rzero
620   continue
      itipe=stype+1
      if(itipe.EQ.4)itipe=1
      if(itipe.EQ.2)then
      if(scon.EQ.1)then
      read(In,*)(e(i),cp(i),i=1,ngauss)
      else
      read(In,*)(e(i),cs(i),cp(i),i=1,ngauss)
      endif
      elseif(itipe.EQ.3)then
      if(scon.EQ.0)then
      write(Iout,99013)
99013 format(' The Use of SPD Shells is silly and not allowed')
      call lnk1e
      else
      read(In,*)(e(i),cd(i),i=1,ngauss)
      endif
      else
      read(In,*)(e(i),cs(i),i=1,ngauss)
      endif
      if(Psave.EQ.0)write(Iout,99010)(e(i),cs(i),cp(i),cd(i),i=1,ngauss)
      c4temp=rzero
      if(stype.GE.3)then
      c4temp=cs(1)
      cs(1)=rzero
      endif
      endif
      do 650 i=1,ncent
      icc=icent(i)
      ns=jcent(icc)+kn
      do 640 j=1,ngauss
      j1=Shella(ns)+j-1
      Exx(j1)=e(j)*sc**2
      C1(j1)=cs(j)
      C2(j1)=cp(j)
      if(stype.GE.2)then
      if(j.LE.1)Shladf(ns)=mmdf
      C3(mmdf)=cd(j)
      C4(mmdf)=c4temp
      mmdf=mmdf+1
      endif
640   continue
      idx=3*(icc-1)
      X(ns)=C(idx+1)
      Y(ns)=C(idx+2)
      Z(ns)=C(idx+3)
      Jan(ns)=IAN(icc)
      Shellt(ns)=stype
      Shellc(ns)=scon
      Scale(ns)=sc
650   continue
      kn=kn+1
      goto 550
      else
      
      nstart=1
      call ilsw(2,16,iff)
      ifd=I5d6d
      id=Numd
      ispd=9
      if(ifd.NE.0)ispd=10
      nf=7
      if(iff.NE.0)nf=10
      mm=0
      call putlbl(0,0,-1)
      icntr=1
      do 700 i=1,Nshell
      if((jcent(icntr).LE.i).AND.(icntr.LE.NATOMS))then
      call putlbl(icntr,IAN(icntr),0)
      icntr=icntr+1
      nstype=1
      nptype=1
      ndtype=1
      nftype=1
      endif
      iscon=Shellc(i)+1
      mm=mm+Shelln(i)
      Iaos(i)=nstart
      itype=Shellt(i)+1
      if(itype.EQ.2)then
      if(iscon.EQ.2)then
      Aos(i)=nstart-1
      call putlbl(nptype,1,1)
      nptype=nptype+1
      nstart=nstart+3
      else
      Aos(i)=nstart
      call putlbl(nstype,0,1)
      nstype=nstype+1
      call putlbl(nptype,1,1)
      nptype=nptype+1
      nstart=nstart+4
      endif
      elseif(itype.EQ.3)then
      if(iscon.EQ.2.OR.iscon.EQ.3)then
      Aos(i)=nstart-4
      call putlbl(ndtype,2,1)
      ndtype=ndtype+1
      nstart=nstart+id
      else
      Aos(i)=nstart
      call putlbl(nstype,0,1)
      nstype=nstype+1
      call putlbl(nptype,1,1)
      nptype=nptype+1
      call putlbl(ndtype,2,1)
      ndtype=ndtype+1
      nstart=nstart+ispd
      endif
      elseif(itype.EQ.4)then
      if(iscon.EQ.0)call lnk1e
      Aos(i)=nstart-10
      call putlbl(nftype,3,1)
      nftype=nftype+1
      nstart=nstart+nf
      else
      Aos(i)=nstart
      call putlbl(nstype,0,1)
      nstype=nstype+1
      nstart=nstart+1
      endif
700   continue
      Iaos(Nshell+1)=nstart
      Aos(Nshell+1)=nstart
      NBASIS=nstart-1
      
      call twrite(ioscal,Scale(1),MAXSHL,1,MAXSHL,1,0)
      
      call twrite(irwbv,Exx(1),LENB,1,LENB,1,0)
      
      if(IOP(10).LE.0)goto 1000
      if(IOP(10).EQ.2)goto 900
      endif
800   write(Iout,99012)IOP(10)
      call lnk1e
900   call sfopti(IOP)
      call sfopt
      
1000  return
      
1100  return
      
      end
C* :1 * 
      
