
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cycphf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cycphf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 33 "cycphf.web"
      subroutine cycphf(JCYCLE,ENTFIN,ACURCY,MAXCYC,NAE,NBE,NBASIS,NSYMO
     &P,NEQBAS)
      implicit none
      double precision ACURCY,Cc,Coeff,D,Da,Dd,De,Dg,energy,eng,ent,ENTF
     &IN,F,Fa,Fb,Ff,Filabc,fm,fm2,gabs
      integer i,ibeg,Icnvg,Icount,Icyc,Id505,idex,Idump,Iext,Ifcnvg,Ifil
     &l,ifin,iflag,ifnext,Iguess,ii,Ij,In,index0,indx
      integer inside,Iout,Ipch,Iprint,Ipunch,Irstrt,Irwc1,Irwc2,Irwc3,Ir
     &wc4,Irwca,Irwcb,Irwev,irwf,Irwfa,Irwfb,Irwgen,Irwh,Irwpa,Irwpb
      integer Irwpt,Irws,Irwt,istop,istrt,j,JCYCLE,jj,js,k,key,MAXBAS,MA
     &XCYC,Maxnbf,Maxntt,n,NAE,nb2,NBASIS,NBE
      integer nbep1,NEQBAS,nsmall,nsml,NSYMOP,ntimes,Ntt,Ntt2
      double precision pt5,temp,thresh,Trmsdp,Val,zero
      integer Psave
      parameter(MAXBAS=150)
      double precision iword,ident,name
      dimension Da(1),D(1),Fa(1),Fb(1),Dg(1),NEQBAS(MAXBAS,8)
      dimension iword(2),istrt(3,2),istop(3,2),idex(3,2),nsml(3),ntimes(
     &3),fm(2),fm2(2),ident(2)
      dimension eng(2)
      dimension irwf(4),name(3)
      common/ops505/Ipch,Iprint,Idump,Iguess,Icnvg,Icyc,Irstrt,Iext,Ifcn
     &vg
      common/max505/Maxnbf,Maxntt
      common/psave/Psave
      common/irw505/Irwgen,Irws,Irwt,Irwh,Irwev,Irwca,Irwcb,Irwpa,Irwpb,
     &Irwpt,Irwfa,Irwfb,Irwc1,Irwc2,Irwc3,Irwc4
      common/rhfcvg/Ntt2,Icount,Trmsdp
      common/memry/Dd(70,70),De(70),F(70,70),Ff(70),Coeff(70,70),Cc(70),
     &Id505,Ifill,Filabc(35089)
      common/io/In,Iout,Ipunch
      common/jnkphf/Ntt,Ij(71)
      common/eigval/Val(210)
      equivalence(Da(1),Dd(1,1)),(Fa(1),F(1,1))
      equivalence(D(1),Dd(36,36)),(Fb(1),F(36,36))
      equivalence(Dg(1),Fb(1))
      data zero/0.00D00/,pt5/0.50D00/,iword/6HBEFORE,6H AFTER/,fm/0.00D0
     &,1.00D00/
      data fm2/2.0D0,-1.0D0/
      data irwf/536,537,538,539/,name/6H(D/E) ,6H(S/E) ,6H(D/S) /
      data thresh/1.0D-7/
      
      
      
      
      
      
      
      
99001 format(43H0DENSITY MATRICES AT THE BEGINNING OF CYCLE,i6,' SUBCYCL
     &E ',i6)
99002 format(20H  DENSITY MATRIX: DA)
99003 format(20H  DENSITY MATRIX: D )
99004 format(13H FOCK MATRIX ,a6,24H TWO-ELECTRON INTEGRALS:)
99005 format(14H      AT CYCLE,i3,10H SUBCYCLE ,i3,1x,a6)
99006 format(1H+,5x,d22.15)
99007 format(1x,i3)
99008 format(32H  INNER LOOP COMPLETE FOR CYCLE ,i3,10H SUBCYCLE ,i3)
99009 format(31H0MOLECULAR ORBITAL COEFFICIENTS)
99010 format(21H0ALPHA DENSITY MATRIX)
99011 format(21H0BETA  DENSITY MATRIX)
99012 format(13H EIGENVALUES:/(11x,g20.10))
99013 format(42H WARNING --- CONVERGENCE CRITERION NOT MET)
99014 format(1H+,7x,17H(NON-VARIATIONAL))
99015 format(1x,i4)
99016 format(1x,a6,17HM.O. FOCK-MATRIX.)
      
      Ntt2=2*Ntt
      Icount=0
      Trmsdp=zero
      iflag=0
      ifnext=0
      key=1
      JCYCLE=0
      nbep1=NBE+1
      nsml(1)=NBASIS-(NAE-NBE)
      ntimes(1)=2
      istrt(1,1)=1
      istop(1,1)=NBE
      idex(1,1)=0
      istrt(1,2)=nbep1
      istop(1,2)=nsml(1)
      idex(1,2)=NAE-NBE
      nsml(2)=NBASIS-NBE
      ntimes(2)=1
      istrt(2,1)=1
      istop(2,1)=nsml(2)
      idex(2,1)=NBE
      nsml(3)=NAE
      ntimes(3)=1
      istrt(3,1)=1
      istop(3,1)=NAE
      idex(3,1)=0
100   JCYCLE=JCYCLE+1
      if(Psave.EQ.0)write(Iout,99007)JCYCLE
      do 500 inside=1,3
      nsmall=nsml(inside)
      if(Iprint.GE.3)then
      write(Iout,99001)JCYCLE,inside
      write(Iout,99002)
      call ltoutd(NBASIS,Da(1),1)
      write(Iout,99003)
      call ltoutd(NBASIS,D(1),1)
      endif
      call tread(Irwh,Fa(1),Maxntt,1,Ntt,1,0)
      if(inside.NE.3)then
      k=0
      energy=zero
      do 120 i=1,NBASIS
      do 110 j=1,i
      k=k+1
      temp=Fa(k)*(fm(inside)*Da(k)+fm2(inside)*D(k))
      energy=energy+temp
110   continue
      energy=energy-temp*pt5
120   continue
      ent=energy+energy
      endif
      if(Iprint.GE.4)then
      write(Iout,99004)iword(1)
      call ltoutd(NBASIS,Fa(1),1)
      endif
      call fofphf(JCYCLE,inside,NBASIS,NSYMOP,NEQBAS,Irwh)
      call twrite(irwf(inside),Fa(1),Maxntt,1,Ntt,1,0)
      if(inside.NE.3)then
      k=0
      do 140 i=1,NBASIS
      do 130 j=1,i
      k=k+1
      temp=Fa(k)*(fm(inside)*Da(k)+fm2(inside)*D(k))
      energy=energy+temp
130   continue
      energy=energy-temp*pt5
140   continue
      eng(inside)=energy
      endif
      call square(Fa(1),F(1,1),Maxnbf,NBASIS,0)
      if(Iprint.GE.4)then
      write(Iout,99004)iword(2)
      call matout(F(1,1),Maxnbf,Maxnbf,NBASIS,NBASIS)
      endif
      call tread(Irwca,Dd(1,1),Maxnbf,Maxnbf,NBASIS,NBASIS,0)
      if(Iprint.GE.4)then
      write(6,99017)
      
99017 format('  M. O. COEFFICIENTS BEFORE BLOCKING')
      
      call matout(Dd(1,1),Maxnbf,Maxnbf,NBASIS,NBASIS)
      endif
      n=ntimes(inside)
      do 200 k=1,n
      ibeg=istrt(inside,k)
      ifin=istop(inside,k)
      indx=idex(inside,k)
      do 160 j=ibeg,ifin
      do 150 i=1,NBASIS
      Coeff(i,j)=Dd(i,j+indx)
150   continue
160   continue
200   continue
      if(Iprint.GE.4)then
      write(6,99018)
      
99018 format('  M. O. COEFFICIENTS AFTER BLOCKING')
      
      call matout(Coeff(1,1),Maxnbf,Maxnbf,NBASIS,nsmall)
      endif
      call matrc3(F,Coeff,Dd,NBASIS,NBASIS,nsmall,1)
      call matrc3(Coeff,Dd,F,nsmall,NBASIS,nsmall,2)
      do 250 i=1,NBASIS
      Ff(i)=zero
250   continue
      call eigen(nsmall,F,Dd,Ff,De)
      call matrc3(Coeff,Dd,F,NBASIS,nsmall,nsmall,1)
      call tread(Irwca,Coeff(1,1),Maxnbf,Maxnbf,NBASIS,NBASIS,0)
      if(Iprint.GE.4)then
      write(6,99017)
      call matout(Coeff(1,1),Maxnbf,Maxnbf,NBASIS,NBASIS)
      endif
      do 300 k=1,n
      ibeg=istrt(inside,k)
      ifin=istop(inside,k)
      indx=idex(inside,k)
      do 280 j=ibeg,ifin
      do 260 i=1,NBASIS
      Coeff(i,j+indx)=F(i,j)
260   continue
280   continue
300   continue
      if(Iprint.GE.4)then
      write(6,99018)
      call matout(Coeff(1,1),Maxnbf,Maxnbf,NBASIS,NBASIS)
      endif
      call twrite(Irwca,Coeff(1,1),Maxnbf,Maxnbf,NBASIS,NBASIS,0)
      call phfchk(NBASIS)
      call tread(Irwca,Coeff(1,1),Maxnbf,Maxnbf,NBASIS,NBASIS,0)
      do 350 i=1,NBASIS
      do 340 j=1,i
      js=Ij(i)+j
      temp=zero
      do 310 k=1,NBE
      temp=temp+(Coeff(i,k)*Coeff(j,k))
310   continue
      D(js)=temp
      do 320 k=nbep1,NAE
      temp=temp+(Coeff(i,k)*Coeff(j,k))
320   continue
      Da(js)=temp
340   continue
350   continue
      
      call twrite(Irwpa,Da(1),Maxntt,1,Ntt,1,0)
      call twrite(Irwpb,D(1),Maxntt,1,Ntt,1,0)
      if(Iprint.GE.3)then
      write(Iout,99008)JCYCLE,inside
      write(Iout,99009)
      call matout(Coeff(1,1),Maxnbf,Maxnbf,NBASIS,NBASIS)
      write(Iout,99010)
      call ltoutd(NBASIS,Da(1),1)
      write(Iout,99011)
      call ltoutd(NBASIS,D(1),1)
      write(Iout,99012)(Ff(k),k=1,NBASIS)
      endif
      index0=70*(inside-1)
      do 400 i=1,NBASIS
      Val(index0+i)=Ff(i)
400   continue
      if(Ifcnvg.EQ.0)call conphf(NBASIS,key,ACURCY,inside,iflag)
500   continue
      ENTFIN=eng(1)+eng(2)
      if(ifnext.EQ.0.AND.Psave.EQ.0)write(Iout,99006)ENTFIN
      if(ifnext.EQ.1.AND.Psave.EQ.0)write(Iout,99014)
      ifnext=iflag
      if(key.NE.0)then
      if(JCYCLE.LT.MAXCYC)goto 100
      write(Iout,99013)
      call ilsw(1,5,1)
      else
      
      call ilsw(1,5,0)
      endif
      call twrite(Irwpa,Da(1),Maxntt,1,Ntt,1,0)
      call twrite(Irwpb,D(1),Maxntt,1,Ntt,1,0)
      call twrite(Irwcb,Coeff(1,1),Maxnbf,Maxnbf,NBASIS,NBASIS,0)
      nb2=NBASIS+NBASIS
      call twrite(Irwev,Ff(1),Maxnbf,1,nb2,1,0)
      
      call tread(Irwca,Coeff,Maxnbf,Maxnbf,NBASIS,NBASIS,0)
      do 600 i=1,3
      call tread(irwf(i),F,Maxnbf,Maxnbf,NBASIS,NBASIS,1)
      call matrc3(F,Coeff,Dd,NBASIS,NBASIS,NBASIS,1)
      call matrc3(Coeff,Dd,F,NBASIS,NBASIS,NBASIS,2)
      call twrite(irwf(i),F,Maxnbf,Maxnbf,NBASIS,NBASIS,1)
      if(Iprint.GE.2)then
      write(Iout,99016)name(i)
      do 520 ii=1,NBASIS
      do 510 jj=1,NBASIS
      if(gabs(F(ii,jj)).LT.thresh)F(ii,jj)=zero
510   continue
520   continue
      call matout(F,Maxnbf,Maxnbf,NBASIS,NBASIS)
      endif
600   continue
      return
      
      end
C* :1 * 
      
