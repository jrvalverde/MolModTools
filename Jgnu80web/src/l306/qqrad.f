
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qqrad"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qqrad.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 32 "qqrad.web"
      subroutine qqrad(L,LMALO,LMAHI,LMBLO,LMBHI,NHI,NP,ALPP,DP,NTCNT,QQ
     &,XNORM,MAXI2,MAXP2)
      implicit none
      real*8 alpa,alpb,alpha,ALPP,arg,argsum,bessa,bessb,Ca,Ca2,Cax,Cay,
     &Caz,Cb,Cb2,Cbx,Cby,Cbz,DP,eps1
      real*8 exparg,F,f100,fka,fkb,four,Fpi,one,Pi,Pi3haf,Pi5hf2,Piqurt,
     &Pt,Ptpow,qlim,QQ,qtemp,rc,rka,rkb
      real*8 Sqpi,Sqpi2,two,Twopi,XNORM,zero
      integer i,igauss,Igbegn,Igdf,Igend,ihi,ilo,In,Iout,iprim,Ipun,j,jg
     &auss,Jgbegn,Jgdf,Jgend,L,la,lalop2,lama
      integer lamb,latru,lb,lblop2,lbtru,LMAHI,lmahm1,LMALO,LMBHI,lmbhm1
     &,LMBLO,MAXI2,MAXP2,n,NHI,nhim1,nlim,NP,npi,Npts
      integer NTCNT
      
      
      common/dist/Cax,Cay,Caz,Ca,Ca2,Cbx,Cby,Cbz,Cb,Cb2
      common/ptwtdt/Ptpow(50,7),F(50,7,7),Pt(50),Npts
      common/prims/Igbegn,Igend,Jgbegn,Jgend,Igdf,Jgdf
      integer MAXSHL,MAXPRM,MAXSH1,MAXS21,Jan,Shella,Shelln,Shellt,Shell
     &c,Shladf,Aos,Aon,Nshell,Maxtyp
      real*8 Exx,C1,C2,C3,C4,X,Y,Z
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/pifac/Pi,Twopi,Fpi,Pi3haf,Pi5hf2,Piqurt,Sqpi,Sqpi2
      common/io/In,Iout,Ipun
      dimension NP(*),ALPP(*),DP(*),NTCNT(*)
      dimension QQ(7,7,7,*),XNORM(MAXP2,MAXI2)
      dimension qtemp(7,7,7)
      save eps1,f100,zero,one,two,four
      data eps1/1.E-15/,f100/100.0D0/,zero/0.0D0/,one/1.0D0/,two/2.0D0/,
     &four/4.0D0/
      
      ilo=NTCNT(L)
      ihi=NTCNT(L+1)-1
      iprim=0
      
      do 100 igauss=Igbegn,Igend
      alpa=Exx(igauss)
      rka=two*alpa*Ca
      
      do 50 jgauss=Jgbegn,Jgend
      alpb=Exx(jgauss)
      rkb=two*alpb*Cb
      
      iprim=iprim+1
      call aclear(343,QQ(1,1,1,iprim))
      
      do 40 i=ilo,ihi
      npi=NP(i)
      alpha=alpa+alpb+ALPP(i)
      argsum=((rka+rkb)*(rka+rkb))/(two*alpha)
      if(argsum.LT.f100)then
      
      exparg=exp(-alpa*Ca*Ca-alpb*Cb*Cb)*DP(i)/sqrt(alpha)
      else
      
      arg=(-alpa*alpb*(Ca-Cb)*(Ca-Cb)-ALPP(i)*(alpa*Ca*Ca+alpb*Cb*Cb))/a
     &lpha
      exparg=exp(arg)*DP(i)/sqrt(alpha)
      rc=(rka+rkb)/(two*alpha)
      if(rc.LT.one)nlim=npi
      if(rc.GT.one)nlim=NHI+npi-1
      bessa=one
      if(rka.NE.zero)bessa=one/(two*rc*rka)
      bessb=one
      if(rkb.NE.zero)bessb=one/(two*rc*rkb)
      qlim=rc**nlim*Sqpi*bessa*bessb
      if((qlim*Fpi*abs(exparg)).LT.eps1)goto 40
      endif
      
      lmahm1=max0(LMAHI-1,1)
      lmbhm1=max0(LMBHI-1,1)
      nhim1=NHI-1
      call aclear(343,qtemp(1,1,1))
      if(rka.NE.zero.AND.rkb.NE.zero)then
      call ptprep(npi,NHI,lmahm1,LMAHI,lmbhm1,LMBHI,alpha,rka,rkb,argsum
     &)
      call recurf(LMALO,LMAHI,LMBLO,LMBHI,rka,rkb)
      call quadr(LMALO,LMAHI,LMBLO,LMBHI,1,1,qtemp(1,1,1))
      if(NHI.GT.1)then
      call quadr(lmahm1,LMAHI,lmbhm1,LMBHI,2,NHI,qtemp(1,1,1))
      if(LMAHI.GT.2.OR.LMBHI.GT.2)call recur2(nhim1,LMALO,LMAHI,LMBLO,LM
     &BHI,rka,rkb,qtemp(1,1,1))
      endif
      elseif(rka.NE.zero.OR.rkb.NE.zero)then
      if(rka.NE.zero)then
      call ptprep(npi,NHI,lmahm1,LMAHI,1,1,alpha,rka,rkb,argsum)
      if(LMBLO.EQ.1)then
      call quadr(lmahm1,LMAHI,1,1,1,NHI,qtemp(1,1,1))
      if(LMAHI.GT.2)then
      fka=one/rka
      lalop2=LMALO+2
      do 6 la=LMAHI,lalop2,-1
      latru=la-1
      do 2 j=1,Npts
      F(j,la-2,1)=F(j,la,1)+(2*latru-1)*(fka/Pt(j))*F(j,la-1,1)
      qtemp(1,la-2,1)=qtemp(1,la-2,1)+Ptpow(j,1)*F(j,la-2,1)
2     continue
      do 4 n=1,nhim1
      qtemp(n+1,la-2,1)=qtemp(n+1,la,1)+(2*latru-1)*fka*qtemp(n,la-1,1)
4     continue
6     continue
      endif
      endif
      elseif(LMALO.EQ.1)then
      call ptprep(npi,NHI,1,1,lmbhm1,LMBHI,alpha,rka,rkb,argsum)
      call quadr(1,1,lmbhm1,LMBHI,1,NHI,qtemp(1,1,1))
      if(LMBHI.GT.2)then
      fkb=one/rkb
      lblop2=LMBLO+2
      do 12 lb=LMBHI,lblop2,-1
      lbtru=lb-1
      do 8 j=1,Npts
      F(j,1,lb-2)=F(j,1,lb)+(2*lbtru-1)*(fkb/Pt(j))*F(j,1,lb-1)
      qtemp(1,1,lb-2)=qtemp(1,1,lb-2)+Ptpow(j,1)*F(j,1,lb-2)
8     continue
      do 10 n=1,nhim1
      qtemp(n+1,1,lb-2)=qtemp(n+1,1,lb)+(2*lbtru-1)*fkb*qtemp(n,1,lb-1)
10    continue
12    continue
      endif
      endif
      elseif((LMALO.EQ.1).AND.(LMBLO.EQ.1))then
      call ptprep(npi,NHI,1,1,1,1,alpha,rka,rkb,argsum)
      call quadr(1,1,1,1,1,NHI,qtemp(1,1,1))
      endif
      
      do 20 lama=LMALO,LMAHI
      do 15 lamb=LMBLO,LMBHI
      do 14 n=1,NHI
      QQ(n,lama,lamb,iprim)=QQ(n,lama,lamb,iprim)+qtemp(n,lama,lamb)*exp
     &arg
14    continue
15    continue
20    continue
40    continue
50    continue
100   continue
      
      
      return
      end
C* :1 * 
      
