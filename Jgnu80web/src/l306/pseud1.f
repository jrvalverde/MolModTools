
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 pseud1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "pseud1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "pseud1.web"
      subroutine pseud1(NT,NP,ALPP,DP,SP,XNORM,MAXI2,MAXP2)
      implicit none
      real*8 alpa,alpb,Alpha,ALPP,ang,angsum,Argab,Ca,Ca2,Cax,Cay,Caz,Cb
     &,Cb2,Cbx,Cby,Cbz,Cca,Ccb,Cdummy
      real*8 DP,eabc,Expab,Fpi,one,pab1,pab2,pab3,Pi,Pi3haf,Pi5hf2,Piqur
     &t,Q,qsum,Rk,SP,Sqpi,Sqpi2,ss,T
      real*8 two,Twopi,Xa,xab,Xb,Xc,xk,XNORM,Ya,yab,Yb,Yc,yk,Za,zab,Zb,Z
     &c,zero,zk
      integer i,Iatom,Iend,igauss,Igbegn,Igdf,Igend,ii,iii,ijk,Imj,Indjx
     &,Indjy,Indjz,Indlp,intc,iprim,Irange,Istart,Itype
      integer j,Jend,jgauss,Jgbegn,Jgdf,Jgend,Jnktyp,Jrange,Jstart,Jtype
     &,k,kb,kstart,la,lalb,Lamax,lambda,lb,Lbmax,Lentq
      integer Limitd,lmb,Lpmax,ltot,ltot1,ma,mamb,Maxdum,MAXI2,MAXP2,mb,
     &na,nanb,nb,NP,NT
      
      
      common/centre/Xa,Ya,Za,Xb,Yb,Zb,Xc,Yc,Zc,Iatom
      common/dist/Cax,Cay,Caz,Ca,Ca2,Cbx,Cby,Cbz,Cb,Cb2
      common/qstore/Q(9,7),Alpha,Rk,T
      common/argab/Argab,Expab
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Limit
     &d(11)
      common/ndex/Indjx(35),Indjy(35),Indjz(35),Indlp(20)
      common/prims/Igbegn,Igend,Jgbegn,Jgend,Igdf,Jgdf
      common/contr/Cca(20),Ccb(20),Cdummy(40)
      common/type/Itype,Jtype,Jnktyp(10)
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
      dimension NP(*),ALPP(*),DP(*)
      dimension SP(MAXP2,MAXI2),XNORM(MAXP2,MAXI2)
      dimension ang(7),xab(7),yab(7),zab(7)
      dimension qsum(7,7),angsum(7,7)
      save zero,one,two
      data zero/0.0D0/,one/1.0D0/,two/2.0D0/
      
      call track(6Hpseud1)
      ltot=Lamax+Lbmax-2
      ltot1=ltot+1
      
      
      iprim=0
      do 200 igauss=Igbegn,Igend
      alpa=Exx(igauss)
      
      do 100 jgauss=Jgbegn,Jgend
      alpb=Exx(jgauss)
      iprim=iprim+1
      
      if(ltot1.GT.7)call lnk1e
      do 20 j=1,ltot1
      do 10 k=1,j
      qsum(j,k)=zero
10    continue
20    continue
      eabc=-alpa*Ca2-alpb*Cb2
      xk=-two*(alpa*Cax+alpb*Cbx)
      yk=-two*(alpa*Cay+alpb*Cby)
      zk=-two*(alpa*Caz+alpb*Cbz)
      Rk=sqrt(xk*xk+yk*yk+zk*zk)
      if(Rk.NE.zero)then
      xk=xk/Rk
      yk=yk/Rk
      zk=zk/Rk
      else
      xk=zero
      yk=zero
      zk=one
      endif
      
      do 40 i=1,NT
      Alpha=alpa+alpb+ALPP(i)
      kstart=NP(i)
      call recur1(NP(i),ltot)
      do 30 j=1,ltot1
      do 25 k=1,j,2
      kb=j-k+1
      if(j.GT.7.OR.kb.LT.1.OR.kb.GT.7.OR.(kstart+j).GT.9)call lnk1e
      qsum(j,kb)=qsum(j,kb)+Q(kstart+j,kb)*DP(i)*exp(eabc-Argab)
25    continue
30    continue
40    continue
      
      intc=0
      do 80 ii=Istart,Iend
      na=Indjx(ii)-1
      la=Indjy(ii)-1
      ma=Indjz(ii)-1
      
      do 70 iii=Jstart,Jend
      nb=Indjx(iii)-1
      lb=Indjy(iii)-1
      mb=Indjz(iii)-1
      intc=intc+1
      call facab(na,nb,Cax,Cbx,xab)
      call facab(la,lb,Cay,Cby,yab)
      call facab(ma,mb,Caz,Cbz,zab)
      
      nanb=na+nb+1
      lalb=la+lb+1
      mamb=ma+mb+1
      ltot1=nanb+lalb+mamb-2
      do 45 j=1,ltot1
      do 42 k=1,j
      angsum(j,k)=zero
42    continue
45    continue
      do 55 i=1,nanb
      pab1=xab(i)
      do 50 j=1,lalb
      pab2=pab1*yab(j)
      do 48 k=1,mamb
      pab3=pab2*zab(k)
      ijk=i+j+k-2
      call ang1(i-1,j-1,k-1,xk,yk,zk,ang)
      do 46 lambda=1,ijk,2
      lmb=ijk-lambda+1
      angsum(ijk,lmb)=angsum(ijk,lmb)+ang(lmb)*pab3
46    continue
48    continue
50    continue
55    continue
      
      ss=zero
      do 60 j=1,ltot1
      do 56 k=1,j,2
      kb=j-k+1
      ss=ss+angsum(j,kb)*qsum(j,kb)
56    continue
60    continue
      SP(iprim,intc)=SP(iprim,intc)+ss
70    continue
80    continue
100   continue
200   continue
      call track(6Hpseud1)
      return
      end
C* :1 * 
      
