
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 pseud2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "pseud2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "pseud2.web"
      subroutine pseud2(LM,NTCNT,NP,ALPP,DP,SP,XNORM,QQ,MAXI2,MAXP2)
      implicit none
      real*8 ALPP,anga,angb,angp,binom,Ca,Ca2,Cax,Cay,Caz,Cb,Cb2,Cbx,Cby
     &,Cbz,Cca,Ccb,Cdummy,DP,eps1
      real*8 Fpi,gmax1,one,pab,pab1,pab2,pab3,pab4,pab5,pab6,Pi,Pi3haf,P
     &i5hf2,Piqurt,pmax,prang,q2sum,QQ,SP,Sqpi
      real*8 Sqpi2,Twopi,Xa,Xb,Xc,xka,xkb,XNORM,Ya,Yb,Yc,yka,ykb,Za,Zb,Z
     &c,zero,zka,zkb
      integer ia,iajaka,Iatom,ib,ibjbkb,Iend,Igbegn,Igdf,Igend,ii,iii,ij
     &inc,ijka,ijkb,ijlhi,ijllo,ijllob,Imj,In,inc
      integer ind,Indjx,Indjy,Indjz,Indlp,intc,Iout,iprim,Ipun,Irange,Is
     &tart,Itype,ja,jb,Jend,Jgbegn,Jgdf,Jgend,Jnktyp,Jrange
      integer Jstart,Jtype,ka,kb,l,la1,laind,lama,lamahi,lamalo,Lamax,la
     &mb,lambhi,lamblo,lb1,lbind,Lbmax,Lentq,lhi,Limitd
      integer llo,LM,lm1,lmahi,lmalo,lmbhi,lmblo,Lpmax,ltota,ltotb,m,ma1
     &,maind,Maxdum,MAXI2,MAXP2,mb1,mbind,mhi,n
      integer na1,naind,nb1,nbind,nhi,NP,nprim,NTCNT
      
      
      common/dist/Cax,Cay,Caz,Ca,Ca2,Cbx,Cby,Cbz,Cb,Cb2
      common/centre/Xa,Ya,Za,Xb,Yb,Zb,Xc,Yc,Zc,Iatom
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
      common/io/In,Iout,Ipun
      dimension QQ(7,7,7,MAXP2)
      dimension NP(*),ALPP(*),DP(*),NTCNT(*)
      dimension q2sum(7,7,7),anga(8,7,7),angb(8,7,7)
      dimension XNORM(MAXP2,MAXI2),SP(MAXP2,MAXI2)
      dimension binom(15),ind(5)
      save binom,ind,eps1,zero,one
      data binom/1.0D0,1.0D0,1.0D0,1.0D0,2.0D0,1.0D0,1.0D0,3.0D0,3.0D0,1
     &.0D0,1.0D0,4.0D0,6.0D0,4.0D0,1.0D0/,ind/1,2,4,7,11/,eps1/1.0D-15/,
     &zero/0.0D0/,one/1.0D0/
      
      call track(6Hpseud2)
      lm1=LM+1
      if(lm1.NE.1)then
      
      if(Ca.NE.zero)then
      xka=-Cax/Ca
      yka=-Cay/Ca
      zka=-Caz/Ca
      else
      xka=zero
      yka=zero
      zka=one
      endif
      if(Cb.NE.zero)then
      xkb=-Cbx/Cb
      ykb=-Cby/Cb
      zkb=-Cbz/Cb
      else
      xkb=zero
      ykb=zero
      zkb=one
      endif
      
      nprim=(Igend-Igbegn+1)*(Jgend-Jgbegn+1)
      
      llo=1
      lhi=LM
      inc=1
      do 100 l=llo,lhi,inc
      
      lmalo=max0(l-Lamax,0)+1
      lmahi=l+Lamax-1
      lmblo=max0(l-Lbmax,0)+1
      lmbhi=l+Lbmax-1
      nhi=Lamax+Lbmax-1
      call qqrad(l,lmalo,lmahi,lmblo,lmbhi,nhi,NP,ALPP,DP,NTCNT,QQ,XNORM
     &,MAXI2,MAXP2)
      
      mhi=l+l-1
      intc=0
      do 60 ii=Istart,Iend
      na1=Indjx(ii)
      la1=Indjy(ii)
      ma1=Indjz(ii)
      naind=ind(na1)
      laind=ind(la1)
      maind=ind(ma1)
      ltota=na1+la1+ma1-2
      lmalo=max0(l-ltota,0)+1
      lmahi=l+ltota-1
      call ang2(na1-1,la1-1,ma1-1,l-1,xka,yka,zka,anga)
      do 50 iii=Jstart,Jend
      nb1=Indjx(iii)
      lb1=Indjy(iii)
      mb1=Indjz(iii)
      nbind=ind(nb1)
      lbind=ind(lb1)
      mbind=ind(mb1)
      ltotb=nb1+lb1+mb1-2
      lmblo=max0(l-ltotb,0)+1
      lmbhi=l+ltotb-1
      nhi=ltota+ltotb-1
      intc=intc+1
      
      if((Ca.EQ.zero).AND.(Cb.EQ.zero))then
      ijlhi=min0(LM,ltota)
      ijlhi=min0(ijlhi,ltotb)
      ijllo=mod(ltota-1,2)+1
      ijllob=mod(ltotb-1,2)+1
      if(ijllo.NE.ijllob)goto 50
      if(ijllo.GT.ijlhi)goto 50
      ijinc=2
      elseif(Ca.NE.zero)then
      if(Cb.NE.zero)goto 5
      ijlhi=min0(LM,ltotb)
      ijllo=mod(ltotb-1,2)+1
      if(ijllo.GT.ijlhi)goto 50
      ijinc=2
      else
      ijlhi=min0(LM,ltota)
      ijllo=mod(ltota-1,2)+1
      if(ijllo.GT.ijlhi)goto 50
      ijinc=2
      endif
      if((l.LT.ijllo).OR.(l.GT.ijlhi))goto 50
      if(mod(l-ijllo,2).NE.0)goto 50
5     call ang2(nb1-1,lb1-1,mb1-1,l-1,xkb,ykb,zkb,angb)
      
      do 10 lama=lmalo,lmahi
      do 8 lamb=lmblo,lmbhi
      do 6 n=1,nhi
      q2sum(n,lama,lamb)=zero
6     continue
8     continue
10    continue
      pmax=0
      pab=one
      ijka=0
      do 30 ia=1,na1
      pab1=pab
      if(ia.NE.na1)pab1=pab*binom(naind+ia-1)*(Cax**(na1-ia))
      do 26 ja=1,la1
      pab2=pab1
      if(ja.NE.la1)pab2=pab1*binom(laind+ja-1)*(Cay**(la1-ja))
      do 24 ka=1,ma1
      pab3=pab2
      if(ka.NE.ma1)pab3=pab2*binom(maind+ka-1)*(Caz**(ma1-ka))
      ijka=ijka+1
      iajaka=ia+ja+ka-3
      lamahi=iajaka+l
      lamalo=max0(l-1-iajaka,0)+1
      if(mod(lamahi-lamalo,2).NE.0)lamalo=lamalo+1
      ijkb=0
      do 22 ib=1,nb1
      pab4=pab3
      if(ib.NE.nb1)pab4=pab3*binom(nbind+ib-1)*(Cbx**(nb1-ib))
      do 20 jb=1,lb1
      pab5=pab4
      if(jb.NE.lb1)pab5=pab4*binom(lbind+jb-1)*(Cby**(lb1-jb))
      do 18 kb=1,mb1
      pab6=pab5
      if(kb.NE.mb1)pab6=pab5*binom(mbind+kb-1)*(Cbz**(mb1-kb))
      ijkb=ijkb+1
      if(pab6.NE.zero)then
      ibjbkb=ib+jb+kb-3
      lambhi=ibjbkb+l
      lamblo=max0(l-1-ibjbkb,0)+1
      if(mod(lambhi-lamblo,2).NE.0)lamblo=lamblo+1
      do 16 lama=lamalo,lamahi,2
      do 14 lamb=lamblo,lambhi,2
      angp=zero
      do 12 m=1,mhi
      angp=angp+anga(ijka,m,lama)*angb(ijkb,m,lamb)
12    continue
      n=iajaka+ibjbkb+1
      prang=pab6*angp
      pmax=gmax1(abs(prang),pmax)
      q2sum(n,lama,lamb)=q2sum(n,lama,lamb)+prang
14    continue
16    continue
      endif
18    continue
20    continue
22    continue
24    continue
26    continue
30    continue
      
      do 40 lama=lmalo,lmahi
      do 36 lamb=lmblo,lmbhi
      do 34 n=1,nhi
      if(q2sum(n,lama,lamb).NE.zero)then
      do 32 iprim=1,nprim
      SP(iprim,intc)=SP(iprim,intc)+QQ(n,lama,lamb,iprim)*q2sum(n,lama,l
     &amb)
32    continue
      endif
34    continue
36    continue
40    continue
50    continue
60    continue
100   continue
      endif
      call track(6Hpseud2)
      return
      end
C* :1 * 
      
