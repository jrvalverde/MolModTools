
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 twldrv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "twldrv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 93 "twldrv.web"
      subroutine twldrv(ISCF,DM,DN,ICRIT,USESYM,NSYMOP,JTRANS,NEQATM,NAT
     &OMS,C,VEE,FXYZ,IDUMP)
      implicit none
      double precision A1,A12,A1234,A1234i,A12i,A2,A3,A34,A34i,A4,abve00
     &,abx,aby,abz,ax,ax1,ay,az,bx,by
      double precision bz,C,C1,C1110,C1120,C1130,C1210,C1220,C1230,C1310
     &,C1320,C1330,C2,C2110,C2120,C2130,C2210,C2220,C2230,C2310
      double precision C2320,C2330,C3,C3110,C3120,C3130,C3210,C3220,C323
     &0,C3310,C3320,C3330,C4,ccut,cdve00,cdx,cdy,cdz,cpa,cpb
      double precision cpc,cpd,cpp,cppp,Cpppp,cppps,cpps,Cppsp,cppss,cps
     &,cpsp,Cpspp,cpsps,cpss,Cpssp,cpsss,csa,csb,csc,csd
      double precision csmab,csmcd,csp,cspp,Csppp,cspps,csps,Cspsp,cspss
     &,css,cssp,Csspp,cssps,csss,Csssp,csuma,csumc,cx,cy,cz
      double precision DM,dmax,DN,dvex,dvexs,dvey,dveys,dvez,dvezs,dx,dx
     &1x,dx1y,dx1z,dx2x,dx2y,dx2z,dy,dz,E,e12
      double precision e34,e34max,eoooo,exooo,Exx,eyooo,ezooo,four,four5
     &,Fq0,Fq1,Fq2,Fq3,Fq4,Fq5,fxi,fxj,fxk,fxl,FXYZ
      double precision fyi,fyj,fyk,fyl,fzi,fzj,fzk,fzl,gabs,gatan,gexp,G
     &oooo,Gooxo,Gooyo,Goozo,gsqrt,Gxooo,Gxoxo,Gxoyo,Gxozo
      double precision Gxxoo,Gxxxo,Gxxyo,Gxxzo,Gxyoo,Gxyzo,Gxzoo,Gyooo,G
     &yoyo,Gyozo,Gyyoo,Gyyxo,Gyyyo,Gyyzo,Gyzoo,Gzooo,Gzozo,Gzzoo,Gzzxo,G
     &zzyo
      double precision Gzzzo,h,ha12i,hfq1,hfq2,one,one5,Opox,Opoy,Opoz,O
     &pxo,Opxox,Opxx,Opxy,Opxz,Opyo,Opyoy,Opyx,Opyy,Opyz
      double precision Opzo,Opzoz,Opzx,Opzy,Opzz,Oqox,Oqoy,Oqoz,Oqxo,Oqx
     &ox,Oqxx,Oqxy,Oqxz,Oqyo,Oqyoy,Oqyx,Oqyy,Oqyz,Oqzo,Oqzoz
      double precision Oqzx,Oqzy,Oqzz,p25,pi,pidiv4,pito52,Pqx,Pqxx,Pqxy
     &,Pqxz,Pqy,Pqyy,Pqyz,Pqz,Pqzz,px,py,pz,Qa
      double precision Qa1,Qa2,qfq1,qfq2,qfq3,qq,qve00,qx,qy,qz,r12,r34,
     &S1,S12,S2,S3,S34,S4,Shladf,sxtn
      double precision t,ta34i,Tbaa,Tbab,Tbac,Tbad,Tbae,Tbaf,Tbba,Tbbb,T
     &bbc,Tbbd,Tbbe,Tbbf,Tbca,Tbcb,Tbcc,Tbcd,Tbce,Tbcf
      double precision temp,temp1,temp2,tenm2,tenm4,tenm7,tfq2,theta,the
     &ta2,theta3,theta4,three,three5,ti,trp,ts3,ts34,ts4,twenty,two
      double precision two5,V0000,V0010,V0020,V0030,V0100,V0110,V0120,V0
     &130,V0200,V0210,V0220,V0230,V0300,V0310,V0320,V0330,V1000,V1010,V1
     &020
      double precision V1030,V1100,V1110,V1120,V1130,V1200,V1210,V1220,V
     &1230,V1300,V1310,V1320,V1330,V2000,V2010,V2020,V2030,V2100,V2110,V
     &2120
      double precision V2130,V2200,V2210,V2220,V2230,V2300,V2310,V2320,V
     &2330,V3000,V3010,V3020,V3030,V3100,V3110,V3120,V3130,V3200,V3210,V
     &3220
      double precision V3230,V3300,V3310,V3320,V3330,Ve00,ve00s,Ve11,ve1
     &1s,Ve12,ve12s,Ve13,Ve14,Ve21,ve21s,Ve22,ve22s,Ve23,Ve24,Ve31
      double precision ve31s,Ve32,ve32s,Ve33,Ve34,VEE,vtest,vtol,vtol1,v
     &tol2,vtols,X,xe34,Y,Z,zero
      integer i,ia,iaind,iat,iat1,iatm,iatx,iaty,iatz,ICRIT,id,IDUMP,ien
     &d,ifqmax,ij,In,inew,Iout,iprio,Ipunch
      integer irwtab,ISCF,ishell,istart,isymop,ixtr,iytr,izero,iztr,j,Ja
     &n,jat,jat1,jatx,jaty,jatz,jend,jnd,jnew,jop
      integer jopind,jopm1,jrop,jshell,jstart,JTRANS,jtype,k,kat,kat1,ka
     &tx,katy,katz,kend,kl,knd,knew,kop,kshell,kstart
      integer kzero,l,la,lat,lat1,latx,laty,latz,lb,lc,ld,LENB,lend,lnd,
     &lnew,lshell,lstart,MAXPRM,MAXS21,MAXSH1
      integer MAXSHL,Maxtyp,mop,n,NATOMS,NEQATM,np1,np2,nprio,Nshell,NSY
     &MOP,numop
      integer Shella,Shelln,Shellt,Aos,Aon
      integer Shellc
      logical USESYM
      logical ijsame,klsame,iksmjl
      logical flag
      dimension DM(*),DN(*),JTRANS(3,8),NEQATM(*),C(*),FXYZ(*)
      dimension xe34(100),qx(100),qy(100),qz(100),ta34i(100),ts3(100),ts
     &4(100),ts34(100),csmcd(100)
      dimension isymop(8),iprio(8)
      common/io/In,Iout,Ipunch
      common/ffq/Fq0,Fq1,Fq2,Fq3,Fq4,Fq5
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      dimension iatm(MAXSHL),E(256)
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/fp4/Qa,Qa1,Qa2,A12i,A34i,A1234i
      common/fp4/A1,A2,A3,A4,A12,A34,A1234,Pqx,Pqy,Pqz,Pqxx,Pqyy,Pqzz,Pq
     &xy,Pqxz,Pqyz,V0000,V0010,V0020,V0030,V0100,V0200,V0300,V0110,V0120
     &,V0130,V0210,V0220,V0230,V0310,V0320,V0330,V1010,V1020,V1030,V2010
     &,V2020,V2030,V3010,V3020,V3030,V1000,V2000,V3000,V1100,V2100,V3100
     &,V1200,V2200,V3200,V1300,V2300,V3300,V1110,V2110,V3110,V1210,V2210
     &,V3210,V1310,V2310,V3310,V1120,V2120,V3120,V1220,V2220,V3220,V1320
     &,V2320,V3320,V1130,V2130,V3130,V1230,V2230,V3230,V1330,V2330,V3330
      common/fp4/C1110,C2110,C3110,C1210,C2210,C3210,C1320,C2320,C3320,C
     &1130,C2130,C3130,C1230,C2230,C3230,C1310,C2310,C3310,C1120,C2120,C
     &3120,C1220,C2220,C3220,C1330,C2330,C3330,Opxo,Opyo,Opzo,Opox,Opoy,
     &Opoz,Opxox,Opyoy,Opzoz,Opxx,Opxy,Opxz,Opyx,Opyy,Opyz,Opzx,Opzy,Opz
     &z,Oqxo,Oqyo,Oqzo,Oqox,Oqoy,Oqoz,Oqxox,Oqyoy,Oqzoz,Oqxx,Oqxy,Oqxz,O
     &qyx,Oqyy,Oqyz,Oqzx,Oqzy,Oqzz,S1,S2,S3,S4,S12,S34
      common/fp4/E,Goooo,Gooxo,Gooyo,Goozo,Gxooo,Gxoxo,Gxoyo,Gxozo,Gxxoo
     &,Gxxxo,Gxxyo,Gxxzo,Gxyoo,Gxyzo,Gxzoo,Gyooo,Gyoyo,Gyozo,Gyyoo,Gyyxo
     &,Gyyyo,Gyyzo,Gyzoo,Gzooo,Gzozo,Gzzoo,Gzzxo,Gzzyo,Gzzzo,Ve00,Ve11,V
     &e12,Ve13,Ve14,Ve21,Ve22,Ve23,Ve24,Ve31,Ve32,Ve33,Ve34,Csssp,Csspp,
     &Cspsp,Cpssp,Csppp,Cpspp,Cppsp,Cpppp
      common/dtable/Tbaa(400),Tbba(400),Tbca(400),Tbab(400),Tbbb(400),Tb
     &cb(400),Tbac(400),Tbbc(400),Tbcc(400),Tbad(400),Tbbd(400),Tbcd(400
     &),Tbae(400),Tbbe(400),Tbce(400),Tbaf(400),Tbbf(400),Tbcf(400)
      data irwtab/503/
      data three/3.0D0/
      data p25/0.25D0/
      data two/2.0D0/,h/0.5D0/,zero/0.0D0/
      data one5/1.5D0/
      data twenty/20.D0/
      data two5,three5,four5/2.5D0,3.5D0,4.5D0/
      data sxtn/16.D0/
      data one/1.0D0/,four/4.0D0/,ccut/1.0D-6/
      data tenm7/1.0D-7/,tenm4/1.0D-4/,tenm2/1.0D-2/
      
      
      
      
      
      
      
      
      
      
99001 format(' ',4I2,12F10.6)
99002 format(1x,'SYMOP',i3,18x,i2,38x,i2,38x,i2)
      
      call tread(irwtab,Tbaa(1),1200,6,1200,6,0)
      
      pi=four*gatan(one)
      pito52=two*(pi**two5)
      pidiv4=p25*pi
      trp=two/gsqrt(pi)
      
      ia=1
      iaind=0
      do 100 i=1,Nshell
      if(gabs(X(i)-C(1+iaind)).LE.ccut)then
      if(gabs(Y(i)-C(2+iaind)).LE.ccut)then
      if(gabs(Z(i)-C(3+iaind)).LE.ccut)goto 50
      endif
      endif
      
      ia=ia+1
      iaind=iaind+3
50    iatm(i)=ia
100   continue
      
      VEE=zero
      
      vtol=tenm7
      if(ICRIT.NE.0)vtol=10**(-ICRIT-3)
      vtol1=vtol*tenm4
      vtol2=vtol*tenm2
      vtols=vtol**2
      
      
      do 200 ishell=1,Nshell
      if(Shellt(ishell).LE.1)then
      do 160 jshell=1,ishell
      if(Shellt(jshell).LE.1)then
      do 140 kshell=1,ishell
      if(Shellt(kshell).LE.1)then
      do 138 lshell=1,kshell
      if(Shellt(lshell).LE.1)then
      if(kshell.EQ.ishell.AND.lshell.GT.jshell)goto 160
      ax1=h
      if(ishell.NE.jshell)ax1=ax1+ax1
      if(kshell.NE.lshell)ax1=ax1+ax1
      if(ishell.NE.kshell.OR.jshell.NE.lshell)ax1=ax1+ax1
      
      
      inew=ishell
      jnew=jshell
      knew=kshell
      lnew=lshell
      la=Shellt(inew)
      lb=Shellt(jnew)
      lc=Shellt(knew)
      ld=Shellt(lnew)
      if(la.LT.lb)then
      inew=jshell
      jnew=ishell
      endif
      if(lc.LT.ld)then
      knew=lshell
      lnew=kshell
      endif
      if(la+lb-lc.LT.ld)then
      id=inew
      inew=knew
      knew=id
      id=jnew
      jnew=lnew
      lnew=id
      endif
      la=3*Shellt(inew)+1
      lb=3*Shellt(jnew)+1
      lc=3*Shellt(knew)+1
      ld=3*Shellt(lnew)+1
      
      
      ijsame=inew.EQ.jnew
      klsame=knew.EQ.lnew
      iksmjl=(inew.EQ.knew).AND.(jnew.EQ.lnew)
      jtype=(la+lb+lc+lc+ld-2)/3
      if(jtype.GT.6)call lnk1e
      ifqmax=Shellt(inew)+Shellt(jnew)+Shellt(knew)+Shellt(lnew)+1
      iat=iatm(inew)
      jat=iatm(jnew)
      kat=iatm(knew)
      lat=iatm(lnew)
      if((iat.NE.jat).OR.(iat.NE.kat).OR.(iat.NE.lat))then
      if(USESYM)then
      
      
      numop=1
      np1=nprio(iat,jat,kat,lat)
      isymop(1)=1
      iprio(1)=np1
      jopind=0
      do 102 jop=2,NSYMOP
      jopind=jopind+NATOMS
      iat1=NEQATM(iat+jopind)
      jat1=NEQATM(jat+jopind)
      kat1=NEQATM(kat+jopind)
      lat1=NEQATM(lat+jopind)
      np2=nprio(iat1,jat1,kat1,lat1)
      if(np2.GT.np1)goto 138
      numop=numop+1
      isymop(numop)=jop
      iprio(numop)=np2
102   continue
      
      
      mop=numop-1
      do 106 jrop=1,mop
      jop=numop-jrop+1
      jopm1=jop-1
      do 104 kop=1,jopm1
      if(iprio(kop).EQ.iprio(jop))then
      isymop(jop)=0
      goto 106
      endif
      
104   continue
106   continue
      endif
      
      iatx=3*(iat-1)+1
      jatx=3*(jat-1)+1
      katx=3*(kat-1)+1
      latx=3*(lat-1)+1
      iaty=iatx+1
      jaty=jatx+1
      katy=katx+1
      laty=latx+1
      iatz=iaty+1
      jatz=jaty+1
      katz=katy+1
      latz=laty+1
      ax=X(inew)
      bx=X(jnew)
      cx=X(knew)
      dx=X(lnew)
      ay=Y(inew)
      by=Y(jnew)
      cy=Y(knew)
      dy=Y(lnew)
      az=Z(inew)
      bz=Z(jnew)
      cz=Z(knew)
      dz=Z(lnew)
      abx=ax-bx
      aby=ay-by
      abz=az-bz
      cdx=cx-dx
      cdy=cy-dy
      cdz=cz-dz
      r34=cdx**2+cdy**2+cdz**2
      r12=abx**2+aby**2+abz**2
      istart=Shella(inew)
      jstart=Shella(jnew)
      kstart=Shella(knew)
      lstart=Shella(lnew)
      iend=istart+Shelln(inew)-1
      jend=jstart+Shelln(jnew)-1
      kend=kstart+Shelln(knew)-1
      lend=lstart+Shelln(lnew)-1
      
      call efill(inew,jnew,knew,lnew,la,lb,lc,ld,ax1,ISCF,DM,DN,E,dmax)
      
      if(dmax.GE.vtol1)then
      
      e34max=zero
      kzero=-10
      do 110 k=kstart,kend
      kzero=kzero+10
      kl=kzero
      A3=Exx(k)
      csumc=gabs(C1(k))+gabs(C2(k))
      lnd=lend
      if(klsame)lnd=k
      do 108 l=lstart,lnd
      A4=Exx(l)
      kl=kl+1
      A34=A3+A4
      A34i=one/A34
      S3=A3*A34i
      S4=A4*A34i
      S34=A3*S4
      qx(kl)=S3*cx+S4*dx
      qy(kl)=S3*cy+S4*dy
      qz(kl)=S3*cz+S4*dz
      ta34i(kl)=A34i
      ts3(kl)=S3
      ts4(kl)=S4
      ts34(kl)=S34
      e34=gexp(-r34*S34)*A34i
      if(klsame.AND.(k.NE.l))e34=e34+e34
      xe34(kl)=e34
      e34=e34*csumc*(gabs(C1(l))+gabs(C2(l)))
      if(e34.GT.e34max)e34max=e34
      csmcd(kl)=e34**2
108   continue
110   continue
      if(dmax*e34max.GE.vtol1)then
      fxi=zero
      fxj=zero
      fxk=zero
      fxl=zero
      fyi=zero
      fyj=zero
      fyk=zero
      fyl=zero
      fzi=zero
      fzj=zero
      fzk=zero
      fzl=zero
      Ve11=zero
      Ve12=zero
      Ve13=zero
      Ve14=zero
      Ve21=zero
      Ve22=zero
      Ve23=zero
      Ve24=zero
      Ve31=zero
      Ve32=zero
      Ve33=zero
      Ve34=zero
      
      
      izero=-10
      
      do 134 i=istart,iend
      izero=izero+10
      ij=izero
      A1=Exx(i)
      csa=C1(i)
      cpa=C2(i)
      csuma=(gabs(csa)+gabs(cpa))*dmax
      jnd=jend
      if(ijsame)jnd=i
      
      do 132 j=jstart,jnd
      ij=ij+1
      A2=Exx(j)
      A12=A1+A2
      A12i=one/A12
      S1=A1*A12i
      S2=A2*A12i
      S12=A1*S2
      e12=gexp(-r12*S12)*pito52*A12i
      if(ijsame.AND.(i.NE.j))e12=e12+e12
      csb=C1(j)*e12
      cpb=C2(j)*e12
      csmab=csuma*(gabs(csb)+gabs(cpb))
      if(csmab*e34max.GE.vtol2)then
      csmab=csmab**2
      css=csa*csb
      px=S1*ax+S2*bx
      py=S1*ay+S2*by
      pz=S1*az+S2*bz
      if(la.NE.1)then
      cps=cpa*csb
      Opxo=-S2*abx
      Opyo=-S2*aby
      Opzo=-S2*abz
      if(lb.NE.1)then
      csp=csa*cpb
      cpp=cpa*cpb
      Opox=S1*abx
      Opoy=S1*aby
      Opoz=S1*abz
      Opxox=Opxo+Opox
      Opyoy=Opyo+Opoy
      Opzoz=Opzo+Opoz
      ha12i=h*A12i
      Opxx=Opxo*Opox+ha12i
      Opyy=Opyo*Opoy+ha12i
      Opzz=Opzo*Opoz+ha12i
      Opxy=Opxo*Opoy
      Opyx=Opxy
      Opxz=Opxo*Opoz
      Opzx=Opxz
      Opyz=Opyo*Opoz
      Opzy=Opyz
      endif
      endif
      ve00s=zero
      ve11s=zero
      ve12s=zero
      ve21s=zero
      ve22s=zero
      ve31s=zero
      ve32s=zero
      dvexs=zero
      dveys=zero
      dvezs=zero
      knd=kend
      if(iksmjl)knd=i
      kzero=-10
      
      do 130 k=kstart,knd
      kzero=kzero+10
      kl=kzero
      A3=Exx(k)
      csc=C1(k)
      cpc=C2(k)
      csss=css*csc
      cssp=css*cpc
      cpss=cps*csc
      cpsp=cps*cpc
      csps=csp*csc
      cspp=csp*cpc
      cpps=cpp*csc
      cppp=cpp*cpc
      lnd=lend
      if(klsame)lnd=k
      if(iksmjl.AND.(i.EQ.k))lnd=j
      
      do 128 l=lstart,lnd
      kl=kl+1
      A34=A3+Exx(l)
      A1234=A12+A34
      A1234i=one/A1234
      vtest=csmab*csmcd(kl)*A1234i
      if(vtest.LT.vtols)goto 128
      Qa2=A34*A1234i
      Qa=A12*Qa2
      Pqx=px-qx(kl)
      Pqy=py-qy(kl)
      Pqz=pz-qz(kl)
      Pqxx=Pqx*Pqx
      Pqyy=Pqy*Pqy
      Pqzz=Pqz*Pqz
      t=Qa*(Pqxx+Pqyy+Pqzz)
      A34i=ta34i(kl)
      S3=ts3(kl)
      S4=ts4(kl)
      S34=ts34(kl)
      e34=xe34(kl)*gsqrt(A1234i)
      if(iksmjl.AND.(ij.NE.kl))e34=e34+e34
      csd=C1(l)*e34
      eoooo=E(1)*csss*csd
      if(t.LT.sxtn)then
      
      qq=t*twenty
      theta=qq-dint(qq)
      n=qq-theta
      theta2=theta*(theta-one)
      theta3=theta2*(theta-two)
      theta4=theta2*(theta+one)
      if(ifqmax.EQ.1)goto 114
      if(ifqmax.EQ.2)then
      Fq2=Tbac(n+1)+theta*Tbbc(n+1)-theta3*Tbcc(n+1)+theta4*Tbcc(n+2)
      goto 114
      elseif(ifqmax.EQ.3)then
      goto 112
      elseif(ifqmax.NE.4)then
      
      Fq5=Tbaf(n+1)+theta*Tbbf(n+1)-theta3*Tbcf(n+1)+theta4*Tbcf(n+2)
      endif
      Fq4=Tbae(n+1)+theta*Tbbe(n+1)-theta3*Tbce(n+1)+theta4*Tbce(n+2)
112   Fq3=Tbad(n+1)+theta*Tbbd(n+1)-theta3*Tbcd(n+1)+theta4*Tbcd(n+2)
      Fq2=Tbac(n+1)+theta*Tbbc(n+1)-theta3*Tbcc(n+1)+theta4*Tbcc(n+2)
      else
      ti=one/t
      if(vtest*ti.LT.vtols)goto 128
      Fq0=gsqrt(pidiv4*ti)
      Fq1=h*Fq0*ti
      if(jtype.EQ.1)goto 122
      Fq2=one5*Fq1*ti
      if(jtype.EQ.2)goto 124
      Fq3=two5*Fq2*ti
      Fq4=three5*Fq3*ti
      Fq5=four5*Fq4*ti
      goto 116
      endif
114   Fq1=Tbab(n+1)+theta*Tbbb(n+1)-theta3*Tbcb(n+1)+theta4*Tbcb(n+2)
      Fq0=Tbaa(n+1)+theta*Tbba(n+1)-theta3*Tbca(n+1)+theta4*Tbca(n+2)
116   if(jtype.EQ.1)goto 122
      if(jtype.EQ.2)goto 124
      Pqxy=Pqx*Pqy
      Pqyz=Pqy*Pqz
      Pqxz=Pqx*Pqz
      Qa1=A12*A1234i
      cpd=C2(l)*e34
      if(lc.NE.1)then
      cssps=cssp*csd
      cpsps=cpsp*csd
      Oqxo=-S4*cdx
      Oqyo=-S4*cdy
      Oqzo=-S4*cdz
      if(ld.NE.1)then
      Csssp=csss*cpd
      Csspp=cssp*cpd
      Cspsp=csps*cpd
      Cpssp=cpss*cpd
      Csppp=cspp*cpd
      Cpspp=cpsp*cpd
      Cppsp=cpps*cpd
      Cpppp=cppp*cpd
      Oqox=S3*cdx
      Oqoy=S3*cdy
      Oqoz=S3*cdz
      Oqxx=Oqxo*Oqox+h*A34i
      Oqyy=Oqyo*Oqoy+h*A34i
      Oqzz=Oqzo*Oqoz+h*A34i
      Oqxy=Oqxo*Oqoy
      Oqxz=Oqxo*Oqoz
      Oqyx=Oqxy
      Oqyz=Oqyo*Oqoz
      Oqzx=Oqxz
      Oqzy=Oqyz
      Oqxox=Oqxo+Oqox
      Oqyoy=Oqyo+Oqoy
      Oqzoz=Oqzo+Oqoz
      endif
      endif
      
      
      flag=.FALSE.
      
      
      
118   Goooo=Fq0
      V0000=Goooo
      Ve00=V0000*eoooo
      if(jtype.GE.2)then
      
      
      qfq1=-Qa2*Fq1
      Gxooo=Pqx*qfq1
      V1000=Opxo*Goooo+Gxooo
      Gyooo=Pqy*qfq1
      V2000=Opyo*Goooo+Gyooo
      Gzooo=Pqz*qfq1
      V3000=Opzo*Goooo+Gzooo
      cpsss=cpss*csd
      Ve00=Ve00+(V1000*E(65)+V2000*E(129)+V3000*E(193))*cpsss
      temp=V0000*cpsss
      Ve11=temp*E(65)
      Ve21=temp*E(129)
      Ve31=temp*E(193)
      if(jtype.LT.3)goto 120
      if(jtype.NE.3)then
      
      
      qfq1=Qa1*Fq1
      Gooxo=Pqx*qfq1
      V0010=Oqxo*Goooo+Gooxo
      Gooyo=Pqy*qfq1
      V0020=Oqyo*Goooo+Gooyo
      Goozo=Pqz*qfq1
      V0030=Oqzo*Goooo+Goozo
      hfq1=h*Fq1*A1234i
      qfq2=-Qa*Fq2*A1234i
      Gxoxo=Pqxx*qfq2+hfq1
      V1010=Opxo*V0010+Oqxo*Gxooo+Gxoxo
      Gyoyo=Pqyy*qfq2+hfq1
      V2020=Opyo*V0020+Oqyo*Gyooo+Gyoyo
      Gzozo=Pqzz*qfq2+hfq1
      V3030=Opzo*V0030+Oqzo*Gzooo+Gzozo
      Gxoyo=Pqxy*qfq2
      V1020=Opxo*V0020+Oqyo*Gxooo+Gxoyo
      V2010=Opyo*V0010+Oqxo*Gyooo+Gxoyo
      Gxozo=Pqxz*qfq2
      V1030=Opxo*V0030+Oqzo*Gxooo+Gxozo
      V3010=Opzo*V0010+Oqxo*Gzooo+Gxozo
      Gyozo=Pqyz*qfq2
      V2030=Opyo*V0030+Oqzo*Gyooo+Gyozo
      V3020=Opzo*V0020+Oqyo*Gzooo+Gyozo
      Ve00=Ve00+(V0010*E(5)+V0020*E(9)+V0030*E(13))*cssps
      temp=V0000*cssps
      Ve13=temp*E(5)
      Ve23=temp*E(9)
      Ve33=temp*E(13)
      Ve00=Ve00+(V1010*E(69)+V1020*E(73)+V1030*E(77)+V2010*E(133)+V2020*
     &E(137)+V2030*E(141)+V3010*E(197)+V3020*E(201)+V3030*E(205))*cpsps
      Ve11=Ve11+(V0010*E(69)+V0020*E(73)+V0030*E(77))*cpsps
      Ve13=Ve13+(V1000*E(69)+V2000*E(133)+V3000*E(197))*cpsps
      Ve21=Ve21+(V0010*E(133)+V0020*E(137)+V0030*E(141))*cpsps
      Ve23=Ve23+(V1000*E(73)+V2000*E(137)+V3000*E(201))*cpsps
      Ve31=Ve31+(V0010*E(197)+V0020*E(201)+V0030*E(205))*cpsps
      Ve33=Ve33+(V1000*E(77)+V2000*E(141)+V3000*E(205))*cpsps
      if(jtype.EQ.4)goto 120
      endif
      
      
      V0100=Opox*Goooo+Gxooo
      V0200=Opoy*Goooo+Gyooo
      V0300=Opoz*Goooo+Gzooo
      cspss=csps*csd
      Ve00=Ve00+(V0100*E(17)+V0200*E(33)+V0300*E(49))*cspss
      temp=V0000*cspss
      Ve12=temp*E(17)
      Ve22=temp*E(33)
      Ve32=temp*E(49)
      hfq1=-h*Qa2*Fq1*A12i
      qfq2=Qa2*Qa2*Fq2
      Gxxoo=Pqxx*qfq2+hfq1
      V1100=Opxx*Goooo+Opxox*Gxooo+Gxxoo
      Gyyoo=Pqyy*qfq2+hfq1
      V2200=Opyy*Goooo+Opyoy*Gyooo+Gyyoo
      Gzzoo=Pqzz*qfq2+hfq1
      V3300=Opzz*Goooo+Opzoz*Gzooo+Gzzoo
      Gxyoo=Pqxy*qfq2
      V1200=Opxo*V0200+Opoy*Gxooo+Gxyoo
      V2100=Opyo*V0100+Opox*Gyooo+Gxyoo
      Gxzoo=Pqxz*qfq2
      V1300=Opxo*V0300+Opoz*Gxooo+Gxzoo
      V3100=Opzo*V0100+Opox*Gzooo+Gxzoo
      Gyzoo=Pqyz*qfq2
      V2300=Opyo*V0300+Opoz*Gyooo+Gyzoo
      V3200=Opzo*V0200+Opoy*Gzooo+Gyzoo
      cppss=cpps*csd
      Ve00=Ve00+(V1100*E(81)+V1200*E(97)+V1300*E(113)+V2100*E(145)+V2200
     &*E(161)+V2300*E(177)+V3100*E(209)+V3200*E(225)+V3300*E(241))*cppss
      Ve11=Ve11+(V0100*E(81)+V0200*E(97)+V0300*E(113))*cppss
      Ve21=Ve21+(V0100*E(145)+V0200*E(161)+V0300*E(177))*cppss
      Ve31=Ve31+(V0100*E(209)+V0200*E(225)+V0300*E(241))*cppss
      Ve12=Ve12+(V1000*E(81)+V2000*E(145)+V3000*E(209))*cppss
      Ve22=Ve22+(V1000*E(97)+V2000*E(161)+V3000*E(225))*cppss
      Ve32=Ve32+(V1000*E(113)+V2000*E(177)+V3000*E(241))*cppss
      if(jtype.NE.3)then
      
      
      V0110=Oqxo*V0100+Opox*Gooxo+Gxoxo
      V0120=Oqyo*V0100+Opox*Gooyo+Gxoyo
      V0130=Oqzo*V0100+Opox*Goozo+Gxozo
      V0210=Oqxo*V0200+Opoy*Gooxo+Gxoyo
      V0220=Oqyo*V0200+Opoy*Gooyo+Gyoyo
      V0230=Oqzo*V0200+Opoy*Goozo+Gyozo
      V0310=Oqxo*V0300+Opoz*Gooxo+Gxozo
      V0320=Oqyo*V0300+Opoz*Gooyo+Gyozo
      V0330=Oqzo*V0300+Opoz*Goozo+Gzozo
      cspps=cspp*csd
      Ve00=Ve00+(V0110*E(21)+V0120*E(25)+V0130*E(29)+V0210*E(37)+V0220*E
     &(41)+V0230*E(45)+V0310*E(53)+V0320*E(57)+V0330*E(61))*cspps
      Ve12=Ve12+(V0010*E(21)+V0020*E(25)+V0030*E(29))*cspps
      Ve22=Ve22+(V0010*E(37)+V0020*E(41)+V0030*E(45))*cspps
      Ve32=Ve32+(V0010*E(53)+V0020*E(57)+V0030*E(61))*cspps
      Ve13=Ve13+(V0100*E(21)+V0200*E(37)+V0300*E(53))*cspps
      Ve23=Ve23+(V0100*E(25)+V0200*E(41)+V0300*E(57))*cspps
      Ve33=Ve33+(V0100*E(29)+V0200*E(45)+V0300*E(61))*cspps
      qfq3=Qa1*Qa2*Qa2*Fq3
      hfq2=-h*Qa2*Fq2*A1234i
      tfq2=three*hfq2
      Gxyzo=Pqxy*Pqz*qfq3
      C1230=Opxy*Goozo+Opxo*Gyozo+Opoy*Gxozo+Gxyzo
      V1230=Oqzo*V1200+C1230
      C1320=Opxz*Gooyo+Opxo*Gyozo+Opoz*Gxoyo+Gxyzo
      V1320=Oqyo*V1300+C1320
      C2130=Opyx*Goozo+Opyo*Gxozo+Opox*Gyozo+Gxyzo
      V2130=Oqzo*V2100+C2130
      C2310=Opyz*Gooxo+Opyo*Gxozo+Opoz*Gxoyo+Gxyzo
      V2310=Oqxo*V2300+C2310
      C3120=Opzx*Gooyo+Opzo*Gxoyo+Opox*Gyozo+Gxyzo
      V3120=Oqyo*V3100+C3120
      C3210=Opzy*Gooxo+Opzo*Gxoyo+Opoy*Gxozo+Gxyzo
      V3210=Oqxo*V3200+C3210
      temp=Pqxx*qfq3
      Gxxxo=Pqx*(temp+tfq2)
      C1110=Opxx*Gooxo+Opxox*Gxoxo+Gxxxo
      V1110=Oqxo*V1100+C1110
      Gxxyo=Pqy*(temp+hfq2)
      C1120=Opxx*Gooyo+Opxox*Gxoyo+Gxxyo
      V1120=Oqyo*V1100+C1120
      C1210=Opxy*Gooxo+Opxo*Gxoyo+Opoy*Gxoxo+Gxxyo
      V1210=Oqxo*V1200+C1210
      C2110=Opyx*Gooxo+Opyo*Gxoxo+Opox*Gxoyo+Gxxyo
      V2110=Oqxo*V2100+C2110
      Gxxzo=Pqz*(temp+hfq2)
      C1130=Opxx*Goozo+Opxox*Gxozo+Gxxzo
      V1130=Oqzo*V1100+C1130
      C1310=Opxz*Gooxo+Opxo*Gxozo+Opoz*Gxoxo+Gxxzo
      V1310=Oqxo*V1300+C1310
      C3110=Opzx*Gooxo+Opzo*Gxoxo+Opox*Gxozo+Gxxzo
      V3110=Oqxo*V3100+C3110
      temp=Pqyy*qfq3
      Gyyyo=Pqy*(temp+tfq2)
      C2220=Opyy*Gooyo+Opyoy*Gyoyo+Gyyyo
      V2220=Oqyo*V2200+C2220
      Gyyxo=Pqx*(temp+hfq2)
      C2210=Opyy*Gooxo+Opyoy*Gxoyo+Gyyxo
      V2210=Oqxo*V2200+C2210
      C2120=Opyx*Gooyo+Opyo*Gxoyo+Opox*Gyoyo+Gyyxo
      V2120=Oqyo*V2100+C2120
      C1220=Opxy*Gooyo+Opxo*Gyoyo+Opoy*Gxoyo+Gyyxo
      V1220=Oqyo*V1200+C1220
      Gyyzo=Pqz*(temp+hfq2)
      C2230=Opyy*Goozo+Opyoy*Gyozo+Gyyzo
      V2230=Oqzo*V2200+C2230
      C2320=Opyz*Gooyo+Opyo*Gyozo+Opoz*Gyoyo+Gyyzo
      V2320=Oqyo*V2300+C2320
      C3220=Opzy*Gooyo+Opzo*Gyoyo+Opoy*Gyozo+Gyyzo
      V3220=Oqyo*V3200+C3220
      temp=Pqzz*qfq3
      Gzzzo=Pqz*(temp+tfq2)
      C3330=Opzz*Goozo+Opzoz*Gzozo+Gzzzo
      V3330=Oqzo*V3300+C3330
      Gzzxo=Pqx*(temp+hfq2)
      C3310=Opzz*Gooxo+Opzoz*Gxozo+Gzzxo
      V3310=Oqxo*V3300+C3310
      C3130=Opzx*Goozo+Opzo*Gxozo+Opox*Gzozo+Gzzxo
      V3130=Oqzo*V3100+C3130
      C1330=Opxz*Goozo+Opxo*Gzozo+Opoz*Gxozo+Gzzxo
      V1330=Oqzo*V1300+C1330
      Gzzyo=Pqy*(temp+hfq2)
      C3320=Opzz*Gooyo+Opzoz*Gyozo+Gzzyo
      V3320=Oqyo*V3300+C3320
      C3230=Opzy*Goozo+Opzo*Gyozo+Opoy*Gzozo+Gzzyo
      V3230=Oqzo*V3200+C3230
      C2330=Opyz*Goozo+Opyo*Gzozo+Opoz*Gyozo+Gzzyo
      V2330=Oqzo*V2300+C2330
      cppps=cppp*csd
      Ve00=Ve00+(V1110*E(85)+V1120*E(89)+V1130*E(93)+V1210*E(101)+V1220*
     &E(105)+V1230*E(109)+V1310*E(117)+V1320*E(121)+V1330*E(125)+V2110*E
     &(149)+V2120*E(153)+V2130*E(157)+V2210*E(165)+V2220*E(169)+V2230*E(
     &173)+V2310*E(181)+V2320*E(185)+V2330*E(189)+V3110*E(213)+V3120*E(2
     &17)+V3130*E(221)+V3210*E(229)+V3220*E(233)+V3230*E(237)+V3310*E(24
     &5)+V3320*E(249)+V3330*E(253))*cppps
      Ve11=Ve11+(V0110*E(85)+V0120*E(89)+V0130*E(93)+V0210*E(101)+V0220*
     &E(105)+V0230*E(109)+V0310*E(117)+V0320*E(121)+V0330*E(125))*cppps
      Ve12=Ve12+(V1010*E(85)+V1020*E(89)+V1030*E(93)+V2010*E(149)+V2020*
     &E(153)+V2030*E(157)+V3010*E(213)+V3020*E(217)+V3030*E(221))*cppps
      Ve13=Ve13+(V1100*E(85)+V1200*E(101)+V1300*E(117)+V2100*E(149)+V220
     &0*E(165)+V2300*E(181)+V3100*E(213)+V3200*E(229)+V3300*E(245))*cppp
     &s
      Ve21=Ve21+(V0110*E(149)+V0120*E(153)+V0130*E(157)+V0210*E(165)+V02
     &20*E(169)+V0230*E(173)+V0310*E(181)+V0320*E(185)+V0330*E(189))*cpp
     &ps
      Ve22=Ve22+(V1010*E(101)+V1020*E(105)+V1030*E(109)+V2010*E(165)+V20
     &20*E(169)+V2030*E(173)+V3010*E(229)+V3020*E(233)+V3030*E(237))*cpp
     &ps
      Ve23=Ve23+(V1100*E(89)+V1200*E(105)+V1300*E(121)+V2100*E(153)+V220
     &0*E(169)+V2300*E(185)+V3100*E(217)+V3200*E(233)+V3300*E(249))*cppp
     &s
      Ve31=Ve31+(V0110*E(213)+V0120*E(217)+V0130*E(221)+V0210*E(229)+V02
     &20*E(233)+V0230*E(237)+V0310*E(245)+V0320*E(249)+V0330*E(253))*cpp
     &ps
      Ve32=Ve32+(V1010*E(117)+V1020*E(121)+V1030*E(125)+V2010*E(181)+V20
     &20*E(185)+V2030*E(189)+V3010*E(245)+V3020*E(249)+V3030*E(253))*cpp
     &ps
      Ve33=Ve33+(V1100*E(93)+V1200*E(109)+V1300*E(125)+V2100*E(157)+V220
     &0*E(173)+V2300*E(189)+V3100*E(221)+V3200*E(237)+V3300*E(253))*cppp
     &s
      
      
      if(jtype.EQ.6)call fpppp
      endif
      endif
120   if(flag)then
      
      
      qve00=Qa*(Ve00+Ve00)
      dvex=-(Ve11+Ve12)*Qa2+(Ve13+Ve14)*Qa1-Pqx*qve00
      dvey=-(Ve21+Ve22)*Qa2+(Ve23+Ve24)*Qa1-Pqy*qve00
      dvez=-(Ve31+Ve32)*Qa2+(Ve33+Ve34)*Qa1-Pqz*qve00
      dvexs=dvexs+dvex
      dveys=dveys+dvey
      dvezs=dvezs+dvez
      goto 126
      else
      
      
      flag=.TRUE.
      Fq0=Fq1
      Fq1=Fq2
      Fq2=Fq3
      Fq3=Fq4
      Fq4=Fq5
      ve00s=ve00s+Ve00
      ve11s=ve11s+Ve11
      ve21s=ve21s+Ve21
      ve31s=ve31s+Ve31
      ve12s=ve12s+Ve12
      ve22s=ve22s+Ve22
      ve32s=ve32s+Ve32
      cdve00=S34*(Ve00+Ve00)
      dx2x=-Ve13*S4+Ve14*S3-cdx*cdve00
      dx2y=-Ve23*S4+Ve24*S3-cdy*cdve00
      dx2z=-Ve33*S4+Ve34*S3-cdz*cdve00
      
      
      goto 118
      endif
      
      
122   Ve00=Fq0*eoooo
      ve00s=ve00s+Ve00
      cdve00=S34*(Ve00+Ve00)
      dx2x=-cdx*cdve00
      dx2y=-cdy*cdve00
      dx2z=-cdz*cdve00
      qve00=Qa*Fq1*(eoooo+eoooo)
      dvex=-Pqx*qve00
      dvey=-Pqy*qve00
      dvez=-Pqz*qve00
      dvexs=dvexs+dvex
      dveys=dveys+dvey
      dvezs=dvezs+dvez
      goto 126
      
      
124   cpsss=cpss*csd
      exooo=E(65)*cpsss
      eyooo=E(129)*cpsss
      ezooo=E(193)*cpsss
      temp1=Opxo*exooo+Opyo*eyooo+Opzo*ezooo
      temp2=-(Pqx*exooo+Pqy*eyooo+Pqz*ezooo)*Qa2
      Ve00=Fq0*(eoooo+temp1)+Fq1*temp2
      ve00s=ve00s+Ve00
      ve11s=ve11s+Fq0*exooo
      ve21s=ve21s+Fq0*eyooo
      ve31s=ve31s+Fq0*ezooo
      cdve00=S34*(Ve00+Ve00)
      dx2x=-cdx*cdve00
      dx2y=-cdy*cdve00
      dx2z=-cdz*cdve00
      Ve00=Fq1*(eoooo+temp1)+Fq2*temp2
      qve00=Qa*(Ve00+Ve00)
      temp=-Qa2*Fq1
      dvex=-Pqx*qve00+temp*exooo
      dvey=-Pqy*qve00+temp*eyooo
      dvez=-Pqz*qve00+temp*ezooo
      dvexs=dvexs+dvex
      dveys=dveys+dvey
      dvezs=dvezs+dvez
      
      
126   fxk=fxk+dx2x-dvex*S3
      fyk=fyk+dx2y-dvey*S3
      fzk=fzk+dx2z-dvez*S3
128   continue
130   continue
      VEE=VEE+ve00s
      abve00=S12*(ve00s+ve00s)
      dx1x=-ve11s*S2+ve12s*S1-abx*abve00
      dx1y=-ve21s*S2+ve22s*S1-aby*abve00
      dx1z=-ve31s*S2+ve32s*S1-abz*abve00
      fxi=fxi+dx1x+dvexs*S1
      fyi=fyi+dx1y+dveys*S1
      fzi=fzi+dx1z+dvezs*S1
      fxj=fxj-dx1x+dvexs*S2
      fyj=fyj-dx1y+dveys*S2
      fzj=fzj-dx1z+dvezs*S2
      endif
132   continue
134   continue
      
      
      fxl=-(fxi+fxj+fxk)
      fyl=-(fyi+fyj+fyk)
      fzl=-(fzi+fzj+fzk)
      FXYZ(iatx)=FXYZ(iatx)+fxi
      FXYZ(jatx)=FXYZ(jatx)+fxj
      FXYZ(katx)=FXYZ(katx)+fxk
      FXYZ(latx)=FXYZ(latx)+fxl
      FXYZ(iaty)=FXYZ(iaty)+fyi
      FXYZ(jaty)=FXYZ(jaty)+fyj
      FXYZ(katy)=FXYZ(katy)+fyk
      FXYZ(laty)=FXYZ(laty)+fyl
      FXYZ(iatz)=FXYZ(iatz)+fzi
      FXYZ(jatz)=FXYZ(jatz)+fzj
      FXYZ(katz)=FXYZ(katz)+fzk
      FXYZ(latz)=FXYZ(latz)+fzl
      if(IDUMP.GE.2)write(Iout,99001)ishell,jshell,kshell,lshell,fxi,fxj
     &,fxk,fxl,fyi,fyj,fyk,fyl,fzi,fzj,fzk,fzl
      if(USESYM)then
      
      
      do 136 kop=2,NSYMOP
      jop=isymop(kop)
      if(jop.NE.0)then
      jopind=(jop-1)*NATOMS
      iat1=NEQATM(iat+jopind)
      jat1=NEQATM(jat+jopind)
      kat1=NEQATM(kat+jopind)
      lat1=NEQATM(lat+jopind)
      iatx=3*iat1-2
      jatx=3*jat1-2
      katx=3*kat1-2
      latx=3*lat1-2
      iaty=iatx+1
      jaty=jatx+1
      katy=katx+1
      laty=latx+1
      iatz=iaty+1
      jatz=jaty+1
      katz=katy+1
      latz=laty+1
      ixtr=JTRANS(1,jop)
      iytr=JTRANS(2,jop)
      iztr=JTRANS(3,jop)
      FXYZ(iatx)=FXYZ(iatx)+fxi*ixtr
      FXYZ(jatx)=FXYZ(jatx)+fxj*ixtr
      FXYZ(katx)=FXYZ(katx)+fxk*ixtr
      FXYZ(latx)=FXYZ(latx)+fxl*ixtr
      FXYZ(iaty)=FXYZ(iaty)+fyi*iytr
      FXYZ(jaty)=FXYZ(jaty)+fyj*iytr
      FXYZ(katy)=FXYZ(katy)+fyk*iytr
      FXYZ(laty)=FXYZ(laty)+fyl*iytr
      FXYZ(iatz)=FXYZ(iatz)+fzi*iztr
      FXYZ(jatz)=FXYZ(jatz)+fzj*iztr
      FXYZ(katz)=FXYZ(katz)+fzk*iztr
      FXYZ(latz)=FXYZ(latz)+fzl*iztr
      if(IDUMP.GE.2)write(Iout,99002)jop,ixtr,iytr,iztr
      endif
136   continue
      endif
      endif
      endif
      endif
      endif
138   continue
      endif
140   continue
      endif
160   continue
      endif
200   continue
      
      
      return
      
      end
C* :1 * 
      
