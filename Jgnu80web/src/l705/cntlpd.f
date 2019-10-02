
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cntlpd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cntlpd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 36 "cntlpd.web"
      subroutine cntlpd(SP,XNORM,QQ,MAXI2,MAXP2,IFFC)
      implicit none
      real*8 Ca,Ca2,Cax,Cay,Caz,Cb,Cb2,Cbx,Cby,Cbz,Cca,Ccb,Ccdum,Clp,D12
     &,D12c,Dva,Dvb,Dvc,fouraa
      real*8 fourab,fourbb,Fpi,gsqrt,Pi,Pi3haf,Pi5hf2,Piqurt,QQ,SP,Sqpi,
     &Sqpi2,tdva,tdvb,two,twoa,twob,Twopi,Xa,Xb
      real*8 Xc,Xint,XNORM,Ya,Yb,Yc,Za,Zb,Zc,Zero1,Zlp
      integer i,i1,i2,i3,Iatom,Idp,idps2,Iend,IFFC,ifind,igauss,Igbegn,I
     &gdf,Igend,ii,iii,Imj,ind,Indjx,Indjy
      integer Indjz,Indlp,intc,intps,iprim,Irange,Istart,Itype,ix,ixm,ix
     &m2,ixp,ixp2,iy,iym,iym2,iyp,iyp2,iz,izm
      integer izm2,izp,izp2,j,j1,j2,j3,Jend,jgauss,Jgbegn,Jgdf,Jgend,jnd
     &,Jnktyp,Jrange,Jstart,Jtype,jx,jxm,jxm2
      integer jxp,jxp2,jy,jym,jym2,jyp,jyp2,jz,jzm,jzm2,jzp,jzp2,k,Kfirs
     &t,Klast,Lamax,Lbmax,Lbound,lcol,ld
      integer Ldum,Lentq,Lmax,Lpmax,Lpskip,MAXATM,Maxdum,MAXI2,MAXP2,N10
     &ord,N5ord,N6ord,N7ord,Nfroz,Nlp,Nordr,nprim,Ntpse
      parameter(MAXATM=100)
      integer Ubound,Ulpure
      integer tistrt,tiend,tjstrt,tjend,tjnd,tirnge,tjrnge
      integer tlamax,tlbmax,tlpmax
      
      
      common/pseud/Ntpse(7,MAXATM)
      common/lp2/Nlp(400),Clp(400),Zlp(400),Kfirst(35,5),Klast(35,5),Lma
     &x(35),Lpskip(35),Nfroz(35)
      common/centre/Xa,Ya,Za,Xb,Yb,Zb,Xc,Yc,Zc,Iatom
      common/dist/Cax,Cay,Caz,Ca,Ca2,Cbx,Cby,Cbz,Cb,Cb2
      common/pifac/Pi,Twopi,Fpi,Pi3haf,Pi5hf2,Piqurt,Sqpi,Sqpi2
      common/ndex/Indjx(35),Indjy(35),Indjz(35),Indlp(20)
      common/dndex/Idp(5,5,5)
      common/contr/Cca(20),Ccb(20),Ccdum(40)
      common/prims/Igbegn,Igend,Jgbegn,Jgend,Igdf,Jgdf
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Ldum(
     &11)
      common/type/Itype,Jtype,Jnktyp(10)
      common/dens/D12(100),D12c(100)
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/int/Zero1,Xint(12)
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
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/dint/Dvc(3),Dva(3),Dvb(3)
      dimension SP(MAXP2,MAXI2),XNORM(MAXP2,MAXI2),QQ(*)
      dimension tdva(3),tdvb(3)
      save two
      data two/2.0D0/
      
      ifind(i1,i2,i3,j1,j2,j3,lcol)=(Idp(i1,i2,i3)-1)*lcol+Idp(j1,j2,j3)
      
      Cax=Xc-Xa
      Cay=Yc-Ya
      Caz=Zc-Za
      Ca2=Cax*Cax+Cay*Cay+Caz*Caz
      Cbx=Xc-Xb
      Cby=Yc-Yb
      Cbz=Zc-Zb
      Cb2=Cbx*Cbx+Cby*Cby+Cbz*Cbz
      Ca=gsqrt(Ca2)
      Cb=gsqrt(Cb2)
      
      call aclear(MAXI2*MAXP2,SP)
      iprim=0
      do 100 igauss=Igbegn,Igend
      call fillc(Itype,Igbegn,igauss,Igdf,Cca)
      do 50 jgauss=Jgbegn,Jgend
      call fillc(Jtype,Jgbegn,jgauss,Jgdf,Ccb)
      iprim=iprim+1
      intc=0
      do 20 ii=Istart,Iend
      jnd=Jend
      if(Imj.EQ.0)jnd=ii
      do 10 iii=Jstart,jnd
      intc=intc+1
      XNORM(iprim,intc)=Fpi*Cca(ii)*Ccb(iii)*D12(intc)
10    continue
20    continue
50    continue
100   continue
      
      tistrt=Istart
      tjstrt=Jstart
      tiend=Iend
      tjend=Jend
      tirnge=Irange
      tjrnge=Jrange
      tlamax=Lamax
      tlbmax=Lbmax
      tlpmax=Lpmax
      ld=1
      Lamax=Lamax+ld
      Lbmax=Lbmax+ld
      Lpmax=Lamax+Lbmax-1
      Istart=1
      if(Lamax.LE.4)then
      Iend=Ubound(Lamax)
      else
      Iend=35
      endif
      Jstart=1
      if(Lbmax.LE.4)then
      Jend=Ubound(Lbmax)
      else
      Jend=35
      endif
      Irange=Iend-Istart+1
      Jrange=Jend-Jstart+1
      Lentq=Irange*Jrange
      
      nprim=(Igend-Igbegn+1)*(Jgend-Jgbegn+1)
      idps2=Kfirst(Iatom,1)
      intps=Klast(Iatom,1)-Kfirst(Iatom,1)+1
      call pseud1(intps,Nlp(idps2),Zlp(idps2),Clp(idps2),SP,XNORM,MAXI2,
     &MAXP2)
      call pseud2(Lmax(Iatom),Ntpse(1,Iatom),Nlp,Zlp,Clp,SP,XNORM,QQ,MAX
     &I2,MAXP2)
      
      call aclear(9,Dvc)
      
      intc=0
      do 200 i=tistrt,tiend
      ix=Indjx(i)
      iy=Indjy(i)
      iz=Indjz(i)
      ixp2=ix+2
      iyp2=iy+2
      izp2=iz+2
      ixp=ix+1
      iyp=iy+1
      izp=iz+1
      ixm=ix-1
      iym=iy-1
      izm=iz-1
      ixm2=ix-2
      iym2=iy-2
      izm2=iz-2
      tjnd=tjend
      if(Imj.EQ.0)tjnd=i
      do 150 j=tjstrt,tjnd
      jx=Indjx(j)
      jy=Indjy(j)
      jz=Indjz(j)
      jxp2=jx+2
      jyp2=jy+2
      jzp2=jz+2
      jxp=jx+1
      jyp=jy+1
      jzp=jz+1
      jxm=jx-1
      jym=jy-1
      jzm=jz-1
      jxm2=jx-2
      jym2=jy-2
      jzm2=jz-2
      intc=intc+1
      iprim=0
      do 120 igauss=Igbegn,Igend
      twoa=two*Exx(igauss)
      fouraa=twoa*twoa
      do 110 jgauss=Jgbegn,Jgend
      twob=two*Exx(jgauss)
      fourbb=twob*twob
      fourab=twoa*twob
      iprim=iprim+1
      
      ind=ifind(ixp,iy,iz,jx,jy,jz,Jrange)
      tdva(1)=SP(iprim,ind)*twoa
      ind=ifind(ix,iyp,iz,jx,jy,jz,Jrange)
      tdva(2)=SP(iprim,ind)*twoa
      ind=ifind(ix,iy,izp,jx,jy,jz,Jrange)
      tdva(3)=SP(iprim,ind)*twoa
      if(ix.NE.1)then
      ind=ifind(ixm,iy,iz,jx,jy,jz,Jrange)
      tdva(1)=tdva(1)-Xint(ixm)*SP(iprim,ind)
      endif
      if(iy.NE.1)then
      ind=ifind(ix,iym,iz,jx,jy,jz,Jrange)
      tdva(2)=tdva(2)-Xint(iym)*SP(iprim,ind)
      endif
      if(iz.NE.1)then
      ind=ifind(ix,iy,izm,jx,jy,jz,Jrange)
      tdva(3)=tdva(3)-Xint(izm)*SP(iprim,ind)
      endif
      
      ind=ifind(ix,iy,iz,jxp,jy,jz,Jrange)
      tdvb(1)=SP(iprim,ind)*twob
      ind=ifind(ix,iy,iz,jx,jyp,jz,Jrange)
      tdvb(2)=SP(iprim,ind)*twob
      ind=ifind(ix,iy,iz,jx,jy,jzp,Jrange)
      tdvb(3)=SP(iprim,ind)*twob
      if(jx.NE.1)then
      ind=ifind(ix,iy,iz,jxm,jy,jz,Jrange)
      tdvb(1)=tdvb(1)-Xint(jxm)*SP(iprim,ind)
      endif
      if(jy.NE.1)then
      ind=ifind(ix,iy,iz,jx,jym,jz,Jrange)
      tdvb(2)=tdvb(2)-Xint(jym)*SP(iprim,ind)
      endif
      if(jz.NE.1)then
      ind=ifind(ix,iy,iz,jx,jy,jzm,Jrange)
      tdvb(3)=tdvb(3)-Xint(jzm)*SP(iprim,ind)
      endif
      
      
      do 105 k=1,3
      Dva(k)=Dva(k)+tdva(k)*XNORM(iprim,intc)
      Dvb(k)=Dvb(k)+tdvb(k)*XNORM(iprim,intc)
105   continue
110   continue
120   continue
150   continue
200   continue
      Lamax=tlamax
      Lbmax=tlbmax
      Lpmax=tlpmax
      Istart=tistrt
      Iend=tiend
      Jstart=tjstrt
      Jend=tjend
      Irange=tirnge
      Jrange=tjrnge
      Lentq=Irange*Jrange
      
      return
      end
C* :1 * 
      
