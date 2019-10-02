
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cntrlp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cntrlp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 31 "cntrlp.web"
      subroutine cntrlp(XPS,SP,XNORM,QQ,MAXI2,MAXP2)
      implicit none
      real*8 Ca,Ca2,Cax,Cay,Caz,Cb,Cb2,Cbx,Cby,Cbz,Cca,Ccb,Cdummy,Clp,Fp
     &i,Pi,Pi3haf,Pi5hf2,Piqurt,QQ
      real*8 SP,Sqpi,Sqpi2,Twopi,Xa,Xb,Xc,XNORM,XPS,Ya,Yb,Yc,Za,Zb,Zc,Zl
     &p
      integer Iatom,idps2,Iend,igauss,Igbegn,Igdf,Igend,ii,iii,Imj,intc,
     &intps,iprim,Irange,Istart,Itype,Jend,jgauss,Jgbegn,Jgdf
      integer Jgend,Jnktyp,Jrange,Jstart,Jtype,Kfirst,Klast,Lentq,Limitd
     &,Lmax,Lpskip,MAXATM,MAXI2,MAXP2,Nfroz,Nlp,nprim,Ntpse
      parameter(MAXATM=100)
      
      
      common/pseud/Ntpse(7,MAXATM)
      common/lp2/Nlp(400),Clp(400),Zlp(400),Kfirst(35,5),Klast(35,5),Lma
     &x(35),Lpskip(35),Nfroz(35)
      common/centre/Xa,Ya,Za,Xb,Yb,Zb,Xc,Yc,Zc,Iatom
      common/dist/Cax,Cay,Caz,Ca,Ca2,Cbx,Cby,Cbz,Cb,Cb2
      common/pifac/Pi,Twopi,Fpi,Pi3haf,Pi5hf2,Piqurt,Sqpi,Sqpi2
      common/contr/Cca(20),Ccb(20),Cdummy(40)
      common/prims/Igbegn,Igend,Jgbegn,Jgend,Igdf,Jgdf
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Limit
     &d(11)
      common/type/Itype,Jtype,Jnktyp(10)
      dimension XPS(*),SP(MAXP2,MAXI2),XNORM(MAXP2,MAXI2),QQ(*)
      
      Cax=Xc-Xa
      Cay=Yc-Ya
      Caz=Zc-Za
      Ca2=Cax*Cax+Cay*Cay+Caz*Caz
      Cbx=Xc-Xb
      Cby=Yc-Yb
      Cbz=Zc-Zb
      Cb2=Cbx*Cbx+Cby*Cby+Cbz*Cbz
      Ca=sqrt(Ca2)
      Cb=sqrt(Cb2)
      
      call aclear(MAXI2*MAXP2,SP)
      iprim=0
      do 100 igauss=Igbegn,Igend
      call fillc(Itype,Igbegn,igauss,Igdf,Cca)
      do 50 jgauss=Jgbegn,Jgend
      call fillc(Jtype,Jgbegn,jgauss,Jgdf,Ccb)
      iprim=iprim+1
      intc=0
      do 20 ii=Istart,Iend
      do 10 iii=Jstart,Jend
      intc=intc+1
      XNORM(iprim,intc)=Fpi*Cca(ii)*Ccb(iii)
10    continue
20    continue
50    continue
100   continue
      nprim=(Igend-Igbegn+1)*(Jgend-Jgbegn+1)
      idps2=Kfirst(Iatom,1)
      intps=Klast(Iatom,1)-Kfirst(Iatom,1)+1
      call pseud1(intps,Nlp(idps2),Zlp(idps2),Clp(idps2),SP,XNORM,MAXI2,
     &MAXP2)
      call pseud2(Lmax(Iatom),Ntpse(1,Iatom),Nlp,Zlp,Clp,SP,XNORM,QQ,MAX
     &I2,MAXP2)
      
      intc=0
      do 200 ii=Istart,Iend
      do 150 iii=Jstart,Jend
      intc=intc+1
      do 120 iprim=1,nprim
      XPS(intc)=XPS(intc)+SP(iprim,intc)*XNORM(iprim,intc)
120   continue
150   continue
200   continue
      return
      end
C* :1 * 
      
