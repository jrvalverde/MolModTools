
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ovlp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ovlp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 34 "ovlp.web"
      subroutine ovlp(S,NBASA,NBASB,ISDIM,IPRINT)
      implicit none
      double precision abx,aby,abz,arabsq,as,asax,asay,asaz,ax,ay,az,bs,
     &bx,by,bz,ca,cb,ccx,ccy,ccz
      double precision Cda,Cdb,Cdfa,Cdfb,Cfa,Cfb,Cpa,Cpb,Csa,Csb,Dumint,
     &Eight,ep,epi,epio2,Exxa,Exxb,Five,Four,gatan
      double precision gexp,gsqrt,half,One,pexp,pi,pi3haf,Pt5,px,py,pz,R
     &1,R2,R3,R3ov2,R4,rabsq,Root15,Root3,Root5
      double precision rootpi,S,s1c,Seven,Six,ss,sterm,sx,sy,sz,Ten,Thre
     &e,Two,Xa,xap,Xb,xbp,Ya,yap,Yb
      double precision ybp,Z1,Z2,Z3,Za,zap,Zb,zbp,zero,Zero1
      integer i,Iend,igauss,igbegn,igdf,igend,Ildum,Imj,In,indjx,indjy,i
     &ndjz,indsx,indsy,indsz,Inew,intc,Iout,IPRINT,Ipunch
      integer Ipurd,Ipurf,Irange,ISDIM,ishell,Istart,Itype,ix,iy,iz,j,Ja
     &na,Janb,Jdump,Jend,jgauss,jgbegn,jgdf,jgend,Jjdump
      integer Jnew,Jnktyp,Jrange,jshell,Jstart,Jtype,jx,jy,jz,Lamax,Lbma
     &x,Lbound,Lentq,lim1ds,Lpmax,Maxdum,MAXPRM,MAXS21,MAXSH1,MAXSHL
      integer Maxta,Maxtb,N10ord,N5ord,N6ord,N7ord,na,nb,NBASA,NBASB,Nor
     &dr,Nshela,Nshelb
      double precision Nine
      integer Shelaa,Shelna,Shelta,Shelca,Aosa,Aona,scona
      integer Shelab,Shelnb,Sheltb,Shelcb,Aosb,Aonb,sconb
      integer Shlafa,Shlafb
      integer Ubound,Ulpurd
      dimension ccx(64),ccy(64),ccz(64),ss(100),sx(16),sy(16),sz(16)
      dimension s1c(9),indsx(20),indsy(20),indsz(20),ca(20),cb(20)
      dimension indjx(20),indjy(20),indjz(20)
      dimension S(ISDIM,ISDIM)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      dimension Cda(MAXSHL),Cfa(MAXSHL),Shlafa(MAXSHL)
      dimension Cdb(MAXSHL),Cfb(MAXSHL),Shlafb(MAXSHL)
      common/b/Exxa(MAXPRM),Csa(MAXPRM),Cpa(MAXPRM),Cdfa(MAXPRM),Xa(MAXS
     &HL),Ya(MAXSHL),Za(MAXSHL),Jana(MAXSHL),Shelaa(MAXSHL),Shelna(MAXSH
     &L),Shelta(MAXSHL),Shelca(MAXSHL),Aosa(MAXSHL),Aona(MAXSHL),Nshela,
     &Maxta
      common/b2/Exxb(MAXPRM),Csb(MAXPRM),Cpb(MAXPRM),Cdfb(MAXPRM),Xb(MAX
     &SHL),Yb(MAXSHL),Zb(MAXSHL),Janb(MAXSHL),Shelab(MAXSHL),Shelnb(MAXS
     &HL),Sheltb(MAXSHL),Shelcb(MAXSHL),Aosb(MAXSHL),Aonb(MAXSHL),Nshelb
     &,Maxtb
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Ildum
     &(11)
      common/type/Itype,Jtype,Jnktyp(10)
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      common/io/In,Iout,Ipunch
      common/int/Zero1,One,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,
     &Dumint(2)
      common/new/Inew,Jnew
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpurd(4)
      common/ipure/Ipurd,Ipurf
      common/dump/Jdump,Jjdump
      equivalence(Cda(1),Cdfa(1))
      equivalence(Cfa(1),Cdfa(MAXSH1))
      equivalence(Shlafa(1),Cdfa(MAXS21))
      equivalence(Cdb(1),Cdfb(1))
      equivalence(Cfb(1),Cdfb(MAXSH1))
      equivalence(Shlafb(1),Cdfb(MAXS21))
      data indjx/1,2,1,1,3,1,1,2,2,1,4,1,1,2,3,3,2,1,1,2/
      data indjy/1,1,2,1,1,3,1,2,1,2,1,4,1,3,2,1,1,2,3,2/
      data indjz/1,1,1,2,1,1,3,1,2,2,1,1,4,1,1,2,3,3,2,2/
      data zero/0.0D0/,half/0.5D0/
      
      
      
      
      
      
      if(Jdump.NE.0)write(Iout,99001)
      
99001 format('  OVLP')
      
      pi=Four*gatan(One)
      rootpi=gsqrt(pi)
      pi3haf=pi*rootpi
      call ilsw(2,16,Ipurf)
      call ilsw(2,2,Ipurd)
      
      do 100 i=1,20
      indsx(i)=(indjx(i)-1)*4
      indsy(i)=(indjy(i)-1)*4
      indsz(i)=(indjz(i)-1)*4
100   continue
      call setord
      
      
      
      do 300 ishell=1,Nshela
      Inew=ishell
      ax=Xa(ishell)
      ay=Ya(ishell)
      az=Za(ishell)
      igbegn=Shelaa(ishell)
      na=Shelna(ishell)
      igend=igbegn+na-1
      Itype=Shelta(ishell)
      Lamax=Itype+1
      scona=Shelca(ishell)
      Iend=Ubound(Lamax)
      Istart=Lbound(Lamax,scona+1)
      Irange=Iend-Istart+1
      igdf=Shlafa(ishell)
      
      
      do 200 jshell=1,Nshelb
      Jnew=jshell
      bx=Xb(jshell)
      by=Yb(jshell)
      bz=Zb(jshell)
      nb=Shelnb(jshell)
      jgbegn=Shelab(jshell)
      jgend=jgbegn+nb-1
      Jtype=Sheltb(jshell)
      Lbmax=Jtype+1
      sconb=Shelcb(jshell)
      Jstart=Lbound(Lbmax,sconb+1)
      Jend=Ubound(Lbmax)
      Jrange=Jend-Jstart+1
      jgdf=Shlafb(jshell)
      
      Lpmax=Lamax+Lbmax-1
      lim1ds=(Lpmax+1)/2
      Lentq=Irange*Jrange
      Imj=iabs(ishell-jshell)
      abx=bx-ax
      aby=by-ay
      abz=bz-az
      rabsq=abx*abx+aby*aby+abz*abz
      do 120 i=1,Lentq
      ss(i)=zero
120   continue
      
      
      do 160 igauss=igbegn,igend
      as=Exxa(igauss)
      asax=as*ax
      asay=as*ay
      asaz=as*az
      arabsq=as*rabsq
      call cfill(Itype,igbegn,igauss,igdf,Csa,Cpa,Cda,Cfa,ca)
      
      do 140 jgauss=jgbegn,jgend
      bs=Exxb(jgauss)
      call cfill(Jtype,jgbegn,jgauss,jgdf,Csb,Cpb,Cdb,Cfb,cb)
      
      ep=as+bs
      epi=One/ep
      epio2=epi*half
      pexp=gexp(-bs*arabsq*epi)
      
      px=(asax+bs*bx)*epi
      py=(asay+bs*by)*epi
      pz=(asaz+bs*bz)*epi
      
      xap=px-ax
      yap=py-ay
      zap=pz-az
      xbp=px-bx
      ybp=py-by
      zbp=pz-bz
      
      call getcc1(ccx,xap,xbp,0)
      call getcc1(ccy,yap,ybp,0)
      call getcc1(ccz,zap,zbp,0)
      
      sterm=rootpi*gsqrt(epi)
      call get1cs(s1c,sterm,epio2,0)
      
      call get2cs(sx,s1c,ccx,0)
      call get2cs(sy,s1c,ccy,0)
      do 125 i=1,lim1ds
      s1c(i)=s1c(i)*pexp
125   continue
      call get2cs(sz,s1c,ccz,0)
      
      
      intc=0
      do 130 i=Istart,Iend
      ix=indsx(i)
      iy=indsy(i)
      iz=indsz(i)
      do 126 j=Jstart,Jend
      jx=indjx(j)
      jy=indjy(j)
      jz=indjz(j)
      intc=intc+1
      ss(intc)=ss(intc)+sx(ix+jx)*sy(iy+jy)*sz(iz+jz)*ca(i)*cb(j)
126   continue
130   continue
      
140   continue
160   continue
      
      call filrec(ss,S,Aosa(ishell),Aosb(jshell),ISDIM)
      
200   continue
300   continue
      
      return
      
      end
C* :1 * 
      
