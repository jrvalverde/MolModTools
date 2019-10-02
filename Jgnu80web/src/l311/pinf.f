
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 pinf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "pinf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "pinf.web"
      subroutine pinf(SYMFAC)
      implicit none
      double precision Ag,App,Ax,Ay,Az,Bg,Bpp,Bx,By,Bz,Cg,Cmax,Cmaxa,Cma
     &xb,Cmaxc,Cmaxd,Conp,Const,Cpa,cpai
      double precision Cpb,Cpc,Cpd,Csa,csai,Csb,Csc,Csd,Cx,Cy,Cz,Dg,Dp00
     &p,Dp01p,Dp10p,Dp11p,Dx,Dy,Dz,eab
      double precision Ep,Error1,Error2,Ga,Gab,Gb,gbeab,Gc,Gcd,Gd,Gp,one
     &,P11,P12,P13,P21,P22,P23,P31,P32
      double precision P33,Pa,Pb,Pc,Pd,Pidiv4,Pito52,Q11,Q12,Q13,Q21,Q22
     &,Q23,Q31,Q32,Q33,Rab,Rabsq,Rcd,Rcdsq
      double precision Sa,Sb,Sc,Sd,sixty,SYMFAC,x,xqq,xx,xxtest,zero
      integer i,ind,Isml,Ismlp,Ismlq,Itype,j,Jnktyp,Jtype,La,Lb,Lc,Ld,Ma
     &b,Mcd,Nga,Ngangb,Ngb,Ngc,Ngd
      common/cconst/Const,Conp(100)
      common/shlinf/Nga,La,Ag(10),Csa(10),Cpa(10),Ngb,Lb,Bg(10),Csb(10),
     &Cpb(10),Ngc,Lc,Cg(10),Csc(10),Cpc(10),Ngd,Ld,Dg(10),Csd(10),Cpd(10
     &)
      common/misc/Mab,Mcd,Ngangb
      common/cgeom/Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,Dx,Dy,Dz,Rab,Rabsq,Rcd,Rcd
     &sq,P11,P12,P13,P21,P22,P23,P31,P32,P33,Q11,Q12,Q13,Q21,Q22,Q23,Q31
     &,Q32,Q33
      common/pgeom/Gp(100),Ep(100),Dp00p(100),Dp01p(100),Dp10p(100),Dp11
     &p(100),App(100),Bpp(100)
      common/ginf/Ga,Gb,Gc,Gd,Sa,Sb,Sc,Sd,Pa,Pb,Pc,Pd,Gab,Gcd
      common/type/Itype,Jtype,Jnktyp(10)
      common/maxc/Cmax(240),Cmaxa(10),Cmaxb(10),Cmaxc(10),Cmaxd(10),Isml
     &p(100),Ismlq,Isml,Error1,Error2
      common/picon/Pito52,Pidiv4
      data zero/0.0D0/,one/1.0D0/,sixty/60.0D0/
      
      
      
      
      
      
      ind=0
      do 100 i=1,Nga
      Ga=Ag(i)
      csai=Csa(i)
      cpai=Cpa(i)
      do 50 j=1,Ngb
      ind=ind+1
      Gb=Bg(j)
      Gab=Ga+Gb
      Gp(ind)=Gab
      eab=one/Gab
      Ep(ind)=eab
      gbeab=Gb*eab
      App(ind)=gbeab*Rab
      Bpp(ind)=App(ind)-Rab
      xqq=Ga*gbeab*Rabsq
      if(xqq.LE.sixty)then
      
      xx=dexp(-xqq)*eab
      xxtest=Cmaxa(i)*Cmaxb(j)*xx
      if(xxtest.GT.Error1)then
      Ismlp(ind)=0
      
      elseif(xxtest.LE.Error2)then
      
      Ismlp(ind)=2
      else
      Ismlp(ind)=1
      endif
      x=Pito52*xx/SYMFAC
      Dp00p(ind)=x*csai*Csb(j)
      if(Jtype.GT.3)then
      Dp01p(ind)=x*csai*Cpb(j)
      if(Jtype.LE.5)then
      Conp(ind)=Dp01p(ind)*eab
      Dp00p(ind)=Dp00p(ind)*Gab/Dp01p(ind)
      Bpp(ind)=Bpp(ind)*Gab
      else
      
      Dp10p(ind)=x*cpai*Csb(j)
      Dp11p(ind)=x*cpai*Cpb(j)
      Conp(ind)=Dp11p(ind)
      Dp00p(ind)=Dp00p(ind)/Dp11p(ind)
      Dp01p(ind)=Dp01p(ind)/Dp11p(ind)
      Dp10p(ind)=Dp10p(ind)/Dp11p(ind)
      endif
      endif
      else
      Ismlp(ind)=2
      Dp00p(ind)=zero
      if(Jtype.GT.3)then
      Dp01p(ind)=zero
      Conp(ind)=zero
      if(Jtype.LE.5)then
      Bpp(ind)=Bpp(ind)*Gab
      else
      
      Dp10p(ind)=zero
      Dp11p(ind)=zero
      endif
      endif
      endif
50    continue
100   continue
      return
      
      end
C* :1 * 
      
