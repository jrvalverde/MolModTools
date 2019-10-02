
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 phfprt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "phfprt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "phfprt.web"
      subroutine phfprt(NBASIS)
      implicit none
      double precision D,Dd,F,Ff,V,Val,Vv
      integer i,Icnvg,Icyc,Idum,Idump,Iext,Ifcnvg,Iguess,Ij,In,Iout,Ipch
     &,Iprint,Ipunch,Irstrt,Irwc1,Irwc2,Irwc3,Irwc4,Irwca
      integer Irwcb,Irwev,Irwfa,Irwfb,Irwgen,Irwh,Irwpa,Irwpb,Irwpt,Irws
     &,Irwt,j,k,Maxnbf,Maxntt,NBASIS,Ntt
      common/memry/D(70,70),Dd(70),F(70,70),Ff(70),V(70,70),Vv(70),Idum
      common/irw505/Irwgen,Irws,Irwt,Irwh,Irwev,Irwca,Irwcb,Irwpa,Irwpb,
     &Irwpt,Irwfa,Irwfb,Irwc1,Irwc2,Irwc3,Irwc4
      common/max505/Maxnbf,Maxntt
      common/ops505/Ipch,Iprint,Idump,Iguess,Icnvg,Icyc,Irstrt,Iext,Ifcn
     &vg
      common/eigval/Val(210)
      common/io/In,Iout,Ipunch
      common/jnkphf/Ntt,Ij(71)
      
      
      
      
99001 format(17H     EIGENVALUES:/23x,6HF(D/E),32x,6HF(S/E),32x,6HF(D/S)
     &)
99002 format(10x,i2,1H.,4x,d20.12,2(18x,d20.12))
99003 format(1H ,23H  ALPHA DENSITY MATRIX.)
99004 format(1H ,23H  BETA  DENSITY MATRIX.)
99005 format(1H ,33H  MOLECULAR ORBITAL COEFFICIENTS.)
      
      write(Iout,99001)
      do 100 i=1,NBASIS
      j=i+70
      k=i+140
      write(Iout,99002)i,Val(i),Val(j),Val(k)
100   continue
      
      write(Iout,99005)
      call tread(Irwca,D,Maxnbf,Maxnbf,NBASIS,NBASIS,0)
      call matout(D,Maxnbf,Maxnbf,NBASIS,NBASIS)
      
      if(Iprint.GE.2)then
      write(Iout,99003)
      call tread(Irwpa,D,Maxntt,1,Ntt,1,0)
      call ltoutd(NBASIS,D,1)
      write(Iout,99004)
      call tread(Irwpb,D,Maxntt,1,Ntt,1,0)
      call ltoutd(NBASIS,D,1)
      endif
      
      return
      
      end
C* :1 * 
      
