
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 phfchk"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "phfchk.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "phfchk.web"
      subroutine phfchk(NBASIS)
      implicit none
      double precision A,Aa,B,Bb,C,Cc,Filabc,gabs,gsqrt,one,temp,thresh
      integer i,Icnvg,Icyc,Id505,Idump,Iext,Ifcnvg,Ifill,iflag,Iguess,im
     &1,In,Iout,Ipch,Iprint,Ipunch,Irstrt,Irwc1,Irwc2,Irwc3
      integer Irwc4,Irwca,Irwcb,Irwev,Irwfa,Irwfb,Irwgen,Irwh,Irwpa,Irwp
     &b,Irwpt,Irws,Irwt,j,Maxnbf,Maxntt,NBASIS
      common/memry/A(70,70),Aa(70),B(70,70),Bb(70),C(70,70),Cc(70),Id505
     &,Ifill,Filabc(35089)
      common/io/In,Iout,Ipunch
      common/irw505/Irwgen,Irws,Irwt,Irwh,Irwev,Irwca,Irwcb,Irwpa,Irwpb,
     &Irwpt,Irwfa,Irwfb,Irwc1,Irwc2,Irwc3,Irwc4
      common/max505/Maxnbf,Maxntt
      common/ops505/Ipch,Iprint,Idump,Iguess,Icnvg,Icyc,Irstrt,Iext,Ifcn
     &vg
      data thresh/1.0D-07/,one/1.0D0/
      
      
      
      
99001 format(' BAD DIAGONAL ELEMENT: ',i3,d20.10)
99002 format(' BAD OFF-DIAGONAL ELEMENT: ',2I3,d20.10)
99003 format(' C(DAGGER)*S*C FROM PHF')
99004 format(' ORTHONORMALIZATION REQUIRED ... PHFCHK.')
99005 format(' OFFENDING EIGENVALUE ... CANNOT ORTHONORMALIZE:',i3,d20.1
     &3)
99006 format(' FROM PHFCHK, RE-ORTHONORMALIZED COEFFICIENT ARRAY:')
      
      call tread(Irws,A,Maxnbf,Maxnbf,NBASIS,NBASIS,1)
      call tread(Irwca,B,Maxnbf,Maxnbf,NBASIS,NBASIS,0)
      
      call matpac(A,B,C,Maxnbf,NBASIS,1)
      
      call matpac(B,C,A,Maxnbf,NBASIS,2)
      
      iflag=0
      do 100 i=1,NBASIS
      if(gabs(A(i,i)-one).GE.thresh)then
      write(Iout,99001)i,A(i,i)
      iflag=1
      endif
100   continue
      do 200 i=1,NBASIS
      im1=i-1
      if(im1.GT.0)then
      do 120 j=1,im1
      if(gabs(A(i,j)).GE.thresh)then
      iflag=1
      write(Iout,99002)i,j,A(i,j)
      endif
120   continue
      endif
200   continue
      if(Iprint.LT.3)then
      if(iflag.LE.0)goto 300
      endif
      write(Iout,99003)
      call matout(A,Maxnbf,Maxnbf,NBASIS,NBASIS)
      
300   if(iflag.GT.0)then
      write(Iout,99004)
      
      call eigen(NBASIS,A,B,Bb,Aa)
      do 350 j=1,NBASIS
      if(Bb(j).LE.0)then
      write(Iout,99005)j,Bb(j)
      call lnk1e
      endif
      temp=one/gsqrt(Bb(j))
      do 320 i=1,NBASIS
      B(i,j)=B(i,j)*temp
320   continue
350   continue
      call tread(Irwca,A,Maxnbf,Maxnbf,NBASIS,NBASIS,0)
      call matpac(A,B,C,Maxnbf,NBASIS,1)
      call twrite(Irwca,C,Maxnbf,Maxnbf,NBASIS,NBASIS,0)
      if(Iprint.GT.2)then
      write(Iout,99006)
      call matout(C,Maxnbf,Maxnbf,NBASIS,NBASIS)
      endif
      endif
      
      return
      
      end
C* :1 * 
      
