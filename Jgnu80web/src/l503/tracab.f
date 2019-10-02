
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tracab"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tracab.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "tracab.web"
      double precision function tracab(NBASIS,M1,M2)
      implicit none
      double precision A,B,Big,Fillab,Four,gfloat,One,Onept5,Pt5,Small,T
     &hree,Two,Zero
      integer idump,Ksm,Kspin,Ksw,M1,M2,NBASIS,Nesk,Nest,Nest1,Nse,Nsep
      logical Cmp,Rhf
      dimension M1(*),M2(*)
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/memry/A(4970),B(4970),Fillab(40060)
      common/con503/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      
      tracab=Zero
      idump=0
      do 100 Kspin=1,Ksm
      call tioc(NBASIS,2,M1,A,2,1,idump)
      call tioc(NBASIS,2,M2,B,2,1,idump)
      call matmul(NBASIS,A,B,2,0,2)
      tracab=tracab+A(1)
100   continue
      
      tracab=tracab/gfloat(Ksm)
      return
      
      end
C* :1 * 
      
