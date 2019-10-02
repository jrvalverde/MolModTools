
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tests"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tests.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "tests.web"
      subroutine tests
      implicit none
      double precision Alpha,Delta1,Delta2,Epsiln,Fconv,Flowb,Gkm1p,Gnor
     &m,Pnorm,Stpmin
      integer Idnts,Ierr,Igo,Iguess,Istype,Ndum
      common/ctests/Alpha,Delta1,Delta2,Epsiln,Stpmin,Fconv,Flowb,Gkm1p,
     &Pnorm,Gnorm,Idnts,Iguess,Ierr,Igo,Istype,Ndum
      
      
      
      
      
      Delta1=1.0D-08
      Delta2=1.0D-02
      Epsiln=1.0D-08
      Istype=1
      Fconv=5.0D-07
      Idnts=1
      Iguess=0
      Igo=1
      Stpmin=1.0D-03
      
      return
      
      end
C* :1 * 
      
