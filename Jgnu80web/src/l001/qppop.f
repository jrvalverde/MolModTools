
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qppop"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qppop.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "qppop.web"
      subroutine qppop
      implicit none
      integer Chrctr,Digit,Inte,Lastyp,Lcursr,Lenstr,Maxdep,Maxkey,Qpabr
     &v,Qpambg,Qpblnk,Qpcaps,Qpdpth,Qpend,Qperr,Qpexit,Qpfail,Qpnoab,Qpo
     &k,Qprecr
      integer Qpret,Stack,State,Status,String,Tcursr,Tran
      real Fp
      double precision Dp
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxkey
      
      
      
      Lastyp=Stack(1,Qpdpth)
      Status=Stack(2,Qpdpth)
      Tcursr=Stack(3,Qpdpth)
      Lcursr=Stack(4,Qpdpth)
      State=Stack(5,Qpdpth)
      Tran=Stack(6,Qpdpth)
      
      Tcursr=Tcursr+1
      Qpdpth=Qpdpth-1
      return
      
      end
C* :1 * 
      
