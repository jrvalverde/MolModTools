
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpinit"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpinit.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "qpinit.web"
      subroutine qpinit(TABLE,BLNKS,CAPS,ABRVS)
      implicit none
      integer ABRVS,blank,BLNKS,CAPS,Chrctr,Digit,i,Inte,Lastyp,Lcursr,L
     &enstr,Maxdep,Maxlen,Qpabrv,Qpambg,Qpblnk,Qpcaps,Qpdpth,Qpend,Qperr
      integer Qpexit,Qpfail,Qpnoab,Qpok,Qprecr,Qpret,qptval,Stack,State,
     &Status,String,TABLE,Tcursr,Tran
      dimension TABLE(*)
      real Fp
      double precision Dp
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxlen
      data blank/4H    /
      
      
      
      
      Qpblnk=BLNKS
      Qpcaps=CAPS
      Qpnoab=ABRVS
      
      Lastyp=0
      Inte=0
      Fp=0.0
      Dp=0.0D0
      Status=0
      Qpdpth=0
      Chrctr=blank
      Digit=blank
      do 100 i=1,64
      String(i)=blank
100   continue
      Tcursr=0
      State=qptval(TABLE,Tcursr)
      Lcursr=0
      Tran=0
      Lenstr=0
      return
      
      end
C* :1 * 
      
