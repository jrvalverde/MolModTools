
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpscan"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpscan.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "qpscan.web"
      integer function qpscan(TABLE,LINE,LENGTH)
      implicit none
      integer Chrctr,Digit,Inte,Lastyp,Lcursr,LENGTH,Lenstr,LINE,Maxdep,
     &Maxlen,Qpabrv,Qpambg,Qpblnk,Qpcaps,Qpdpth,Qpend,Qperr,Qpexit,Qpfai
     &l,qpmatc
      integer Qpnoab,Qpok,Qprecr,Qpret,Stack,State,Status,String,TABLE,T
     &cursr,Tran
      dimension LINE(*),TABLE(*)
      real Fp
      double precision Dp
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxlen
      
      
      if(Qpblnk.EQ.0)call qpskbl(LINE,Lcursr,LENGTH)
100   Tran=Tran+1
      Status=qpmatc(TABLE,LINE,LENGTH)
      if(Status.NE.Qpok)call qpskip(Tcursr)
      if(Status.EQ.Qpfail)goto 100
      if(Status.EQ.Qpend)Status=Qpfail
      qpscan=Status
      return
      
      end
C* :1 * 
      
