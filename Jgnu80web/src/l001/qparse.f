
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qparse"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qparse.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 39 "qparse.web"
      integer function qparse(RESULT,TABLE,LINE,LENGTH)
      implicit none
      integer Chrctr,Digit,Inte,Lastyp,Lcursr,LENGTH,Lenstr,LINE,Maxdep,
     &Maxlen,Qpabrv,Qpambg,Qpblnk,Qpcaps,qpdest,Qpdpth,Qpend,Qperr,Qpexi
     &t,Qpfail
      integer Qpnoab,Qpok,qppush,Qprecr,Qpret,qpscan,qptran,RESULT,Stack
     &,State,Status,String,TABLE,Tcursr,Tran
      dimension RESULT(*),TABLE(*),LINE(*)
      real Fp
      double precision Dp
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxlen
      
100   if(qpscan(TABLE,LINE,LENGTH).EQ.Qpok)then
      
150   if(qptran(RESULT,TABLE).EQ.Qpok)goto 100
      if(Status.EQ.Qpexit)then
      
      if(Qpdpth.GT.0)then
      call qppop
      goto 150
      endif
      endif
      
      elseif(Status.NE.Qprecr)then
      
      if(Qpdpth.GT.0)then
      
      call qppop
      call qpskip(Tcursr)
      goto 100
      endif
      elseif(qppush(0).EQ.Qpok)then
      Status=qpdest(TABLE,Tcursr,State)
      goto 100
      endif
      
      if(Status.EQ.Qpexit)Status=Qpok
      qparse=Status
      return
      
      end
C* :1 * 
      
