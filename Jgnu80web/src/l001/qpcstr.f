
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpcstr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpcstr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "qpcstr.web"
      integer function qpcstr(REFRNC,LENR,FOUND,LENF)
      implicit none
      integer Chrctr,Digit,FOUND,Inte,Lastyp,Lcursr,LENF,LENR,Lenstr,Max
     &dep,Maxlen,Qpabrv,Qpambg,Qpblnk,Qpcaps,Qpdpth,Qpend,Qperr,Qpexit,Q
     &pfail
      integer Qpnoab,Qpok,Qprecr,Qpret,REFRNC,Stack,State,Status,String,
     &Tcursr,Tran,val
      logical streq,streqc,ifeq
      dimension REFRNC(*),FOUND(*)
      real Fp
      double precision Dp
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxlen
      
      
      val=Qpfail
      if(LENF.LE.LENR)then
      if(Qpcaps.EQ.0)ifeq=streqc(REFRNC,FOUND,LENF)
      if(Qpcaps.NE.0)ifeq=streq(REFRNC,FOUND,LENF)
      if(ifeq)then
      val=Qpabrv
      if(LENF.EQ.LENR)val=Qpok
      endif
      endif
      qpcstr=val
      return
      
      end
C* :1 * 
      
