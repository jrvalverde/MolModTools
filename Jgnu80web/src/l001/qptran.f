
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qptran"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qptran.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "qptran.web"
      integer function qptran(RESULT,TABLE)
      implicit none
      integer Chrctr,Digit,index,Inte,Lastyp,Lcursr,Lenstr,Maxdep,Maxlen
     &,mycrsr,Qpabrv,Qpambg,Qpblnk,Qpcaps,qpdest,Qpdpth,Qpend,Qperr,Qpex
     &it,Qpfail
      integer Qpnoab,Qpok,Qprecr,Qpret,qptval,RESULT,Stack,State,Status,
     &String,TABLE,Tcursr,Tran,val,value
      real Fp
      double precision Dp
      dimension RESULT(*),TABLE(*)
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxlen
      
      
      
      mycrsr=Tcursr
      val=qpdest(TABLE,Tcursr,State)
      if(val.NE.Qperr)then
      index=qptval(TABLE,mycrsr)
      index=qptval(TABLE,mycrsr)
      value=qptval(TABLE,mycrsr)
      if(index.NE.0)then
      if(value.EQ.0)then
      call qputit(RESULT(index))
      else
      RESULT(index)=RESULT(index)+value
      endif
      endif
      endif
      Status=val
      qptran=val
      return
      
      end
C* :1 * 
      
