
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qppush"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qppush.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "qppush.web"
      integer function qppush(I)
      implicit none
      integer Chrctr,Digit,I,Inte,Lastyp,Lcursr,Lenstr,Maxdep,Maxkey,Qpa
     &brv,Qpambg,Qpblnk,Qpcaps,Qpdpth,Qpend,Qperr,Qpexit,Qpfail,Qpnoab,Q
     &pok
      integer Qprecr,Qpret,Stack,State,Status,String,Tcursr,Tran
      real Fp
      double precision Dp
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxkey
      
      
      
      Qpdpth=Qpdpth+1
      if(Qpdpth.LE.Maxdep)then
      
      Stack(1,Qpdpth)=Lastyp
      Stack(2,Qpdpth)=Status
      Stack(3,Qpdpth)=Tcursr
      Stack(4,Qpdpth)=Lcursr
      Stack(5,Qpdpth)=State
      Stack(6,Qpdpth)=Tran
      qppush=Qpok
      return
      endif
      
      qppush=Qperr
      return
      
      end
C* :1 * 
      
