
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpreal"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpreal.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "qpreal.web"
      integer function qpreal(LINE,LENGTH,LCURSR,R)
      implicit none
      integer LCURSR,LENGTH,Maxdep,Maxkey,Qpabrv,Qpambg,qpdp,Qpdpth,Qpen
     &d,Qperr,Qpexit,Qpfail,Qpok,Qprecr,Qpret,Stack,val
      integer LINE(*)
      real R
      double precision x
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxkey
      
      val=qpdp(LINE,LENGTH,LCURSR,x)
      if(val.EQ.Qpok)R=x
      qpreal=val
      return
      
      end
C* :1 * 
      
