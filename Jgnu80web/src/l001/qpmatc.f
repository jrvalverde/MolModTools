
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpmatc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpmatc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "qpmatc.web"
      integer function qpmatc(TABLE,LINE,LENGTH)
      implicit none
      integer branch,Chrctr,Digit,Inte,Lastyp,Lcursr,LENGTH,Lenstr,LINE,
     &Maxdep,Maxlen,Qpabrv,qpaint,Qpambg,qpany1,Qpblnk,Qpcaps,qpdcod,qpd
     &p,Qpdpth
      integer Qpend,Qperr,Qpexit,Qpfail,Qpnoab,Qpok,qpreal,Qprecr,Qpret,
     &qpschr,qpskey,qptval,qpword,Stack,State,Status,String,TABLE,Tcursr
     &,Tran
      integer val
      dimension TABLE(*),LINE(*)
      real Fp
      double precision Dp
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxlen
      
      
      
      Lastyp=qptval(TABLE,Tcursr)
      branch=qpdcod(Lastyp)
      if(branch.LE.0.OR.branch.GE.24)then
      val=Qperr
      
      elseif(branch.EQ.2)then
      
      val=qpschr(TABLE,LINE,LENGTH)
      elseif(branch.EQ.3)then
      
      val=qpany1(10,LINE,LENGTH,Lcursr,Digit)
      elseif(branch.EQ.4)then
      
      val=qpany1(-1,LINE,LENGTH,Lcursr,Chrctr)
      elseif(branch.EQ.5)then
      
      val=qpany1(-2,LINE,LENGTH,Lcursr,Chrctr)
      elseif(branch.EQ.6)then
      
      val=qpany1(-3,LINE,LENGTH,Lcursr,Chrctr)
      elseif(branch.EQ.7)then
      
      val=qpany1(-4,LINE,LENGTH,Lcursr,Chrctr)
      elseif(branch.EQ.8)then
      
      val=qpaint(10,LINE,LENGTH,Lcursr,Inte,0)
      elseif(branch.EQ.9)then
      
      val=qpreal(LINE,LENGTH,Lcursr,Fp)
      elseif(branch.EQ.10)then
      
      val=qpdp(LINE,LENGTH,Lcursr,Dp)
      elseif(branch.EQ.11)then
      
      val=Qpok
      elseif(branch.EQ.12)then
      
      val=Qpfail
      if(Lcursr.GE.LENGTH)val=Qpok
      elseif(branch.EQ.13)then
      
      val=qpword(0,0,0,LINE,LENGTH,Lcursr,String,Lenstr)
      elseif(branch.EQ.14)then
      
      val=qpword(1,TABLE,Tcursr,LINE,LENGTH,Lcursr,String,Lenstr)
      elseif(branch.EQ.15)then
      
      val=Qpend
      elseif(branch.EQ.16)then
      
      val=Qpend
      elseif(branch.EQ.17)then
      
      val=qpany1(2,LINE,LENGTH,Lcursr,Digit)
      elseif(branch.EQ.18)then
      
      val=qpany1(8,LINE,LENGTH,Lcursr,Digit)
      elseif(branch.EQ.19)then
      
      val=qpany1(16,LINE,LENGTH,Lcursr,Digit)
      elseif(branch.EQ.20)then
      
      val=qpaint(2,LINE,LENGTH,Lcursr,Inte,0)
      elseif(branch.EQ.21)then
      
      val=qpaint(8,LINE,LENGTH,Lcursr,Inte,0)
      elseif(branch.EQ.22)then
      
      val=qpaint(16,LINE,LENGTH,Lcursr,Inte,0)
      elseif(branch.EQ.23)then
      
      val=Qprecr
      else
      
      val=qpskey(TABLE,LINE,LENGTH)
      endif
      
      qpmatc=val
      return
      
      end
C* :1 * 
      
