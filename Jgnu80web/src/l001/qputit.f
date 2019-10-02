
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qputit"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qputit.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "qputit.web"
      subroutine qputit(WHERE)
      implicit none
      real gsqrt,Qpblnk,Qpcaps,Qpnoab,x,y
      integer i,jjjj
      integer branch,cur1,cur2,WHERE(2),chr,Dpl(2),qpdcod,Fpl,getchr
      integer blank
      integer Lastyp,Status,Chrctr,Digit,Inte,String,Tcursr,Lcursr,State
     &,Tran,Lenstr
      real Fp
      double precision Dp
      integer qpcpv
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      equivalence(Dpl(1),Dp),(Fpl,Fp)
      data blank/'    '/
      
      
      
      
      branch=qpdcod(Lastyp)
      if(branch.EQ.3.OR.branch.EQ.17.OR.branch.EQ.18.OR.branch.EQ.19)the
     &n
      WHERE(1)=Digit
      return
      elseif(branch.EQ.4.OR.branch.EQ.5.OR.branch.EQ.6.OR.branch.EQ.7)th
     &en
      WHERE(1)=Chrctr
      return
      elseif(branch.EQ.8.OR.branch.EQ.20.OR.branch.EQ.21.OR.branch.EQ.22
     &)then
      WHERE(1)=Inte
      return
      elseif(branch.EQ.9)then
      WHERE(1)=Fpl
      return
      elseif(branch.EQ.10)then
      WHERE(1)=Dpl(1)
      WHERE(2)=Dpl(2)
      return
      elseif(branch.EQ.11.OR.branch.EQ.12.OR.branch.EQ.15.OR.branch.EQ.1
     &6)then
      y=-1
      x=gsqrt(y)
      elseif(branch.NE.23)then
      cur1=0
      cur2=0
      WHERE(1)=Lenstr
      do 50 i=1,Lenstr
      chr=getchr(String,cur1)
      if(mod(i,qpcpv(jjjj)).EQ.0)String(i/qpcpv(jjjj))=blank
      call puticr(chr,WHERE(2),cur2)
50    continue
      return
      endif
      return
      
      end
C* :1 * 
      
