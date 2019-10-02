
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpany1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpany1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "qpany1.web"
      integer function qpany1(TYPE,LINE,LENGTH,LCURSR,CHRCTR)
      implicit none
      integer chr,CHRCTR,getchr,intchr,LCURSR,LENGTH,LINE,Maxdep,Maxkey,
     &mycrsr,Qpabrv,Qpambg,Qpdpth,Qpend,Qperr,Qpexit,Qpfail,Qpok,Qprecr,
     &Qpret
      integer Stack,t,TYPE,val
      dimension LINE(*)
      logical ifalph
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxkey
      
      val=Qpfail
      
      if(LCURSR.LT.LENGTH)then
      mycrsr=LCURSR
      
      chr=getchr(LINE,mycrsr)
      if(TYPE.LE.0)then
      t=-TYPE
      if(t.EQ.2)then
      if(.NOT.ifalph(chr))goto 100
      elseif(t.EQ.3)then
      if(.NOT.ifalph(chr).AND.(intchr(chr,10).EQ.-1))goto 100
      elseif(t.NE.4)then
      if(intchr(chr,10).EQ.-1)goto 100
      endif
      val=Qpok
      LCURSR=mycrsr
      CHRCTR=chr
      else
      t=intchr(chr,TYPE)
      if(t.NE.-1)then
      CHRCTR=t
      val=Qpok
      LCURSR=mycrsr
      endif
      endif
      endif
100   qpany1=val
      return
      
      end
C* :1 * 
      
