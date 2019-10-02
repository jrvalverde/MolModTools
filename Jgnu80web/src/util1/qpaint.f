
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpaint"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpaint.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "qpaint.web"
      integer function qpaint(BASE,LINE,LENGTH,LCURSR,INTE,IDEL)
      implicit none
      integer BASE,chr,getchr,IDEL,intchr,INTE,iord,LCURSR,LENGTH,LINE,M
     &axdep,Maxkey,mycrsr,n,Qpabrv,Qpambg,Qpdpth,Qpend,Qperr,Qpexit
      integer Qpfail,Qpok,Qprecr,Qpret,Stack,t,val
      dimension LINE(*)
      logical gotone,neg,ifalph
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxkey
      
      val=Qpfail
      if(LCURSR.GE.LENGTH)goto 400
      
      neg=.FALSE.
      n=0
      gotone=.FALSE.
      mycrsr=LCURSR
      
      chr=getchr(LINE,mycrsr)
      if(chr.EQ.iord('+'))goto 200
      if(chr.EQ.iord('-'))then
      neg=.TRUE.
      goto 200
      endif
      
100   t=intchr(chr,BASE)
      if(t.EQ.-1)then
      
      if(.NOT.gotone)goto 400
      
      if(IDEL.EQ.0.AND.ifalph(chr))goto 400
      if(IDEL.EQ.3.AND.(chr.EQ.iord('.').OR.ifalph(chr)))goto 400
      if(IDEL.EQ.2.AND.chr.EQ.iord('.'))goto 400
      mycrsr=mycrsr-1
      goto 300
      else
      gotone=.TRUE.
      n=BASE*n+t
      endif
200   if(mycrsr.LT.LENGTH)then
      chr=getchr(LINE,mycrsr)
      goto 100
      
      elseif(.NOT.gotone)then
      goto 400
      endif
      
300   val=Qpok
      if(neg)n=-n
      INTE=n
      LCURSR=mycrsr
400   qpaint=val
      return
      
      end
C* :1 * 
      
