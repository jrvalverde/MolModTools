
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpword"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpword.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "qpword.web"
      integer function qpword(IFDLIM,TABLE,TCURSR,LINE,LENGTH,LCURSR,STR
     &ING,LENSTR)
      implicit none
      integer c,chr,cur,cursr,delims,getchr,i,IFDLIM,incur,intchr,j,jjjj
     &,LCURSR,LENGTH,LENSTR,LINE,Maxdep,Maxkey,n,nch
      integer ndlims,outcur,Qpabrv,Qpambg,qpcpv,Qpdpth,Qpend,Qperr,Qpexi
     &t,Qpfail,Qpok,Qprecr,Qpret,qptval,Stack,STRING,t,TABLE,TCURSR,val
      dimension LINE(*),STRING(*),TABLE(*)
      logical ifalph,gotone,ifeq,streq,streqc
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxkey
      
      val=Qpfail
      
      ndlims=0
      if(IFDLIM.EQ.1)ndlims=qptval(TABLE,TCURSR)
      n=(ndlims-1)/qpcpv(jjjj)+1
      if(LCURSR.GE.LENGTH)goto 400
      
      incur=LCURSR
      outcur=0
      gotone=.FALSE.
      
100   chr=getchr(LINE,incur)
      
      if(ndlims.GT.0)then
      cur=0
      cursr=TCURSR
      
      do 150 i=1,n
      delims=qptval(TABLE,cursr)
      cur=0
      nch=qpcpv(jjjj)
      do 120 j=1,nch
      c=getchr(delims,cur)
      ifeq=streq(chr,c,1)
      if(ifeq)goto 200
120   continue
      
150   continue
      
      elseif((.NOT.ifalph(chr)).AND.(intchr(chr,10).EQ.-1))then
      goto 200
      endif
      gotone=.TRUE.
      call puticr(chr,STRING,outcur)
      LENSTR=outcur
      if(incur.LT.LENGTH)goto 100
      goto 300
      
200   if(.NOT.gotone)goto 400
      incur=incur-1
300   val=Qpok
      LCURSR=incur
400   if(ndlims.GT.0)then
      
      do 450 i=1,n
      t=qptval(delims,TCURSR)
450   continue
      endif
      qpword=val
      return
      
      end
C* :1 * 
      
