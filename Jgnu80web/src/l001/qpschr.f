
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpschr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpschr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "qpschr.web"
      integer function qpschr(TABLE,LINE,LENGTH)
      implicit none
      integer Chrctr,Digit,getchr,i,Inte,j,jjjj,Lastyp,Lcursr,len,LENGTH
     &,Lenstr,Maxdep,Maxlen,mycrsr,nwrds,Qpabrv,Qpambg,Qpblnk,Qpcaps
      integer qpcpv,Qpdpth,Qpend,Qperr,Qpexit,Qpfail,Qpnoab,Qpok,Qprecr,
     &Qpret,qptval,Stack,State,Status,String,t,Tcursr,Tran,val
      integer LINE(*),TABLE(*)
      integer tword(16),lword(16)
      logical ifeq,streq,streqc
      real Fp
      double precision Dp
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxlen
      
      
      
      val=Qpfail
      len=-Lastyp
      nwrds=(len-1)/qpcpv(jjjj)+1
      do 100 i=1,nwrds
      tword(i)=qptval(TABLE,Tcursr)
100   continue
      mycrsr=Lcursr
      j=0
      do 200 i=1,len
      if(mycrsr.GE.LENGTH)goto 300
      t=getchr(LINE,mycrsr)
      call puticr(t,lword,j)
200   continue
      if(Qpcaps.EQ.0)ifeq=streqc(tword,lword,len)
      if(Qpcaps.NE.0)ifeq=streq(tword,lword,len)
      if(ifeq)then
      Lcursr=mycrsr
      Lenstr=len
      do 250 i=1,nwrds
      String(i)=tword(i)
250   continue
      val=Qpok
      endif
300   qpschr=val
      return
      
      end
C* :1 * 
      
