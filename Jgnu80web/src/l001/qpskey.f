
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpskey"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpskey.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "qpskey.web"
      integer function qpskey(TABLE,LINE,LENGTH)
      implicit none
      integer chr,Chrctr,cursor,Digit,getchr,i,intchr,Inte,jjjj,Lastyp,L
     &cursr,len,LENGTH,lenkey,Lenstr,LINE,Maxdep,Maxlen,nvals,Qpabrv
      integer Qpambg,qpambi,Qpblnk,Qpcaps,qpcpv,qpcstr,Qpdpth,Qpend,Qper
     &r,Qpexit,Qpfail,Qpnoab,Qpok,Qprecr,Qpret,qptval,scr,scr2,Stack,Sta
     &te
      integer Status,String,TABLE,Tcursr,Tran,val
      dimension scr(16),scr2(16),LINE(*),TABLE(*)
      logical ifalph
      real Fp
      double precision Dp
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxlen
      
      
      
      val=Qpfail
      lenkey=Lastyp
      if(lenkey.GT.Maxlen)then
      
      val=Qperr
      else
      nvals=(lenkey-1)/qpcpv(jjjj)+1
      do 50 i=1,nvals
      scr2(i)=qptval(TABLE,Tcursr)
50    continue
      if(Lcursr.LT.LENGTH)then
      cursor=Lcursr
      len=0
60    chr=getchr(LINE,cursor)
      if((ifalph(chr)).OR.(intchr(chr,10).NE.-1))then
      call puticr(chr,scr,len)
      if(cursor.LT.LENGTH)goto 60
      else
      cursor=cursor-1
      endif
      if(len.GT.0)val=qpcstr(scr2,lenkey,scr,len)
      if(len.LE.0)val=Qpfail
      if(val.EQ.Qpabrv)then
      if(Qpnoab.EQ.0)then
      
      val=qpambi(TABLE,scr,len)
      else
      val=Qpfail
      goto 100
      endif
      endif
      if(val.EQ.Qpok)then
      Lcursr=cursor
      Lenstr=len
      len=(len-1)/qpcpv(jjjj)+1
      do 70 i=1,len
      String(i)=scr(i)
70    continue
      endif
      endif
      endif
100   qpskey=val
      return
      
      end
C* :1 * 
      
