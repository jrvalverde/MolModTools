
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpambi"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpambi.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "qpambi.web"
      integer function qpambi(TABLE,FOUND,LENF)
      implicit none
      integer Chrctr,cursor,Digit,FOUND,i,Inte,jjjj,Lastyp,Lcursr,len,LE
     &NF,Lenstr,Maxdep,Maxkey,nvals,oneamb,Qpabrv,Qpambg,Qpblnk,Qpcaps
      integer qpcpv,qpcstr,qpdcod,Qpdpth,Qpend,Qperr,Qpexit,Qpfail,Qpnoa
     &b,Qpok,Qprecr,Qpret,qptval,scr,Stack,State,Status,String,TABLE,Tcu
     &rsr
      integer tmp,Tran,val
      dimension scr(16),TABLE(*),FOUND(*)
      real Fp
      double precision Dp
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxkey
      
      
      
      cursor=Tcursr
      oneamb=0
      
      
100   call qpskip(cursor)
      len=qptval(TABLE,cursor)
      
      tmp=qpdcod(len)
      
      if(tmp.NE.1)then
      if(tmp.NE.2)then
      
      if(tmp.EQ.16.OR.tmp.EQ.15)then
      
      val=Qpok
      if(oneamb.EQ.1)val=Qpambg
      goto 200
      
      elseif(tmp.EQ.14)then
      
      len=qptval(TABLE,cursor)
      if(len.GT.64.OR.len.LE.0)then
      
      val=Qperr
      goto 200
      else
      nvals=(len-1)/qpcpv(jjjj)+1
      do 105 i=1,nvals
      tmp=qptval(TABLE,cursor)
105   continue
      goto 100
      endif
      else
      
      if(tmp.NE.0)goto 100
      val=Qperr
      goto 200
      endif
      endif
      endif
      
      if(iabs(len).GT.Maxkey)then
      val=Qperr
      else
      nvals=(iabs(len)-1)/qpcpv(jjjj)+1
      do 150 i=1,nvals
      scr(i)=qptval(TABLE,cursor)
150   continue
      if(len.LT.0)goto 100
      
      val=qpcstr(scr,len,FOUND,LENF)
      if(val.EQ.Qpfail)goto 100
      if(val.EQ.Qpabrv)oneamb=1
      if(val.EQ.Qpabrv)goto 100
      
      val=Qpfail
      endif
200   qpambi=val
      return
      
      end
C* :1 * 
      
