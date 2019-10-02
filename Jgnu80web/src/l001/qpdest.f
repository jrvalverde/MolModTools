
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpdest"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpdest.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "qpdest.web"
      integer function qpdest(TABLE,TCURSR,STATE)
      implicit none
      integer dest,i,iord,jjjj,Maxdep,Maxkey,mycrsr,n,Qpabrv,Qpambg,qpcp
     &v,qpdcod,Qpdpth,Qpend,Qperr,Qpexit,Qpfail,Qpok,Qprecr,Qpret
      integer qptval,Stack,STATE,TCURSR,tmp,tran,type,val
      integer TABLE(*)
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxkey
      
      
      mycrsr=TCURSR
      val=Qpok
      dest=qptval(TABLE,mycrsr)
      call captlz(dest,dest,4)
      if(dest.EQ.iord('RET'))val=Qpret
      if(dest.EQ.iord('EXI'))val=Qpexit
      if(dest.EQ.iord('FAI'))val=Qpfail
      if(val.EQ.Qpok)then
      if(dest.NE.0)TCURSR=0
      if(dest.NE.0)goto 200
      call qpskip(TCURSR)
      else
      call qpskip(TCURSR)
      if(val.NE.Qpret)goto 300
      dest=0
      endif
100   tmp=qptval(TABLE,TCURSR)
      type=qpdcod(tmp)
      if(type.NE.15)then
      if(type.NE.16)then
      if(type.EQ.14)then
      tmp=qptval(TABLE,TCURSR)
      elseif(type.NE.1)then
      if(type.NE.2)then
      if(type.EQ.0)then
      
      val=Qperr
      goto 300
      else
      call qpskip(TCURSR)
      goto 100
      endif
      endif
      endif
      if(iabs(tmp).GT.64)then
      val=Qperr
      goto 300
      else
      n=(iabs(tmp)-1)/qpcpv(jjjj)+1
      if(n.LE.0)then
      val=Qperr
      goto 300
      else
      do 105 i=1,n
      tmp=qptval(TABLE,TCURSR)
105   continue
      call qpskip(TCURSR)
      goto 100
      endif
      endif
      endif
      endif
200   STATE=qptval(TABLE,TCURSR)
      if(dest.NE.0)then
      if(dest.NE.STATE)goto 100
      tran=0
      val=Qpok
      endif
300   qpdest=val
      return
      
      end
C* :1 * 
      
