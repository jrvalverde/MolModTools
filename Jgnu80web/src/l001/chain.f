
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 chain"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "chain.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 41 "chain.web"
      subroutine chain(NCHAIN)
      implicit none
      integer In,Iop,Iout,iov,Ipunch,j,l,NCHAIN,nn
      
      
      
      common/io/In,Iout,Ipunch
      common/iop/Iop(50)
      dimension iov(36)
      data nn/36/
      data iov/1,101,102,103,105,202,301,302,303,305,306,310,311,314,401
     &,501,502,503,505,601,602,701,702,703,705,716,801,802,803,901,909,9
     &10,911,912,913,9999/
      
100   do 200 j=1,nn
      if(iov(j).EQ.NCHAIN)goto 300
200   continue
      write(Iout,99001)NCHAIN
99001 format('1  NCHAIN = ',i9,',  NOT RECOGNIZABLE.')
      call lnk1e
      stop
300   if(iov(j).NE.1)then
      call fileio(9,l,l,l,l)
      call ilsw(2,20,l)
      if(l.EQ.0)write(Iout,99002)iov(j)
99002 format('+',120x,'(ENTER ',i4,')')
      endif
      if(j.EQ.1)then
      call l001(NCHAIN)
      elseif(j.EQ.2)then
      call l101(NCHAIN)
      elseif(j.EQ.3)then
      call l102(NCHAIN)
      elseif(j.EQ.4)then
      call l103(NCHAIN)
      elseif(j.EQ.5)then
      call l105(NCHAIN)
      elseif(j.EQ.6)then
      call l202(NCHAIN)
      elseif(j.EQ.7)then
      call l301(NCHAIN)
      elseif(j.EQ.8)then
      call l302(NCHAIN)
      elseif(j.EQ.9)then
      call l303(NCHAIN)
      elseif(j.EQ.10)then
      call l305(NCHAIN)
      elseif(j.EQ.11)then
      call l306(NCHAIN)
      elseif(j.EQ.12)then
      call l310(NCHAIN)
      elseif(j.EQ.13)then
      call l311(NCHAIN)
      elseif(j.EQ.14)then
      call l314(NCHAIN)
      elseif(j.EQ.15)then
      call l401(NCHAIN)
      elseif(j.EQ.16)then
      call l501(NCHAIN)
      elseif(j.EQ.17)then
      call l502(NCHAIN)
      elseif(j.EQ.18)then
      call l503(NCHAIN)
      elseif(j.EQ.19)then
      call l505(NCHAIN)
      elseif(j.EQ.20)then
      call l601(NCHAIN)
      elseif(j.EQ.21)then
      call l602(NCHAIN)
      elseif(j.EQ.22)then
      call l701(NCHAIN)
      elseif(j.EQ.23)then
      call l702(NCHAIN)
      elseif(j.EQ.24)then
      call l703(NCHAIN)
      elseif(j.EQ.25)then
      call l705(NCHAIN)
      elseif(j.EQ.26)then
      call l716(NCHAIN)
      elseif(j.EQ.27)then
      call l801(NCHAIN)
      elseif(j.EQ.28)then
      call l802(NCHAIN)
      elseif(j.EQ.29)then
      call l803(NCHAIN)
      elseif(j.EQ.30)then
      call l901(NCHAIN)
      elseif(j.EQ.31)then
      call l909(NCHAIN)
      elseif(j.EQ.32)then
      call l910(NCHAIN)
      elseif(j.EQ.33)then
      call l911(NCHAIN)
      elseif(j.EQ.34)then
      call l912(NCHAIN)
      elseif(j.EQ.35)then
      call l913(NCHAIN)
      elseif(j.EQ.36)then
      goto 400
      else
      write(Iout,99003)iov(j)
99003 format('   LINK ',i4,' NOT IMPLEMENTED')
      goto 400
      endif
      goto 100
400   call l9999(NCHAIN)
      
      stop
      end
C* :1 * 
      
