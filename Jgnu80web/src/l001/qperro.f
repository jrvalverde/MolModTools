
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qperro"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qperro.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "qperro.web"
      subroutine qperro(LINE,LENGTH,IOUT)
      implicit none
      integer blank,Chrctr,Digit,i,Inte,IOUT,jjjj,Lastyp,Lcursr,LENGTH,L
     &enstr,LINE,loc,max,Maxdep,Maxkey,n,Qpabrv,Qpambg,Qpblnk
      integer Qpcaps,qpcpv,Qpdpth,Qpend,Qperr,Qpexit,Qpfail,Qpnoab,Qpok,
     &Qprecr,Qpret,quest,Stack,State,Status,String,Tcursr,Tran,wordn
      dimension LINE(*)
      real Fp
      double precision Dp
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxkey
      data blank/'    '/,quest/'?'/
      
      
      
      if(Status.EQ.Qpok)then
      write(IOUT,99001)
      
99001 format('  QPERR --- EVERYTHING SEEMS OK IN THE PARSER.')
      
      return
      
      elseif(Status.EQ.Qpambg)then
      write(IOUT,99002)
      
      
99002 format('  QPERR --- AN AMBIGUOUS KEYWORD WAS DETECTED.')
      
      elseif(Status.EQ.Qpfail)then
      write(IOUT,99003)
      
      
99003 format('  QPERR --- A SYNTAX ERROR WAS DETECTED IN THE INPUT ','LI
     &NE.')
      
      elseif(Status.NE.Qperr)then
      
      write(IOUT,99004)Status
      
      
99004 format('  QPERR --- UNKNOWN CONDITION. STATUS =',i10)
      else
      write(IOUT,99005)
      
      
99005 format('  QPERR --- AN ERROR IN THE PARSE TABLE WAS DETECTED.')
      endif
      
      wordn=Lcursr/qpcpv(jjjj)-4
      if(wordn.LE.0)wordn=1
      n=40
      max=LENGTH-(wordn-1)*4
      if(n.GT.max)n=max
      call strout(IOUT,LINE(wordn),n,1)
      loc=Lcursr-(wordn-1)*4+1
      write(IOUT,99006)(blank,i=1,loc),quest
      
99006 format(132A1)
      
      write(IOUT,99007)State,Tcursr,Lcursr
      
99007 format('  LAST STATE: ',a4,', TCURSR:',i4,', LCURSR:',i3)
      
      return
      
      end
C* :1 * 
      
