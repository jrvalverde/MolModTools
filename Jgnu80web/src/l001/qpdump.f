
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpdump"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpdump.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "qpdump.web"
      subroutine qpdump
      implicit none
      integer i,lenwr
      real Qpblnk,Qpcaps,Qpnoab
      integer Lastyp,Status,Chrctr,Digit,Inte,String,Tcursr,Lcursr,State
     &,Tran,Lenstr
      real Fp
      double precision Dp
      common/qpstat/Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,Lcursr,
     &State,Tran,Lenstr,String(64),Qpblnk,Qpnoab,Qpcaps
      
      
      
      
      
      
99001 format('  LASTYP=',i8,', LASTYP=',a4,', STATUS=',i3,',            
     &     CHRCTR=',a4,', DIGIT=',a4,/,'  INTEGER=',i4,', FP=',f10.3,', 
     &DP=',d14.4,/,'  TCURSR=',i2,', LCURSR=',i5,', STATE=',a4,', TRAN='
     &,i2,/,'  LENSTR=',i2,', STRING: ',64A4)
      
      lenwr=(Lenstr-1)/4+1
      write(6,99001)Lastyp,Lastyp,Status,Chrctr,Digit,Inte,Fp,Dp,Tcursr,
     &Lcursr,State,Tran,Lenstr,(String(i),i=1,lenwr)
      return
      
      end
C* :1 * 
      
