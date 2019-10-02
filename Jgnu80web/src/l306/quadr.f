
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 quadr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "quadr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "quadr.web"
      subroutine quadr(LMALO,LMAHI,LMBLO,LMBHI,NLO,NHI,QQ)
      implicit none
      real*8 F,Pt,Ptpow,QQ
      integer i,la,lb,LMAHI,LMALO,LMBHI,LMBLO,n,NHI,NLO,Npts
      common/ptwtdt/Ptpow(50,7),F(50,7,7),Pt(50),Npts
      dimension QQ(7,7,7)
      
      
      do 100 lb=LMBLO,LMBHI
      do 50 la=LMALO,LMAHI
      do 20 n=NLO,NHI
      do 10 i=1,Npts
      QQ(n,la,lb)=QQ(n,la,lb)+Ptpow(i,n)*F(i,la,lb)
10    continue
20    continue
50    continue
100   continue
      
      return
      end
C* :1 * 
      
