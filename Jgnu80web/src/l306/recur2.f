
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 recur2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "recur2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "recur2.web"
      subroutine recur2(NHI,LALO,LAHI,LBLO,LBHI,RKA,RKB,QQ)
      implicit none
      real*8 fka,fkb,one,QQ,RKA,RKB
      integer la,LAHI,lahm1,LALO,lalop2,latru,lb,LBHI,lbhm1,LBLO,lblop2,
     &lbtru,n,NHI
      
      
      dimension QQ(7,7,7)
      save one
      data one/1.0D0/
      
      fka=one/RKA
      fkb=one/RKB
      lahm1=max0(LAHI-1,1)
      lbhm1=max0(LBHI-1,1)
      lalop2=LALO+2
      lblop2=LBLO+2
      if(lblop2.LE.LBHI)then
      do 50 lb=LBHI,lblop2,-1
      lbtru=lb-1
      do 20 n=1,NHI
      QQ(n+1,LAHI,lb-2)=QQ(n+1,LAHI,lb)+(2*lbtru-1)*fkb*QQ(n,LAHI,lb-1)
      QQ(n+1,lahm1,lb-2)=QQ(n+1,lahm1,lb)+(2*lbtru-1)*fkb*QQ(n,lahm1,lb-
     &1)
20    continue
50    continue
      endif
      
      if(lalop2.LE.LAHI)then
      do 100 la=LAHI,lalop2,-1
      latru=la-1
      do 60 n=1,NHI
      QQ(n+1,la-2,LBHI)=QQ(n+1,la,LBHI)+(2*latru-1)*fka*QQ(n,la-1,LBHI)
      QQ(n+1,la-2,lbhm1)=QQ(n+1,la,lbhm1)+(2*latru-1)*fka*QQ(n,la-1,lbhm
     &1)
60    continue
100   continue
      endif
      
      if(lalop2.LE.LAHI.AND.lblop2.LE.LBHI)then
      do 150 lb=LBHI,lblop2,-1
      lbtru=lb-1
      do 120 la=LAHI,lalop2,-1
      latru=la-1
      do 110 n=1,NHI
      QQ(n+1,la-2,lb-2)=QQ(n+1,la-2,lb)+(2*lbtru-1)*fkb*QQ(n,la-2,lb-1)
110   continue
120   continue
150   continue
      endif
      
      return
      end
C* :1 * 
      
