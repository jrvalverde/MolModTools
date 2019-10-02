
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 recurf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "recurf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "recurf.web"
      subroutine recurf(LALO,LAHI,LBLO,LBHI,RKA,RKB)
      implicit none
      real*8 F,fka,fkb,one,Pt,Ptpow,RKA,RKB
      integer i,la,LAHI,lahm1,LALO,lalop2,latru,lb,LBHI,lbhm1,LBLO,lblop
     &2,lbtru,Npts
      
      
      common/ptwtdt/Ptpow(50,7),F(50,7,7),Pt(50),Npts
      dimension fka(50),fkb(50)
      save one
      data one/1.0D0/
      
      lahm1=max0(LAHI-1,1)
      lbhm1=max0(LBHI-1,1)
      lalop2=LALO+2
      lblop2=LBLO+2
      do 100 i=1,Npts
      fka(i)=one/(RKA*Pt(i))
      fkb(i)=one/(RKB*Pt(i))
100   continue
      
      if(lblop2.LE.LBHI)then
      do 150 lb=LBHI,lblop2,-1
      lbtru=lb-1
      do 120 i=1,Npts
      F(i,LAHI,lb-2)=F(i,LAHI,lb)+(2*lbtru-1)*fkb(i)*F(i,LAHI,lb-1)
      F(i,lahm1,lb-2)=F(i,lahm1,lb)+(2*lbtru-1)*fkb(i)*F(i,lahm1,lb-1)
120   continue
150   continue
      endif
      
      if(lalop2.LE.LAHI)then
      do 200 la=LAHI,lalop2,-1
      latru=la-1
      do 160 i=1,Npts
      F(i,la-2,LBHI)=F(i,la,LBHI)+(2*latru-1)*fka(i)*F(i,la-1,LBHI)
      F(i,la-2,lbhm1)=F(i,la,lbhm1)+(2*latru-1)*fka(i)*F(i,la-1,lbhm1)
160   continue
200   continue
      endif
      
      if(lalop2.LE.LAHI.AND.lblop2.LE.LBHI)then
      do 250 lb=LBHI,lblop2,-1
      lbtru=lb-1
      do 220 la=LAHI,lalop2,-1
      latru=la-1
      do 210 i=1,Npts
      F(i,la-2,lb-2)=F(i,la-2,lb)+(2*lbtru-1)*fkb(i)*F(i,la-2,lb-1)
210   continue
220   continue
250   continue
      endif
      return
      end
C* :1 * 
      
