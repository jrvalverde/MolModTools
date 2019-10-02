
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dipa"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dipa.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "dipa.web"
      subroutine dipa(XIP,YIP,ZIP,XIPI,YIPI,ZIPI,IXYZNT)
      implicit none
      double precision Aiab,atemp,axcnst,aycnst,azcnst,Biab,Cicd,D1abx,D
     &1aby,D1abz,D1cdx,D1cdy,D1cdz,d2apr,d2aqr,Dicd,Ep2i,Eq2i,Pqx,Pqy
      double precision Pqz,Rhot2,rhot2a,XIP,xipa,XIPI,xipj,xipk,xtemp,Xy
     &za1,Xyza2,Xyza3,Xyza4,Xyzb1,Xyzb2,Xyzb3,Xyzb4,Xyzc1,Xyzc2,Xyzc3
      double precision Xyzc4,YIP,yipa,YIPI,yipj,yipk,ytemp,ZIP,zipa,ZIPI
     &,zipj,zipk,ztemp
      integer i,ind,ip,ip1,ip16,ip4,ip64,ipi,ipj,ipk,IXYZNT,j,k,l,Lamax,
     &Lbmax,Lcmax,Ldmax,Lpmax,Lpqmax
      integer Lqmax
      dimension XIP(2),YIP(2),ZIP(2),XIPI(2),xipj(2),xipk(2),YIPI(2),yip
     &j(2),yipk(2),ZIPI(2),zipj(2),zipk(2)
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      common/rhot2/Rhot2
      common/ipdrv/Ep2i,Eq2i,Aiab,Biab,Cicd,Dicd,Pqx,Pqy,Pqz,D1abx,D1aby
     &,D1abz,D1cdx,D1cdy,D1cdz,Xyza1(4),Xyzb1(4),Xyzc1(4),Xyza2(4),Xyzb2
     &(4),Xyzc2(4),Xyza3(4),Xyzb3(4),Xyzc3(4),Xyza4(4),Xyzb4(4),Xyzc4(4)
      
      
      
      
      
      rhot2a=Rhot2*Aiab
      axcnst=D1abx+Pqx*rhot2a
      aycnst=D1aby+Pqy*rhot2a
      azcnst=D1abz+Pqz*rhot2a
      if(Lpmax.NE.1)then
      d2apr=rhot2a*Ep2i
      if(Lamax.NE.1)then
      Xyza1(2)=-Biab-d2apr
      Xyza1(3)=Xyza1(2)+Xyza1(2)
      Xyza1(4)=Xyza1(3)+Xyza1(2)
      endif
      if(Lbmax.NE.1)then
      Xyza2(2)=Aiab-d2apr
      Xyza2(3)=Xyza2(2)+Xyza2(2)
      Xyza2(4)=Xyza2(3)+Xyza2(2)
      endif
      endif
      if(Lqmax.NE.1)then
      d2aqr=rhot2a*Eq2i
      if(Lcmax.NE.1)then
      Xyza3(2)=d2aqr
      Xyza3(3)=Xyza3(2)+Xyza3(2)
      Xyza3(4)=Xyza3(3)+Xyza3(2)
      endif
      if(Ldmax.NE.1)then
      Xyza4(2)=d2aqr
      Xyza4(3)=Xyza4(2)+Xyza4(2)
      Xyza4(4)=Xyza4(3)+Xyza4(2)
      endif
      endif
      ind=IXYZNT-1
      do 100 i=1,Lamax
      ipi=(i-1)*64+IXYZNT-1
      do 50 j=1,Lbmax
      ipj=(j-1)*16+ipi
      do 20 k=1,Lcmax
      ipk=(k-1)*4+ipj
      do 10 l=1,Ldmax
      ip=ipk+l
      ind=ind+1
      xtemp=XIP(ip)
      ytemp=YIP(ip)
      ztemp=ZIP(ip)
      xipa=axcnst*xtemp
      yipa=aycnst*ytemp
      zipa=azcnst*ztemp
      if(i.NE.1)then
      ip64=ip-64
      xtemp=XIP(ip64)
      ytemp=YIP(ip64)
      ztemp=ZIP(ip64)
      atemp=Xyza1(i)
      xipa=xipa+atemp*xtemp
      yipa=yipa+atemp*ytemp
      zipa=zipa+atemp*ztemp
      endif
      if(j.NE.1)then
      ip16=ip-16
      xtemp=XIP(ip16)
      ytemp=YIP(ip16)
      ztemp=ZIP(ip16)
      atemp=Xyza2(j)
      xipa=xipa+atemp*xtemp
      yipa=yipa+atemp*ytemp
      zipa=zipa+atemp*ztemp
      endif
      if(k.NE.1)then
      ip4=ip-4
      xtemp=XIP(ip4)
      ytemp=YIP(ip4)
      ztemp=ZIP(ip4)
      atemp=Xyza3(k)
      xipa=xipa+atemp*xtemp
      yipa=yipa+atemp*ytemp
      zipa=zipa+atemp*ztemp
      endif
      if(l.NE.1)then
      ip1=ip-1
      xtemp=XIP(ip1)
      ytemp=YIP(ip1)
      ztemp=ZIP(ip1)
      atemp=Xyza4(l)
      xipa=xipa+atemp*xtemp
      yipa=yipa+atemp*ytemp
      zipa=zipa+atemp*ztemp
      endif
      XIPI(ind)=xipa
      YIPI(ind)=yipa
      ZIPI(ind)=zipa
10    continue
20    continue
50    continue
100   continue
      
      
      ind=IXYZNT-1
      do 200 i=1,Lamax
      ipi=(i-1)*64+IXYZNT-1
      do 150 j=1,Lbmax
      ipj=(j-1)*16+ipi
      do 120 k=1,Lcmax
      ipk=(k-1)*4+ipj
      do 110 l=1,Ldmax
      ip=ipk+l
      ind=ind+1
      XIP(ind)=XIP(ip)
      YIP(ind)=YIP(ip)
      ZIP(ind)=ZIP(ip)
110   continue
120   continue
150   continue
200   continue
      return
      
      end
C* :1 * 
      
