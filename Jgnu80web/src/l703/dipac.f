
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dipac"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dipac.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 32 "dipac.web"
      subroutine dipac(XIP,YIP,ZIP,XIPI,YIPI,ZIPI,XIPK,YIPK,ZIPK,IXYZNT)
      implicit none
      double precision Aiab,atemp,axcnst,aycnst,azcnst,Biab,Cicd,ctemp,c
     &xcnst,cycnst,czcnst,D1abx,D1aby,D1abz,D1cdx,D1cdy,D1cdz,d2apr,d2aq
     &r,d2cpr
      double precision d2cqr,Dicd,Ep2i,Eq2i,Pqx,Pqy,Pqz,Rhot2,rhot2a,rho
     &t2c,XIP,xipa,xipc,XIPI,xipj,XIPK,xtemp,Xyza1,Xyza2,Xyza3
      double precision Xyza4,Xyzb1,Xyzb2,Xyzb3,Xyzb4,Xyzc1,Xyzc2,Xyzc3,X
     &yzc4,YIP,yipa,yipc,YIPI,yipj,YIPK,ytemp,zero,ZIP,zipa,zipc
      double precision ZIPI,zipj,ZIPK,ztemp
      integer i,ind,ip,ip1,ip16,ip4,ip64,ipi,ipj,ipk,IXYZNT,j,k,l,Lamax,
     &Lbmax,Lcmax,Ldmax,Lpmax,Lpqmax
      integer Lqmax
      dimension XIP(2),YIP(2),ZIP(2),XIPI(2),xipj(2),XIPK(2),YIPI(2),yip
     &j(2),YIPK(2),ZIPI(2),zipj(2),ZIPK(2)
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      common/rhot2/Rhot2
      common/ipdrv/Ep2i,Eq2i,Aiab,Biab,Cicd,Dicd,Pqx,Pqy,Pqz,D1abx,D1aby
     &,D1abz,D1cdx,D1cdy,D1cdz,Xyza1(4),Xyzb1(4),Xyzc1(4),Xyza2(4),Xyzb2
     &(4),Xyzc2(4),Xyza3(4),Xyzb3(4),Xyzc3(4),Xyza4(4),Xyzb4(4),Xyzc4(4)
      data zero/0.D0/
      
      
      
      
      
      rhot2a=Rhot2*Aiab
      rhot2c=Rhot2*Cicd
      axcnst=D1abx+Pqx*rhot2a
      aycnst=D1aby+Pqy*rhot2a
      azcnst=D1abz+Pqz*rhot2a
      cxcnst=D1cdx-Pqx*rhot2c
      cycnst=D1cdy-Pqy*rhot2c
      czcnst=D1cdz-Pqz*rhot2c
      if(Lpmax.NE.1)then
      d2apr=rhot2a*Ep2i
      d2cpr=rhot2c*Ep2i
      if(Lamax.NE.1)then
      Xyza1(2)=-Biab-d2apr
      Xyzc1(2)=d2cpr
      Xyza1(3)=Xyza1(2)+Xyza1(2)
      Xyzc1(3)=Xyzc1(2)+Xyzc1(2)
      Xyza1(4)=Xyza1(3)+Xyza1(2)
      Xyzc1(4)=Xyzc1(3)+Xyzc1(2)
      endif
      if(Lbmax.NE.1)then
      Xyza2(2)=Aiab-d2apr
      Xyzc2(2)=d2cpr
      Xyza2(3)=Xyza2(2)+Xyza2(2)
      Xyzc2(3)=Xyzc2(2)+Xyzc2(2)
      Xyza2(4)=Xyza2(3)+Xyza2(2)
      Xyzc2(4)=Xyzc2(3)+Xyzc2(2)
      endif
      endif
      if(Lqmax.NE.1)then
      d2aqr=rhot2a*Eq2i
      d2cqr=rhot2c*Eq2i
      if(Lcmax.NE.1)then
      Xyza3(2)=d2aqr
      Xyzc3(2)=-Dicd-d2cqr
      Xyza3(3)=Xyza3(2)+Xyza3(2)
      Xyzc3(3)=Xyzc3(2)+Xyzc3(2)
      Xyza3(4)=Xyza3(3)+Xyza3(2)
      Xyzc3(4)=Xyzc3(3)+Xyzc3(2)
      endif
      if(Ldmax.NE.1)then
      Xyza4(2)=d2aqr
      Xyzc4(2)=+Cicd-d2cqr
      Xyza4(3)=Xyza4(2)+Xyza4(2)
      Xyzc4(3)=Xyzc4(2)+Xyzc4(2)
      Xyza4(4)=Xyza4(3)+Xyza4(2)
      Xyzc4(4)=Xyzc4(3)+Xyzc4(2)
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
      xipc=cxcnst*xtemp
      yipa=aycnst*ytemp
      yipc=cycnst*ytemp
      zipa=azcnst*ztemp
      zipc=czcnst*ztemp
      if(i.NE.1)then
      ip64=ip-64
      xtemp=XIP(ip64)
      ytemp=YIP(ip64)
      ztemp=ZIP(ip64)
      atemp=Xyza1(i)
      ctemp=Xyzc1(i)
      xipa=xipa+atemp*xtemp
      xipc=xipc+ctemp*xtemp
      yipa=yipa+atemp*ytemp
      yipc=yipc+ctemp*ytemp
      zipa=zipa+atemp*ztemp
      zipc=zipc+ctemp*ztemp
      endif
      if(j.NE.1)then
      ip16=ip-16
      xtemp=XIP(ip16)
      ytemp=YIP(ip16)
      ztemp=ZIP(ip16)
      atemp=Xyza2(j)
      ctemp=Xyzc2(j)
      xipa=xipa+atemp*xtemp
      xipc=xipc+ctemp*xtemp
      yipa=yipa+atemp*ytemp
      yipc=yipc+ctemp*ytemp
      zipa=zipa+atemp*ztemp
      zipc=zipc+ctemp*ztemp
      endif
      if(k.NE.1)then
      ip4=ip-4
      xtemp=XIP(ip4)
      ytemp=YIP(ip4)
      ztemp=ZIP(ip4)
      atemp=Xyza3(k)
      ctemp=Xyzc3(k)
      xipa=xipa+atemp*xtemp
      xipc=xipc+ctemp*xtemp
      yipa=yipa+atemp*ytemp
      yipc=yipc+ctemp*ytemp
      zipa=zipa+atemp*ztemp
      zipc=zipc+ctemp*ztemp
      endif
      if(l.NE.1)then
      ip1=ip-1
      xtemp=XIP(ip1)
      ytemp=YIP(ip1)
      ztemp=ZIP(ip1)
      atemp=Xyza4(l)
      ctemp=Xyzc4(l)
      xipa=xipa+atemp*xtemp
      xipc=xipc+ctemp*xtemp
      yipa=yipa+atemp*ytemp
      yipc=yipc+ctemp*ytemp
      zipa=zipa+atemp*ztemp
      zipc=zipc+ctemp*ztemp
      endif
      XIPK(ind)=zero
      YIPK(ind)=zero
      ZIPK(ind)=zero
      XIPI(ind)=xipa
      YIPI(ind)=yipa
      ZIPI(ind)=zipa
      XIPK(ind)=XIPK(ind)+xipc
      YIPK(ind)=YIPK(ind)+yipc
      ZIPK(ind)=ZIPK(ind)+zipc
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
      
