
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 drvip1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "drvip1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "drvip1.web"
      subroutine drvip1(XIP,YIP,ZIP,XIP1,YIP1,ZIP1,XIP2,YIP2,ZIP2,TWOPT2
     &,IXYZ)
      implicit none
      double precision Aiab,atemp,axcnst,aycnst,azcnst,Biab,ctemp,cxcnst
     &,cycnst,czcnst,Dabx,Daby,Dabz,Epio2,Fillip,Pcx,Pcy,Pcz,t2,TWOPT2
      double precision XIP,XIP1,xip1t,XIP2,xip2t,xyza1,xyza2,xyzc,YIP,YI
     &P1,yip1t,YIP2,yip2t,ZIP,ZIP1,zip1t,ZIP2,zip2t
      integer i,ip,ipi,IXYZ,j,k,Lamax,Lbmax,Lpmax,Maxdum
      dimension XIP(2),YIP(2),ZIP(2),XIP1(2),YIP1(2),ZIP1(2),XIP2(2),YIP
     &2(2),ZIP2(2)
      dimension xyza1(4),xyza2(4),xyzc(4)
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/ipdrv/Aiab,Biab,Epio2,Pcx,Pcy,Pcz,Dabx,Daby,Dabz,Fillip(54)
      
      
      t2=TWOPT2*Aiab
      axcnst=Dabx+Pcx*t2
      aycnst=Daby+Pcy*t2
      azcnst=Dabz+Pcz*t2
      cxcnst=-Pcx*TWOPT2
      cycnst=-Pcy*TWOPT2
      czcnst=-Pcz*TWOPT2
      xyza1(2)=-Biab-t2*Epio2
      xyza1(3)=xyza1(2)+xyza1(2)
      xyza1(4)=xyza1(3)+xyza1(2)
      xyza2(2)=Aiab-t2*Epio2
      xyza2(3)=xyza2(2)+xyza2(2)
      xyza2(4)=xyza2(3)+xyza2(2)
      xyzc(2)=TWOPT2*Epio2
      xyzc(3)=xyzc(2)+xyzc(2)
      xyzc(4)=xyzc(3)+xyzc(2)
      do 100 i=1,Lamax
      ipi=(i-1)*4+IXYZ-1
      do 50 j=1,Lbmax
      ip=ipi+j
      xip1t=cxcnst*XIP(ip)
      yip1t=cycnst*YIP(ip)
      zip1t=czcnst*ZIP(ip)
      xip2t=axcnst*XIP(ip)
      yip2t=aycnst*YIP(ip)
      zip2t=azcnst*ZIP(ip)
      if(i.NE.1)then
      k=ip-4
      atemp=xyza1(i)
      ctemp=xyzc(i)
      xip1t=xip1t+ctemp*XIP(k)
      yip1t=yip1t+ctemp*YIP(k)
      zip1t=zip1t+ctemp*ZIP(k)
      xip2t=xip2t+atemp*XIP(k)
      yip2t=yip2t+atemp*YIP(k)
      zip2t=zip2t+atemp*ZIP(k)
      endif
      if(j.NE.1)then
      k=ip-1
      atemp=xyza2(j)
      ctemp=xyzc(j)
      xip1t=xip1t+ctemp*XIP(k)
      yip1t=yip1t+ctemp*YIP(k)
      zip1t=zip1t+ctemp*ZIP(k)
      xip2t=xip2t+atemp*XIP(k)
      yip2t=yip2t+atemp*YIP(k)
      zip2t=zip2t+atemp*ZIP(k)
      endif
      XIP1(ip)=xip1t
      YIP1(ip)=yip1t
      ZIP1(ip)=zip1t
      ZIP2(ip)=zip2t
      YIP2(ip)=yip2t
      XIP2(ip)=xip2t
50    continue
100   continue
      return
      
      end
C* :1 * 
      
