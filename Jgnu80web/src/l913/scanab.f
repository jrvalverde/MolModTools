
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 scanab"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "scanab.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "scanab.web"
      subroutine scanab(IBUC,NO1,NO2,NV1,NV2,A00)
      implicit none
      double precision A00,aval,gabs,Thresh,V
      integer IBUC,Iflag,In,ind,Iout,Ipunch,Ispin,leng,m1,m1d,m2,m2d,max
     &,Mdv,n1,n1d,n2,n2d,name,ncore
      integer ncount,Nd,NO1,no12,NO2,NV1,nv12,NV2
      dimension name(3)
      common/v/V(20000),Mdv
      common/io/In,Iout,Ipunch
      common/scana/Thresh,Nd,Ispin,Iflag
      data name(1)/'AAAA'/,name(2)/'ABAB'/,name(3)/'BBBB'/
      
      
      
      
      
      
      
99001 format(1x,2x,a4,10x,i2,3x,i2,3x,i2,3x,i2,6x,d13.6)
99002 format(' DOMINANT CONFIGURATIONS',/,1x,23(1H*),/,' SPIN CASE',6x,'
     &  I    J    A    B  ',8x,'VALUE     ')
      
      call track('SCANAB')
      
      call fileio(2,-IBUC,0,0,0)
      nv12=NV1*NV2
      no12=NO1*NO2
      max=Mdv/nv12
      ncore=0
      ncount=0
      
      do 100 n1=1,NO1
      do 50 n2=1,NO2
      ncount=ncount+1
      if(ncount.GT.ncore)then
      ncore=min0(max,no12)
      leng=ncore*nv12
      call fileio(2,IBUC,leng,V,0)
      no12=no12-ncore
      ind=0
      ncount=1
      endif
      do 20 m1=1,NV1
      do 10 m2=1,NV2
      ind=ind+1
      aval=V(ind)/A00
      if(gabs(aval).GE.Thresh)then
      Iflag=Iflag+1
      if(Iflag.EQ.1)write(Iout,99002)
      n1d=n1+Nd
      n2d=n2+Nd
      m1d=m1+NO1+Nd
      m2d=m2+NO2+Nd
      write(Iout,99001)name(Ispin),n1d,n2d,m1d,m2d,aval
      endif
10    continue
20    continue
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
