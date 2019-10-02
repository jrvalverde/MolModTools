
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 scanss"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "scanss.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "scanss.web"
      subroutine scanss(IBUC,NO,NV,A00)
      implicit none
      double precision A00,aval,gabs,Thresh,V
      integer IBUC,Iflag,In,ind,Iout,Ipunch,Ispin,leng,Mdv,n1,n1d,n2,n2d
     &,name,Nd,NO,nov,NV
      dimension name(2)
      common/v/V(20000),Mdv
      common/io/In,Iout,Ipunch
      common/scana/Thresh,Nd,Ispin,Iflag
      data name(1)/' AA '/,name(2)/' BB '/
      
      
      
      
      
      
      
99001 format(1x,2x,a4,10x,i2,8x,i2,11x,d13.6)
99002 format(' DOMINANT CONFIGURATIONS',/,1x,23(1H*),/,' SPIN CASE',6x,'
     &  I         A       ',8x,'VALUE     ')
      
      call track('SCANSS')
      
      nov=NO*NV
      leng=nov
      call fileio(2,-IBUC,leng,V,0)
      ind=0
      
      do 100 n1=1,NO
      do 50 n2=1,NV
      ind=ind+1
      aval=V(ind)/A00
      if(gabs(aval).GE.Thresh)then
      Iflag=Iflag+1
      if(Iflag.EQ.1)write(Iout,99002)
      n1d=n1+Nd
      n2d=n2+NO+Nd
      write(Iout,99001)name(Ispin),n1d,n2d,aval
      endif
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
