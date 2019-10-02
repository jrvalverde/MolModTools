
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 scanaa"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "scanaa.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "scanaa.web"
      subroutine scanaa(IBUC,NO,NV,A00)
      implicit none
      double precision A00,aval,gabs,Thresh,V
      integer IBUC,Iflag,In,ind,Iout,Ipunch,Ispin,leng,max,Mdv,name,ncor
     &e,ncount,Nd,NO,no1,no1d,no1p,no2,no2d
      integer no3,nom,NV,nv1,nv1d,nv1p,nv2,nv2d,nv3,nvm
      dimension name(3)
      common/v/V(20000),Mdv
      common/io/In,Iout,Ipunch
      common/scana/Thresh,Nd,Ispin,Iflag
      data name(1)/'AAAA'/,name(2)/'ABAB'/,name(3)/'BBBB'/
      
      
      
      
      
      
      
99001 format(1x,2x,a4,10x,i2,3x,i2,3x,i2,3x,i2,6x,d13.6)
99002 format(' DOMINANT CONFIGURATIONS',/,1x,23(1H*),/,' SPIN CASE',6x,'
     &  I    J    A    B  ',8x,'VALUE     ')
      
      call track('SCANAA')
      
      nv3=NV*(NV-1)/2
      no3=NO*(NO-1)/2
      if(no3*nv3.GT.0)then
      max=Mdv/nv3
      ncore=0
      ncount=0
      call fileio(2,-IBUC,0,0,0)
      nom=NO-1
      nvm=NV-1
      
      do 50 no1=1,nom
      no1p=no1+1
      do 20 no2=no1p,NO
      ncount=ncount+1
      if(ncount.GT.ncore)then
      ncore=min0(max,no3)
      leng=ncore*nv3
      call fileio(2,IBUC,leng,V,0)
      no3=no3-ncore
      ncount=1
      ind=0
      endif
      do 10 nv1=1,nvm
      nv1p=nv1+1
      do 5 nv2=nv1p,NV
      ind=ind+1
      aval=V(ind)/A00
      if(gabs(aval).GE.Thresh)then
      Iflag=Iflag+1
      if(Iflag.EQ.1)write(Iout,99002)
      no1d=no1+Nd
      no2d=no2+Nd
      nv1d=nv1+NO+Nd
      nv2d=nv2+NO+Nd
      write(Iout,99001)name(Ispin),no1d,no2d,nv1d,nv2d,aval
      endif
5     continue
10    continue
20    continue
50    continue
      endif
      
      return
      
      end
C* :1 * 
      
