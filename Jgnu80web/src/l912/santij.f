
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 santij"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "santij.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "santij.web"
      subroutine santij(IBUC1,IBUC2,NO,NV)
      implicit none
      integer i,iab,IBUC1,IBUC2,ii1,ii2,ij1,ij2,ind,index,ip,j,leng,Mdv,
     &mdv2,mdv21,NO,no1,no3,nonv3
      integer NV,nv3
      double precision V
      common/v/V(20000),Mdv
      
      
      
      
      
      
      call track('SANTIJ')
      
      no1=NO-1
      no3=NO*(NO-1)/2
      nv3=NV*(NV-1)/2
      if(no3*nv3.LE.0)return
      nonv3=NO*nv3
      mdv2=Mdv/2
      mdv21=mdv2+1
      leng=nv3
      ind=nonv3+nv3
      call fileio(1,-IBUC2,0,0,0)
      
      ii1=nv3
      ii2=nonv3
      do 100 i=1,no1
      ip=i+1
      ij1=ii1
      ij2=ii2
      do 50 j=ip,NO
      index=ij1
      call fileio(2,-IBUC1,leng,V,index)
      index=ij2
      call fileio(2,-IBUC1,leng,V(mdv21),index)
      do 20 iab=1,nv3
      V(iab)=V(iab)-V(mdv2+iab)
20    continue
      call fileio(1,IBUC2,leng,V,0)
      ij1=ij1+nv3
      ij2=ij2+nonv3
50    continue
      ii1=ii1+ind
      ii2=ii2+ind
100   continue
      
      return
      
      end
C* :1 * 
      
