
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sumant"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sumant.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "sumant.web"
      subroutine sumant(IBUC1,IBUC2,NO,NV)
      implicit none
      integer ia,ia1,ia2,ia3,iap,ib,ib1,IBUC1,IBUC2,ii,ii1,ii2,iip,ij,ij
     &1,ij2,incri,ind,index,leng
      integer Mdv,mdv1,mdv11,mdv2,mdv21,NO,no1,NV,nv1,nv2,nv2no,nv3
      double precision V
      common/v/V(20000),Mdv
      
      
      
      
      
      
      
      call track('SUMANT')
      
      no1=NO-1
      nv1=NV-1
      if(no1.LE.0.OR.nv1.LE.0)return
      
      mdv1=Mdv/3
      mdv11=mdv1+1
      mdv2=mdv1*2
      mdv21=mdv2+1
      nv2=NV**2
      nv2no=nv2*NO
      incri=nv2no+nv2
      nv3=NV*nv1/2
      
      call fileio(1,-IBUC2,0,0,0)
      ii1=nv2
      ii2=nv2no
      do 100 ii=1,no1
      iip=ii+1
      ij1=ii1
      ij2=ii2
      do 50 ij=iip,NO
      leng=nv2
      index=ij1
      call fileio(2,-IBUC1,leng,V,index)
      index=ij2
      call fileio(2,-IBUC1,leng,V(mdv11),index)
      ind=mdv2
      
      ia1=0
      do 20 ia=1,nv1
      iap=ia+1
      ia2=ia1+mdv1
      ia3=ia+mdv1
      ib1=ia1+NV
      do 10 ib=iap,NV
      ind=ind+1
      V(ind)=V(ia1+ib)-V(ib1+ia)-V(ia2+ib)+V(ib1+ia3)
      ib1=ib1+NV
10    continue
      ia1=ia1+NV
20    continue
      
      leng=nv3
      call fileio(1,IBUC2,leng,V(mdv21),0)
      
      ij1=ij1+nv2
      ij2=ij2+nv2no
50    continue
      ii1=ii1+incri
      ii2=ii2+incri
100   continue
      
      return
      
      end
C* :1 * 
      
