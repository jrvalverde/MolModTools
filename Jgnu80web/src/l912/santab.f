
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 santab"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "santab.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "santab.web"
      subroutine santab(IBUC1,IBUC2,NO,NV)
      implicit none
      integer i,IBUC1,IBUC2,ind,ind1,ind2,j,jp,k,leng,Mdv,mdv2,mdv21,mm,
     &ncore,nleft,nmax,NO,no3,NV
      integer nv1,nv2,nv3
      double precision V
      common/v/V(20000),Mdv
      
      
      
      
      
      
      call track('SANTAB')
      
      mdv2=Mdv/2
      mdv21=mdv2+1
      no3=NO*(NO-1)/2
      nv3=NV*(NV-1)/2
      nv2=NV*NV
      nv1=NV-1
      if(no3.LE.0.OR.nv3.LE.0)return
      call fileio(2,-IBUC1,0,0,0)
      call fileio(1,-IBUC2,0,0,0)
      
      nleft=no3
      nmax=mdv2/nv2
100   ncore=min0(nleft,nmax)
      nleft=nleft-ncore
      
      leng=nv2*ncore
      call fileio(2,IBUC1,leng,V,0)
      
      mm=mdv2
      do 200 i=1,ncore
      ind1=(i-1)*nv2
      ind=ind1
      
      do 150 j=1,nv1
      jp=j+1
      ind=ind+NV
      ind2=ind
      do 120 k=jp,NV
      mm=mm+1
      V(mm)=V(ind1+k)-V(ind2+j)
      ind2=ind2+NV
120   continue
      ind1=ind1+NV
150   continue
200   continue
      
      leng=nv3*ncore
      call fileio(1,IBUC2,leng,V(mdv21),0)
      
      if(nleft.GT.0)goto 100
      
      return
      
      end
C* :1 * 
      
