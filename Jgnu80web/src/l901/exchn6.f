
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 exchn6"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "exchn6.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "exchn6.web"
      subroutine exchn6(NO,NV,ISD1,ISD2,IBUC1,IBUC2)
      implicit none
      integer ib,ibp,IBUC1,IBUC2,ic,iia,ind1,ind2,ISD1,ISD2,leng,Mdv,mdv
     &2,mdv21,mm,ncore,nleft,nmax,NO,nov
      integer NV,nv1,nv2,nv3
      double precision V
      common/v/V(20000),Mdv
      
      
      
      
      
      
      call track('EXCHN6')
      
      mdv2=Mdv/2
      mdv21=mdv2+1
      nv1=NV-1
      nv2=NV*NV
      nv3=NV*(NV-1)/2
      nov=NO*NV
      
      call expabs(ISD1,IBUC2,nov,NV)
      
      call mattrn(NO,NV,NV,NV,2,IBUC2,IBUC1,mdv2)
      
      call mattrn(NO,NV,NV,NV,3,IBUC1,IBUC2,mdv2)
      
      call fileio(2,-IBUC1,0,0,0)
      call fileio(2,-IBUC2,0,0,0)
      call fileio(1,-ISD2,0,0,0)
      
      nleft=NO*NV
      nmax=mdv2/nv2
100   ncore=min0(nleft,nmax)
      nleft=nleft-ncore
      
      leng=nv2*ncore
      call fileio(2,IBUC1,leng,V,0)
      call fileio(2,IBUC2,leng,V(mdv21),0)
      
      mm=0
      do 200 iia=1,ncore
      ind1=(iia-1)*nv2
      ind2=ind1+mdv2
      
      do 150 ib=1,nv1
      ibp=ib+1
      do 120 ic=ibp,NV
      mm=mm+1
      V(mm)=V(ind1+ic)-V(ind2+ic)
120   continue
      ind1=ind1+NV
      ind2=ind2+NV
150   continue
200   continue
      
      leng=nv3*ncore
      call fileio(1,ISD2,leng,V,0)
      
      if(nleft.GT.0)goto 100
      
      return
      
      end
C* :1 * 
      
