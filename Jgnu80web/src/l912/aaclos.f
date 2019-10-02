
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 aaclos"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "aaclos.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "aaclos.web"
      subroutine aaclos(IBUC1,IBUC2,NO,NV)
      implicit none
      integer i,i1,ia,iap,ib,IBUC1,IBUC2,il,ind,ind1,ind2,ind3,ind4,ip,i
     &ps,iu,j,leng,Mdv,ncore
      integer ncount,nl,nleft,NO,no2,nom,NV,nv2,nv3,nvm
      double precision V
      common/v/V(20000),Mdv
      
      
      
      
      
      
      
      call track('AACLOS')
      
      nom=NO-1
      nvm=NV-1
      if((nom.LE.0).OR.(nvm.LE.0))return
      no2=NO*NO
      nleft=no2
      nv2=NV*NV
      nv3=NV*(NV-1)/2
      ncore=Mdv/nv2
      call fileio(2,-IBUC1,0,0,0)
      call fileio(1,-IBUC2,0,0,0)
      il=0
      iu=0
      ips=1
      ncount=-1
      
      do 100 i=1,nom
      i1=i+1
      ip=ips
      do 50 j=i1,NO
      ip=ip+1
      if(ip.LT.il.OR.ip.GT.iu)then
      ncount=ncount+1
      if(ncount.GT.0)then
      leng=ind1
      call fileio(1,IBUC2,leng,V,0)
      ncount=0
      endif
10    nl=min0(nleft,ncore)
      nleft=nleft-nl
      il=iu+1
      iu=iu+nl
      leng=nl*nv2
      call fileio(2,IBUC1,leng,V,0)
      if(ip.GT.iu)goto 10
      ind1=0
      endif
      ind2=(ip-il)*nv2
      ind=ind2
      ind3=ind2
      ind4=ind2
      do 40 ia=1,nvm
      iap=ia+1
      ind=ind+NV
      ind4=ind
      do 20 ib=iap,NV
      ind1=ind1+1
      V(ind1)=V(ind3+ib)-V(ind4+ia)
      ind4=ind4+NV
20    continue
      ind3=ind3+NV
40    continue
50    continue
      ips=ips+NO+1
100   continue
      leng=ind1
      call fileio(1,IBUC2,leng,V,0)
      
      return
      
      end
C* :1 * 
      
