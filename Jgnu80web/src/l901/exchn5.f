
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 exchn5"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "exchn5.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "exchn5.web"
      subroutine exchn5(NO,NV,ISD1,ISD2,ISC1,ISC2)
      implicit none
      integer ia,ii,ii1,ij,ik,il,ind1,ind2,ip,ips,ISC1,ISC2,ISD1,ISD2,iu
     &,leng,Mdv,mdv2,ncore,ncount
      integer nl,nleft,NO,no2,nom,nov,novtot,NV
      double precision one,V
      common/v/V(20000),Mdv
      data one/1.D0/
      
      
      
      
      
      
      
      call track('EXCHN5')
      
      no2=NO*NO
      nom=NO-1
      if(nom.LE.0)return
      
      nov=NO*NV
      mdv2=Mdv/2
      novtot=no2*nov
      
      call expijs(ISD1,ISC1,NO,nov)
      
      call mattrn(NO,NO,NO,NV,2,ISC1,ISC2,mdv2)
      call mattrn(1,NO,no2,NV,2,ISC2,ISC1,mdv2)
      call sumn(ISC1,ISC2,novtot,-one)
      
      call fileio(2,-ISC2,0,0,0)
      call fileio(1,-ISD2,0,0,0)
      
      nleft=no2
      ncore=Mdv/nov
      il=0
      iu=0
      ips=1
      ncount=-1
      do 100 ii=1,nom
      ii1=ii+1
      ip=ips
      do 50 ij=ii1,NO
      ip=ip+1
      if(ip.LT.il.OR.ip.GT.iu)then
      ncount=ncount+1
      if(ncount.GT.0)then
      leng=ind1
      call fileio(1,ISD2,leng,V,0)
      ncount=0
      endif
      
10    nl=min0(nleft,ncore)
      nleft=nleft-nl
      il=iu+1
      iu=iu+nl
      leng=nl*nov
      call fileio(2,ISC2,leng,V,0)
      
      if(ip.GT.iu)goto 10
      ind1=0
      endif
      ind2=(ip-il)*nov
      do 40 ik=1,NO
      do 20 ia=1,NV
      ind1=ind1+1
      V(ind1)=V(ind2+ia)
20    continue
      ind2=ind2+NV
40    continue
50    continue
      ips=ips+NO+1
100   continue
      
      leng=ind1
      call fileio(1,ISD2,leng,V,0)
      
      return
      
      end
C* :1 * 
      
