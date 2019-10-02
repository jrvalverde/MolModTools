
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 exchn1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "exchn1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "exchn1.web"
      subroutine exchn1(NO,NV,IBUC1,IBUC2,IBUC3,E)
      implicit none
      double precision a0,a1,dv,E,E2,ei,eij,eija,eijab,S2,T,V
      integer i1,i2,i3,ia,ia1,iada,iadb,iadi,ib,IBUC1,IBUC2,IBUC3,ii,ii1
     &,ij,index,leng,Mdv,mdv2,mdv21
      integer NO,no1,nov2,NV,nv1,nv2
      dimension E(*)
      common/v/V(20000),Mdv
      common/result/T,E2,S2
      
      
      
      
      
      
      
      
      
      call track('EXCHN1')
      
      no1=NO-1
      nv1=NV-1
      if(no1.LE.0.OR.nv1.LE.0)return
      
      nv2=NV**2
      nov2=NO*nv2
      mdv2=Mdv/2
      mdv21=mdv2+1
      call fileio(1,-IBUC2,0,0,0)
      call fileio(1,-IBUC3,0,0,0)
      
      do 100 ii=1,no1
      iadi=(ii-1)*nov2
      ei=E(ii)
      ii1=ii+1
      do 50 ij=ii1,NO
      eij=ei+E(ij)
      index=iadi+(ij-1)*nv2
      leng=nv2
      call fileio(2,-IBUC1,leng,V,index)
      iada=0
      i3=0
      
      do 20 ia=1,nv1
      eija=E(ia+NO)-eij
      ia1=ia+1
      iadb=iada+NV
      do 10 ib=ia1,NV
      eijab=eija+E(ib+NO)
      i1=iada+ib
      i2=iadb+ia
      i3=i3+1
      dv=(V(i1))-(V(i2))
      a0=(dv)
      a1=(dv/eijab)
      E2=E2-a0*a1
      T=T+a1**2
      V(i3)=a0
      V(i3+mdv2)=-a1
      iadb=iadb+NV
10    continue
      iada=iada+NV
20    continue
      
      leng=i3
      call fileio(1,IBUC2,leng,V,0)
      call fileio(1,IBUC3,leng,V(mdv21),0)
50    continue
      
100   continue
      
      return
      
      end
C* :1 * 
      
