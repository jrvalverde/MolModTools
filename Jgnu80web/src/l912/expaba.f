
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 expaba"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "expaba.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "expaba.web"
      subroutine expaba(IBUC1,IBUC2,NTIMES,NV)
      implicit none
      integer i,IBUC1,IBUC2,ind,ind1,ind2,j,l2,l3,leng,leng2,Mdv,nocore,
     &noleft,nomax,NTIMES,NV
      double precision V
      common/v/V(20000),Mdv
      
      
      
      
      
      
      call track('EXPABA')
      
      if(NTIMES.LE.0.OR.NV.LE.0)return
      l2=NV*(NV-1)/2
      l3=NV*NV
      if(l2.GT.0)call fileio(2,-IBUC1,0,0,0)
      call fileio(1,-IBUC2,0,0,0)
      
      nomax=Mdv/l3
      noleft=NTIMES
100   nocore=min0(noleft,nomax)
      noleft=noleft-nocore
      leng=nocore*l2
      leng2=nocore*l3
      ind=leng2-leng+1
      if(l2.GT.0)call fileio(2,IBUC1,leng,V(ind),0)
      ind1=ind-1
      ind2=0
      
      do 200 i=1,nocore
      if(l2.NE.0)then
      do 120 j=1,l2
      V(ind2+j)=V(ind1+j)
120   continue
      endif
      call lsexa(V(ind2+1),NV)
      ind2=ind2+l3
      ind1=ind1+l2
200   continue
      leng=leng2
      call fileio(1,IBUC2,leng,V,0)
      if(noleft.GT.0)goto 100
      
      return
      
      end
C* :1 * 
      
