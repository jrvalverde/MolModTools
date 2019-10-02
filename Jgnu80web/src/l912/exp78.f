
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 exp78"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "exp78.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "exp78.web"
      subroutine exp78(NO,NV,IBUC1,IBUC2)
      implicit none
      double precision F42,Four,Half,One,Onept5,Ten,Three,Two,V,Zero
      integer i,IBUC1,IBUC2,icore,ii,ij,imj,In,ind1,ind2,indi,indj,indrd
     &,Iout,Ipunch,isave,j,kount,ktimes,leng
      integer Mdv,NO,NV,nv2,nv3
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/io/In,Iout,Ipunch
      
      
      
      
      
      
      call track('EXP78 ')
      
      if(NO.GT.0.AND.NV.GT.0)then
      call fileio(1,-IBUC2,0,0,0)
      nv2=NV*NV
      nv3=NV*(NV+1)/2
      icore=0
      kount=0
      
      do 50 ii=1,NO
      indi=(ii-1)*(2*NO-ii+2)/2
      
      do 20 ij=1,NO
      indj=(ij-1)*(2*NO-ij+2)/2
      isave=icore
      icore=icore+nv2
      if(icore.GT.Mdv)then
      
      leng=isave
      call fileio(1,IBUC2,leng,V,0)
      icore=nv2
      isave=0
      endif
      
      imj=ii-ij
      if(imj.GT.0)then
      
      indrd=(indj+ii-ij)*nv3
      leng=nv3
      call fileio(2,-IBUC1,leng,V(isave+1),indrd)
      call lsexs(V(isave+1),NV)
      
      elseif(ij.LT.NO.AND.(icore+nv2).LE.Mdv)then
      
      kount=kount+1
      else
      ktimes=kount*nv3
      indrd=(indi+ij-ii)*nv3-ktimes
      leng=nv3+ktimes
      call fileio(2,-IBUC1,leng,V(isave-ktimes+1),indrd)
      
      ind1=isave-kount*nv2
      ind2=isave-kount*nv3
      if(kount.GT.0)then
      do 4 i=1,kount
      if(nv3.GT.0)then
      do 2 j=1,nv3
      V(ind1+j)=V(ind2+j)
2     continue
      endif
      call lsexs(V(ind1+1),NV)
      ind1=ind1+nv2
      ind2=ind2+nv3
4     continue
      endif
      call lsexs(V(ind1+1),NV)
      kount=0
      endif
      
20    continue
50    continue
      
      leng=icore
      call fileio(1,IBUC2,leng,V,0)
      endif
      
      return
      
      end
C* :1 * 
      
