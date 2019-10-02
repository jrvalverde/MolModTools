
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 expijw"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "expijw.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "expijw.web"
      subroutine expijw(IBUC1,IBUC2,NDIM)
      implicit none
      double precision a0,F42,Four,Half,One,Onept5,Ten,Three,Two,V,Zero
      integer ia,ia1,ib,IBUC1,IBUC2,icore,Ieval,ii,ij,imj,In,ind1,ind2,i
     &ndi,indj,indrd,Ioab,Iout,Ipunch,isave
      integer Ispect,kount,ktimes,leng,Loab,Lspect,Maxbuc,Mdv,NDIM,no,No
     &a,Noa2,Noa3,Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobva
      integer Nobvb,Novaa,Novab,Novbb,Nrorb,ntimes,Nva,Nva2,Nva3,Nvavb,N
     &vb,Nvb2,Nvb3
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/io/In,Iout,Ipunch
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      
      
      
      
      
      
      call track('EXPIJW')
      
      no=Noa
      ntimes=NDIM*NDIM
      if(no.GT.0.AND.ntimes.GT.0)then
      call fileio(1,-IBUC2,0,0,0)
      icore=0
      kount=0
      
      do 50 ii=1,no
      indi=(ii-1)*(2*no-ii+2)/2
      
      do 20 ij=1,no
      indj=(ij-1)*(2*no-ij+2)/2
      isave=icore
      icore=icore+ntimes
      if(icore.GT.Mdv)then
      
      leng=isave
      call fileio(1,IBUC2,leng,V,0)
      icore=ntimes
      isave=0
      endif
      
      imj=ii-ij
      if(imj.GT.0)then
      
      indrd=(indj+ii-ij)*ntimes
      leng=ntimes
      call fileio(2,-IBUC1,leng,V(isave+1),indrd)
      
      do 5 ia=2,NDIM
      ia1=ia-1
      do 2 ib=1,ia1
      ind1=isave+(ia-1)*NDIM+ib
      ind2=isave+(ib-1)*NDIM+ia
      a0=V(ind1)
      V(ind1)=V(ind2)
      V(ind2)=a0
2     continue
5     continue
      
      elseif(ij.LT.no.AND.(icore+ntimes).LE.Mdv)then
      
      kount=kount+1
      else
      ktimes=kount*ntimes
      indrd=(indi+ij-ii)*ntimes-ktimes
      leng=ntimes+ktimes
      call fileio(2,-IBUC1,leng,V(isave-ktimes+1),indrd)
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
      
