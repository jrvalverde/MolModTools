
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 expijs"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "expijs.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "expijs.web"
      subroutine expijs(IBUC1,IBUC2,NO,NTIMES)
      implicit none
      double precision F42,Four,Half,One,Onept5,Ten,Three,Two,V,Zero
      integer IBUC1,IBUC2,icore,ii,ij,imj,In,indi,indj,indrd,Iout,Ipunch
     &,isave,kount,ktimes,leng,Mdv,NO,NTIMES
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/io/In,Iout,Ipunch
      
      
      
      
      
      
      call track('EXPIJS')
      
      if(NO.GT.0.AND.NTIMES.GT.0)then
      call fileio(1,-IBUC2,0,0,0)
      icore=0
      kount=0
      
      do 50 ii=1,NO
      indi=(ii-1)*(2*NO-ii+2)/2
      
      do 20 ij=1,NO
      indj=(ij-1)*(2*NO-ij+2)/2
      isave=icore
      icore=icore+NTIMES
      if(icore.GT.Mdv)then
      
      leng=isave
      call fileio(1,IBUC2,leng,V,0)
      icore=NTIMES
      isave=0
      endif
      
      imj=ii-ij
      if(imj.GT.0)then
      
      indrd=(indj+ii-ij)*NTIMES
      leng=NTIMES
      call fileio(2,-IBUC1,leng,V(isave+1),indrd)
      
      elseif(ij.LT.NO.AND.(icore+NTIMES).LE.Mdv)then
      
      kount=kount+1
      else
      ktimes=kount*NTIMES
      indrd=(indi+ij-ii)*NTIMES-ktimes
      leng=NTIMES+ktimes
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
      
