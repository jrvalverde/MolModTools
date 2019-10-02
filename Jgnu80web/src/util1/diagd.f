
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 diagd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "diagd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "diagd.web"
      subroutine diagd(A,V,D,NBASIS,E,E2,MDIM,CMP)
      implicit none
      double precision A,D,E,E2,one,V,zero
      integer i,ier,j,m1,m2,MDIM,mtt,NBASIS,ntt
      logical CMP
      dimension A(*),V(MDIM,*),D(*),E(*),E2(*)
      data zero/0.0D0/,one/1.0D0/
      
      
      if(CMP)then
      
      ntt=NBASIS*(NBASIS+1)/2
      mtt=MDIM*(MDIM+1)/2
      m1=mtt+1
      m2=mtt+ntt
      do 50 i=m1,m2
      A(i)=-A(i)
50    continue
      call ehoudh(A,A(m1),NBASIS,D,E,E2)
      else
      call ehousd(A,NBASIS,D,E,E2)
      endif
      do 200 i=1,NBASIS
      do 100 j=1,NBASIS
      V(i,j)=zero
100   continue
200   continue
      do 300 i=1,NBASIS
      V(i,i)=one
300   continue
      call eqrt2d(D,E,NBASIS,V,MDIM,ier)
      if(CMP)then
      call ehbckd(A,A(m1),E2,NBASIS,V,V(1,MDIM+1),MDIM)
      return
      endif
      
      call ehobkd(A,NBASIS,1,NBASIS,V,MDIM)
      return
      
      end
C* :1 * 
      
