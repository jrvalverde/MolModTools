
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 aatild"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "aatild.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "aatild.web"
      subroutine aatild(ATILDA,C,A,S,NDIM,NBASIS)
      implicit none
      double precision A,ATILDA,C,F42,Four,gabs,Half,One,Onept5,S,Ten,Th
     &ree,thresh,Two,Zero
      integer ia,ib,ind,ind1,ind1s,ind2,ind3,ind4,mu,mu1,nasym,nb1,NBASI
     &S,NDIM,nu
      dimension ATILDA(*),C(*),A(*),S(*)
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      data thresh/1.D-8/
      
      
      
      
      
      
      
      
      call track('AATILD')
      
      if(NDIM.EQ.0)return
      nb1=NBASIS-1
      nasym=NBASIS*nb1/2
      do 100 ind=1,nasym
      ATILDA(ind)=Zero
100   continue
      
      ind4=0
      do 300 ib=1,NDIM
      do 150 mu=1,nb1
      S(mu)=Zero
150   continue
      ind1=ib
      ind2=0
      do 200 ia=1,NDIM
      if(gabs(A(ind1)).GE.thresh)then
      do 160 mu=1,nb1
      S(mu)=S(mu)+A(ind1)*C(ind2+mu)
160   continue
      endif
      ind2=ind2+NBASIS
      ind1=ind1+NDIM
200   continue
      
      ind3=-1
      ind1s=nb1
      do 250 mu=1,nb1
      mu1=mu+1
      do 220 nu=mu1,NBASIS
      ATILDA(ind3+nu)=ATILDA(ind3+nu)+S(mu)*C(ind4+nu)
220   continue
      ind1s=ind1s-1
      ind3=ind3+ind1s
250   continue
      ind4=ind4+NBASIS
300   continue
      
      return
      
      end
C* :1 * 
      
