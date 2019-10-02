
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ctwc1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ctwc1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "ctwc1.web"
      subroutine ctwc1(W,C,WT,NV,NPIJ,S,INITW,NBASIS)
      implicit none
      double precision a0,C,c0,F42,Four,Half,One,Onept5,S,Ten,Three,Two,
     &W,WT,Zero
      integer ia,ia1,iap,ib,ib1,ij,ija,m,mu,mup,n,nb1,nb3,NBASIS,NPIJ,nu
     &,NV,nv1,nv3
      dimension W(*),C(*),WT(*),S(*)
      logical INITW
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      
      
      
      
      
      
      call track('CTWC1 ')
      
      if(NPIJ.LE.0)return
      nv1=NV-1
      if(nv1.LE.0)return
      nv3=NV*nv1/2
      nb1=NBASIS-1
      nb3=NBASIS*nb1/2
      
      ija=0
      n=0
      do 200 ij=1,NPIJ
      ia1=0
      do 100 ia=1,nv1
      iap=ia+1
      
      do 20 nu=1,NBASIS
      S(nu)=Zero
20    continue
      m=ija
      do 40 mu=1,nb1
      mup=mu+1
      c0=C(ia1+mu)
      do 30 nu=mup,NBASIS
      m=m+1
      S(mu)=S(mu)-C(ia1+nu)*WT(m)
      S(nu)=S(nu)+c0*WT(m)
30    continue
40    continue
      ib1=ia1+NBASIS
      if(INITW)then
      
      do 50 ib=iap,NV
      a0=Zero
      do 45 nu=1,NBASIS
      a0=a0+S(nu)*C(ib1+nu)
45    continue
      n=n+1
      W(n)=W(n)+a0
      ib1=ib1+NBASIS
50    continue
      else
      
      do 60 ib=iap,NV
      a0=Zero
      do 55 nu=1,NBASIS
      a0=a0+S(nu)*C(ib1+nu)
55    continue
      n=n+1
      W(n)=a0
      ib1=ib1+NBASIS
60    continue
      endif
      
      ia1=ia1+NBASIS
100   continue
      ija=ija+nb3
200   continue
      
      return
      
      end
C* :1 * 
      
