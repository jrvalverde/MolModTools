
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ehbckd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ehbckd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 63 "ehbckd.web"
      subroutine ehbckd(AR,AI,TAU,N,ZR,ZI,IZ)
      implicit none
      double precision AI,alpha1,alpha2,AR,delta,gsqrt,TAU,zero,ZI,ZR
      integer inx1,inx2,IZ,j,k,k1,l,N,nr,nrm1
      dimension AR(*),AI(*),TAU(2,*),ZR(IZ,*),ZI(IZ,*)
      data zero/.0D0/
      do 100 j=1,N
      do 50 k=1,N
      ZI(j,k)=-ZR(j,k)*TAU(2,j)
      ZR(j,k)=ZR(j,k)*TAU(1,j)
50    continue
100   continue
      if(N.GT.2)then
      do 150 l=3,N
      nr=N-l+2
      nrm1=nr-1
      inx1=nr*(nrm1)/2+nr
      inx2=inx1-1
      if(AI(inx1).NE.0)then
      delta=AI(inx1)*gsqrt(AR(inx2)**2+AI(inx2)**2)
      do 120 j=1,N
      alpha1=zero
      alpha2=zero
      do 105 k=nr,N
      k1=k*(k-1)/2+nrm1
      alpha1=alpha1+AR(k1)*ZR(k,j)+AI(k1)*ZI(k,j)
      alpha2=alpha2-AI(k1)*ZR(k,j)+AR(k1)*ZI(k,j)
105   continue
      alpha1=alpha1/delta
      alpha2=alpha2/delta
      do 110 k=nr,N
      k1=k*(k-1)/2+nrm1
      ZR(k,j)=ZR(k,j)-AR(k1)*alpha1+AI(k1)*alpha2
      ZI(k,j)=ZI(k,j)-AR(k1)*alpha2-AI(k1)*alpha1
110   continue
120   continue
      endif
150   continue
      endif
      return
      
      end
C* :1 * 
      
