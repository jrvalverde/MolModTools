
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 bess"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "bess.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "bess.web"
      function bess(Z,L)
      implicit none
      real*8 bess,Dfac,f1,f16pt1,f2,f5,Fac,fivm14,Fprod,rm,rp,term,tzm,t
     &zp,Z,zero,zp
      integer j,k,k1,L,l1
      
      
      common/dfac/Dfac(23)
      common/fact/Fac(13),Fprod(7,7)
      save zero,f1,f2,f5,f16pt1,fivm14
      data zero/0.0D0/,f1/1.0D0/,f2/2.0D0/,f5/5.0D0/,f16pt1/16.1D0/
      data fivm14/5.0D-14/
      
      if(Z.GT.f5)then
      if(Z.GT.f16pt1)then
      rm=zero
      tzm=-f2*Z
      l1=L+1
      do 20 k1=1,l1
      k=k1-1
      rm=rm+Fprod(k1,l1)/tzm**k
20    continue
      bess=rm/(-tzm)
      else
      rp=zero
      rm=zero
      tzp=f2*Z
      tzm=-tzp
      l1=L+1
      do 40 k1=1,l1
      k=k1-1
      rp=rp+Fprod(k1,l1)/tzp**k
      rm=rm+Fprod(k1,l1)/tzm**k
40    continue
      bess=(rm-((-f1)**L)*rp*exp(tzm))/tzp
      endif
      elseif(Z.EQ.zero)then
      if(L.NE.0)then
      bess=zero
      else
      bess=f1
      endif
      elseif(Z.LT.zero)then
      bess=zero
      else
      zp=Z*Z/f2
      term=(Z**L)/Dfac(L+L+3)
      bess=term
      j=0
50    j=j+1
      term=term*zp/float(j*(L+L+j+j+1))
      bess=bess+term
      if(abs(term/bess).GT.fivm14)goto 50
      bess=bess*exp(-Z)
      endif
      return
      end
C* :1 * 
      
