
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpow"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpow.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "qpow.web"
      function qpow(N,L)
      implicit none
      real*8 Alpha,Argab,cutoff,Dfac,Expab,f4d14,Fpi,one,Pi,Pi3haf,Pi5hf
     &2,Piqurt,prefac,Q,qpow,Rk,Sqpi,Sqpi2,sum,T
      real*8 term,two,Twopi,xden,xj,xkp,xnum,zero
      integer L,N
      
      common/argab/Argab,Expab
      common/dfac/Dfac(23)
      common/qstore/Q(9,7),Alpha,Rk,T
      common/pifac/Pi,Twopi,Fpi,Pi3haf,Pi5hf2,Piqurt,Sqpi,Sqpi2
      save zero,one,two,cutoff,f4d14
      data zero/0.0D0/,one/1.0D0/,two/2.0D0/,cutoff/1.0D-14/,f4d14/0.4D-
     &14/
      
      if(L.EQ.0)xkp=one
      if(L.NE.0)xkp=Rk**L
      prefac=Expab*xkp/sqrt((two*Alpha)**(N+L+1))
      xnum=L+N-1
      xden=L+L+1
      term=Dfac(L+N+1)/Dfac(L+L+3)
      sum=term
      xj=zero
100   xnum=xnum+two
      xden=xden+two
      xj=xj+one
      term=term*T*xnum/(xj*xden)
      sum=sum+term
      if((term/sum).GT.cutoff)goto 100
      qpow=prefac*sum
      if(mod((L+N),2).EQ.0)qpow=qpow*Sqpi2
      qpow=qpow*(one+xj*f4d14)
      return
      end
C* :1 * 
      
