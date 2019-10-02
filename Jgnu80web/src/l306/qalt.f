
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qalt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qalt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "qalt.web"
      function qalt(N,L)
      implicit none
      real*8 Alpha,Dfac,Fpi,one,Pi,Pi3haf,Pi5hf2,Piqurt,prefac,Q,qalt,Rk
     &,Sqpi,Sqpi2,sum,T,term,two,Twopi,xc
      real*8 xden,xkp
      integer L,N,num
      
      
      common/dfac/Dfac(23)
      common/qstore/Q(9,7),Alpha,Rk,T
      common/pifac/Pi,Twopi,Fpi,Pi3haf,Pi5hf2,Piqurt,Sqpi,Sqpi2
      save one,two
      data one/1.0D0/,two/2.0D0/
      
      if(L.EQ.0)xkp=one
      if(L.NE.0)xkp=Rk**L
      prefac=Sqpi2*xkp*Dfac(N+L+1)/(sqrt((two*Alpha)**(N+L+1))*Dfac(L+L+
     &3))
      num=L-N+2
      xden=L+L+3
      term=one
      sum=term
      xc=-one
100   if(num.NE.0)then
      term=term*float(num)*T/(xden*xc)
      xc=xc-one
      sum=sum+term
      num=num+2
      xden=xden+two
      goto 100
      endif
      qalt=prefac*sum
      return
      end
C* :1 * 
      
