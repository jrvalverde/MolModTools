
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qasy"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qasy.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "qasy.web"
      function qasy(N,L)
      implicit none
      real*8 Alpha,Dfac,fac1,fac2,fe14,four,Fpi,one,Pi,Pi3haf,Pi5hf2,Piq
     &urt,prefac,Q,qasy,Rk,small,Sqpi,Sqpi2,sum
      real*8 T,tn,to,two,Twopi,xc,xkp
      integer L,N
      
      
      common/dfac/Dfac(23)
      common/qstore/Q(9,7),Alpha,Rk,T
      common/pifac/Pi,Twopi,Fpi,Pi3haf,Pi5hf2,Piqurt,Sqpi,Sqpi2
      save one,two,four,fe14
      data one/1.0D0/,two/2.0D0/,four/4.D00/,fe14/0.4D-14/
      
      small=1.0D-12
      xkp=Rk**(N-2)
      prefac=xkp*Sqpi2/sqrt((two*Alpha)**(2*N-1))
      sum=one
      to=one
      fac1=L-N+2
      fac2=1-L-N
      xc=one
100   tn=to*fac1*fac2/(four*xc*T)
      if(tn.NE.0)then
      sum=sum+tn
      if(abs(tn/sum).GE.small)then
      fac1=fac1+two
      fac2=fac2+two
      xc=xc+one
      to=tn
      goto 100
      endif
      endif
      qasy=prefac*sum
      
      qasy=qasy*(one+xc*fe14)
      return
      end
C* :1 * 
      
