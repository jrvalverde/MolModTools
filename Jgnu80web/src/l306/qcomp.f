
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qcomp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qcomp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "qcomp.web"
      function qcomp(N,L)
      implicit none
      real*8 Alpha,Argab,Dfac,Expab,f15,Q,qalt,qasy,qcomp,qpow,Rk,T,tmin
      integer L,N
      
      
      common/dfac/Dfac(23)
      common/qstore/Q(9,7),Alpha,Rk,T
      common/argab/Argab,Expab
      dimension tmin(9)
      save tmin,f15
      data tmin/31.0D0,28.0D0,25.0D0,23.0D0,22.0D0,20.0D0,19.0D0,18.0D0,
     &15.0D0/,f15/15.0D0/
      
      Argab=-T
      Expab=exp(Argab)
      if(iand(N+L,1).EQ.0)then
      if(N.GT.L)then
      qcomp=qalt(N,L)
      return
      endif
      endif
      if(N.LT.9)then
      if(T.LT.tmin(N+1))goto 100
      elseif(T.LT.f15)then
      goto 100
      endif
      qcomp=qasy(N,L)
      return
100   qcomp=qpow(N,L)
      return
      end
C* :1 * 
      
