
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getip2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getip2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "getip2.web"
      subroutine getip2(VALIP,X,CONST,CCP,CCQ)
      implicit none
      double precision CCP,CCQ,CONST,G,Rhot2,Vali2p,Vali3p,VALIP,X,Xint,
     &Zero
      integer Idmp,Idump,iv,Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lpqmax,Lqmax
      dimension VALIP(*),CCP(*),CCQ(*)
      common/int2d/G(13),Vali2p(49),Vali3p(112)
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      common/int/Zero,Xint(12)
      common/rhot2/Rhot2
      common/dump/Idmp,Idump
      
      
      G(1)=CONST
      VALIP(1)=G(1)
      if(Lpqmax.GT.1)then
      G(2)=Rhot2*X*G(1)
      if(Lpqmax.LT.2)goto 100
      if(Lpqmax.NE.2)then
      do 20 iv=3,Lpqmax
      G(iv)=Rhot2*(X*G(iv-1)-Xint(iv-2)*G(iv-2))
20    continue
      endif
      call twod2c
      
      call twod3c(CCQ)
      
      call twod4c(CCP,VALIP)
      endif
100   return
      
      end
C* :1 * 
      
