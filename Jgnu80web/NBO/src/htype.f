
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 htype"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "htype.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "htype.web"
      subroutine htype(HYB,LTYP,MXAO,NH,COEF,PCT,NL,ISGN)
      implicit none
      double precision COEF,hundrd,HYB,PCT,thresh,zero
      integer i,ISGN,l1,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfn
     &nab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa
      integer Lfnpr,LTYP,MXAO,NH,NL
      dimension HYB(MXAO),LTYP(MXAO),PCT(5)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      
      data zero,thresh,hundrd/0.0D0,1.D-4,100.0D0/
      
      NL=0
      
      
      do 100 l1=1,5
      PCT(l1)=zero
100   continue
      COEF=zero
      
      
      do 200 i=1,NH
      l1=LTYP(i)+1
      if(l1.GT.5)goto 600
      PCT(l1)=PCT(l1)+HYB(i)**2
      COEF=COEF+HYB(i)**2
200   continue
      if(dabs(COEF).LT.thresh)return
      
      
      do 300 l1=1,5
      PCT(l1)=PCT(l1)/COEF*hundrd
300   continue
      COEF=dsqrt(COEF)
      
      
      if(ISGN.LT.0)COEF=-COEF
      
      
      do 400 i=1,NH
      HYB(i)=HYB(i)/COEF
400   continue
      
      
      do 500 i=1,NH
      if(dabs(HYB(i)).GE.thresh)then
      if(LTYP(i).GT.NL)NL=LTYP(i)
      endif
500   continue
      NL=NL+1
      return
      
600   write(Lfnpr,99001)l1-1
      stop
      
99001 format(/1x,'AO with unknown angular symmetry, l = ',i3)
      end
C* :1 * 
      
