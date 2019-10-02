
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 bessv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "bessv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "bessv.web"
      subroutine bessv(B,Z,L,N)
      implicit none
      real*8 B,den,denm,denp,Dfac,f16pt1,Fact,five,Fprod,one,pt5,rm,rp,t
     &erm,texm,two,tzi,tzmi,tzpi,Z
      real*8 zero,zp
      integer i,ihi1,ihi2,ihi3,ilo1,ilo2,ilo3,j,k1,L,l1,N,n1,n2,n3
      
      
      
      
      
      
      common/dfac/Dfac(23)
      common/fact/Fact(13),Fprod(7,7)
      dimension Z(*),B(*),den(15),term(50),zp(50),denm(50,7),denp(50,7)
      dimension rp(50),rm(50),tzpi(50),tzmi(50),texm(50)
      equivalence(den(1),rp(1)),(term(1),rm(1)),(zp(1),tzpi(1))
      save zero,one,two,five,pt5,f16pt1
      data zero/0.0D0/,one/1.0D0/,two/2.0D0/,five/5.0D0/
      data pt5/0.5D0/,f16pt1/16.1D0/
      
      ilo1=0
      ilo2=0
      ilo3=0
      n1=0
      n2=0
      n3=0
      do 100 i=1,N
      if(Z(i).LT.0)then
      B(i)=zero
      elseif(Z(i).EQ.0)then
      B(i)=zero
      if(L.EQ.zero)B(i)=one
      elseif(Z(i).LE.five)then
      if(ilo1.EQ.0)ilo1=i
      n1=n1+1
      elseif(Z(i).LE.f16pt1)then
      if(ilo2.EQ.0)ilo2=i
      n2=n2+1
      else
      if(ilo3.EQ.0)ilo3=i
      n3=n3+1
      endif
100   continue
      ihi1=ilo1+n1-1
      ihi2=ilo2+n2-1
      ihi3=ilo3+n3-1
      
      if(n1.NE.0)then
      do 150 i=ilo1,ihi1
      zp(i)=Z(i)*Z(i)*pt5
      term(i)=(Z(i)**L)/Dfac(L+L+3)
      B(i)=term(i)
150   continue
      do 200 j=1,15
      den(j)=one/float(j*(L+L+j+j+1))
200   continue
      do 250 j=1,15
      do 220 i=ilo1,ihi1
      term(i)=term(i)*zp(i)*den(j)
      B(i)=B(i)+term(i)
220   continue
250   continue
      do 300 i=ilo1,ihi1
      B(i)=B(i)*exp(-Z(i))
300   continue
      endif
      
      if(n2.NE.0)then
      l1=L+1
      do 350 i=ilo2,ihi2
      rp(i)=Fprod(1,l1)
      rm(i)=Fprod(1,l1)
      tzi=two*Z(i)
      tzpi(i)=one/tzi
      tzmi(i)=-tzpi(i)
      texm(i)=exp(-tzi)
      denp(i,1)=one
      denm(i,1)=one
350   continue
      if(l1.GT.1)then
      do 380 k1=2,l1
      do 360 i=ilo2,ihi2
      denp(i,k1)=denp(i,k1-1)*tzpi(i)
      denm(i,k1)=denm(i,k1-1)*tzmi(i)
360   continue
380   continue
      do 400 k1=2,l1
      do 390 i=ilo2,ihi2
      rp(i)=rp(i)+denp(i,k1)*Fprod(k1,l1)
      rm(i)=rm(i)+denm(i,k1)*Fprod(k1,l1)
390   continue
400   continue
      endif
      do 450 i=ilo2,ihi2
      B(i)=(rm(i)-((-one)**L)*rp(i)*texm(i))*tzpi(i)
450   continue
      endif
      
      if(n3.NE.0)then
      l1=L+1
      do 500 i=ilo3,ihi3
      rm(i)=Fprod(1,l1)
      tzmi(i)=-pt5/Z(i)
      denm(i,1)=one
500   continue
      if(l1.GT.1)then
      do 520 k1=2,l1
      do 510 i=ilo3,ihi3
      denm(i,k1)=denm(i,k1-1)*tzmi(i)
510   continue
520   continue
      do 540 k1=2,l1
      do 530 i=ilo3,ihi3
      rm(i)=rm(i)+denm(i,k1)*Fprod(k1,l1)
530   continue
540   continue
      endif
      do 550 i=ilo3,ihi3
      B(i)=-rm(i)*tzmi(i)
550   continue
      endif
      
      
      return
      end
C* :1 * 
      
