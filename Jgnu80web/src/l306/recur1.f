
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 recur1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "recur1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "recur1.web"
      subroutine recur1(NSTART,LMAX)
      implicit none
      real*8 Alpha,Argab,Expab,four,Fpi,one,Pi,Pi3haf,Pi5hf2,Piqurt,pt5,
     &Q,qcomp,Rk,Sqpi,Sqpi2,T,talph,three,two
      real*8 Twopi,zero
      integer i,j,l,lbeg,lhi,LMAX,lmax1,lr,n,nbeg,nhi,nmax,NSTART,nzero
      
      
      common/qstore/Q(9,7),Alpha,Rk,T
      common/argab/Argab,Expab
      common/pifac/Pi,Twopi,Fpi,Pi3haf,Pi5hf2,Piqurt,Sqpi,Sqpi2
      save nzero,zero,pt5,one,two,three,four
      data nzero/0/,zero/0.0D0/,pt5/0.5D0/,one/1.0D0/,two/2.0D0/,three/3
     &.0D0/,four/4.0D0/
      
      if(Rk.NE.zero)then
      T=Rk*Rk/(four*Alpha)
      if(LMAX.LT.1)then
      Q(NSTART+1,1)=qcomp(NSTART,0)
      return
      elseif(LMAX.EQ.1)then
      Q(NSTART+1,1)=qcomp(NSTART,0)
      Q(NSTART+2,2)=qcomp(NSTART+1,1)
      return
      else
      talph=Alpha+Alpha
      if(NSTART.LT.1)then
      
      Q(3,1)=qcomp(2,0)
      lmax1=LMAX-1
      do 10 l=1,lmax1
      Q(l+3,l+1)=Rk*Q(l+2,l)/talph
10    continue
      nbeg=4
      if(T.GT.three)then
      Q(1,1)=qcomp(0,0)
      do 15 l=1,LMAX
      Q(l+1,l+1)=(talph*Q(l+2,l)-float(l+l-1)*Q(l,l))/Rk
15    continue
      if(LMAX.LE.3)return
      else
      Q(LMAX+1,LMAX+1)=qcomp(LMAX,LMAX)
      lhi=LMAX-1
      do 20 lr=nzero,lhi
      l=lhi-lr
      Q(l+1,l+1)=(talph*Q(l+3,l+1)-Rk*Q(l+2,l+2))/float(l+l+1)
20    continue
      if(LMAX.LE.3)return
      endif
      elseif(NSTART.EQ.1)then
      
      nbeg=3
      if(T.GT.three)then
      Q(2,1)=qcomp(1,0)
      Q(3,2)=qcomp(2,1)
      do 25 l=2,LMAX
      Q(l+2,l+1)=(float(l+l-2)*Q(l,l-1)+(Rk-float(l+l-1)*talph/Rk)*Q(l+1
     &,l))/talph
25    continue
      else
      Q(LMAX+2,LMAX+1)=qcomp(LMAX+1,LMAX)
      Q(LMAX+1,LMAX)=qcomp(LMAX,LMAX-1)
      lhi=LMAX-2
      do 30 lr=nzero,lhi
      l=lhi-lr
      Q(l+2,l+1)=(talph*Q(l+4,l+3)-(Rk-float(l+l+3)*talph/Rk)*Q(l+3,l+2)
     &)/float(l+l+2)
30    continue
      endif
      else
      
      Q(3,1)=qcomp(2,0)
      do 40 l=1,LMAX
      Q(l+3,l+1)=Rk*Q(l+2,l)/talph
40    continue
      nbeg=4
      endif
      nhi=LMAX+NSTART
      lbeg=0
      do 60 n=nbeg,nhi
      lhi=n-nbeg
      do 50 l=lbeg,lhi,2
      Q(n+1,l+1)=(float(n+l-1)*Q(n-1,l+1)+Rk*Q(n,l+2))/talph
50    continue
      lbeg=iand(lbeg+1,1)
60    continue
      return
      endif
      endif
      
      nmax=NSTART+LMAX
      Argab=zero
      Expab=one
      Q(1,1)=sqrt(Pi/Alpha)*pt5
      Q(2,1)=one/(two*Alpha)
      do 100 i=2,nmax
      Q(i+1,1)=(i-1)*Q(i-1,1)/(two*Alpha)
100   continue
      if(LMAX.EQ.0)return
      do 200 i=1,LMAX
      do 150 j=i,nmax
      Q(j+1,i+1)=zero
150   continue
200   continue
      
      
      return
      end
C* :1 * 
      
