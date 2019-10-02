
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 eigen"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "eigen.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "eigen.web"
      subroutine eigen(NN,A,VEC,EIG,W)
      implicit none
      double precision A,a2,b,beta,betasq,cosa,cosap,d,dia,dif,EIG,g,gab
     &s,gamma,gsqrt,one,p,pp,ppbr,ppbs
      double precision pt5,q,qj,r1,r12,r2,rhosq,s,sgn,shift,sina,sina2,s
     &qrts,sum,temp,two,u,VEC,W,wj
      double precision wtaw,zero
      integer i,i1,iposv,itemp,ivpos,j,jord,k,l,lv,m,n,n1,n2,NN,np,npas,
     &nr,nrr,nt
      integer nv
      dimension A(70,70),VEC(70,70),EIG(70),W(70),gamma(70),beta(70),bet
     &asq(70),p(70),q(70),iposv(70),jord(70),ivpos(70)
      equivalence(p(1),q(1),ivpos(1),beta(1)),(iposv(1),gamma(1)),(jord(
     &1),betasq(1))
      data zero/0.0D0/,pt5/0.5D0/,one/1.0D0/,two/2.0D0/
      data rhosq/1.0D-18/
      
      
      
      
      
      
      n=NN
      if(n.EQ.0)goto 900
      n1=n-1
      n2=n-2
      gamma(1)=A(1,1)
      if(n2.LT.0)goto 200
      if(n2.NE.0)then
      do 100 nr=1,n2
      b=A(nr+1,nr)
      s=zero
      do 20 i=nr,n2
      s=s+A(i+2,nr)**2
20    continue
      A(nr+1,nr)=zero
      if(s.GT.0)then
      s=s+b*b
      sgn=+one
      if(b.LT.0)sgn=-one
      sqrts=gsqrt(s)
      d=sgn/(sqrts+sqrts)
      temp=gsqrt(pt5+b*d)
      W(nr)=temp
      A(nr+1,nr)=temp
      d=d/temp
      b=-sgn*sqrts
      do 30 i=nr,n2
      temp=d*A(i+2,nr)
      W(i+1)=temp
      A(i+2,nr)=temp
30    continue
      wtaw=zero
      do 40 i=nr,n1
      sum=zero
      do 35 j=nr,i
      sum=sum+A(i+1,j+1)*W(j)
35    continue
      i1=i+1
      if(n1.GE.i1)then
      do 36 j=i1,n1
      sum=sum+A(j+1,i+1)*W(j)
36    continue
      endif
      p(i)=sum
      wtaw=wtaw+sum*W(i)
40    continue
      do 50 i=nr,n1
      q(i)=p(i)-wtaw*W(i)
50    continue
      do 60 j=nr,n1
      qj=q(j)
      wj=W(j)
      do 55 i=j,n1
      A(i+1,j+1)=A(i+1,j+1)-two*(W(i)*qj+wj*q(i))
55    continue
60    continue
      endif
      beta(nr)=b
      betasq(nr)=b*b
      gamma(nr+1)=A(nr+1,nr+1)
100   continue
      endif
      b=A(n,n-1)
      beta(n-1)=b
      betasq(n-1)=b*b
      gamma(n)=A(n,n)
200   betasq(n)=zero
      do 300 i=1,n
      do 250 j=1,n
      VEC(i,j)=zero
250   continue
      VEC(i,i)=one
300   continue
      m=n
      sum=zero
      npas=1
400   beta(m)=zero
      betasq(m)=zero
      m=m-1
      if(m.EQ.0)then
      
      EIG(1)=gamma(1)+sum
      do 450 j=1,n
      iposv(j)=j
      ivpos(j)=j
      jord(j)=j
450   continue
      m=n
500   m=m-1
      if(m.NE.0)then
      
      do 520 j=1,m
      if(EIG(j).GT.EIG(j+1))then
      temp=EIG(j)
      EIG(j)=EIG(j+1)
      EIG(j+1)=temp
      itemp=jord(j)
      jord(j)=jord(j+1)
      jord(j+1)=itemp
      endif
520   continue
      goto 500
      else
      if(n1.NE.0)then
      do 530 l=1,n1
      nv=jord(l)
      np=iposv(nv)
      if(np.NE.l)then
      lv=ivpos(l)
      ivpos(np)=lv
      iposv(lv)=np
      do 522 i=1,n
      temp=VEC(i,l)
      VEC(i,l)=VEC(i,np)
      VEC(i,np)=temp
522   continue
      endif
530   continue
      endif
      do 560 nrr=1,n
      k=n1
540   k=k-1
      if(k.GT.0)then
      sum=zero
      do 545 i=k,n1
      sum=sum+VEC(i+1,nrr)*A(i+1,k)
545   continue
      sum=sum+sum
      do 550 i=k,n1
      VEC(i+1,nrr)=VEC(i+1,nrr)-sum*A(i+1,k)
550   continue
      goto 540
      endif
      
560   continue
      goto 900
      endif
      elseif(betasq(m).LE.rhosq)then
      EIG(m+1)=gamma(m+1)+sum
      goto 400
      endif
600   a2=gamma(m+1)
      r2=pt5*a2
      r1=pt5*gamma(m)
      r12=r1+r2
      dif=r1-r2
      temp=gsqrt(dif*dif+betasq(m))
      r1=r12+temp
      r2=r12-temp
      dif=gabs(a2-r1)-gabs(a2-r2)
      if(dif.LT.0)then
      
      shift=r1
      else
      shift=r2
      endif
      
      sum=sum+shift
      cosa=one
      g=gamma(1)-shift
      pp=g
      ppbs=pp*pp+betasq(1)
      ppbr=gsqrt(ppbs)
      do 700 j=1,m
      cosap=cosa
      if(ppbs.NE.0)then
      
      sina=beta(j)/ppbr
      sina2=betasq(j)/ppbs
      cosa=pp/ppbr
      nt=j+npas
      if(nt.GE.n)nt=n
      do 620 i=1,nt
      temp=cosa*VEC(i,j)+sina*VEC(i,j+1)
      VEC(i,j+1)=-sina*VEC(i,j)+cosa*VEC(i,j+1)
      VEC(i,j)=temp
620   continue
      else
      sina=zero
      sina2=zero
      cosa=one
      endif
      dia=gamma(j+1)-shift
      u=sina2*(g+dia)
      gamma(j)=g+u
      g=dia-u
      pp=dia*cosa-sina*cosap*beta(j)
      if(j.NE.m)then
      
      ppbs=pp*pp+betasq(j+1)
      ppbr=gsqrt(ppbs)
      beta(j)=sina*ppbr
      betasq(j)=sina2*ppbs
      else
      beta(j)=sina*pp
      betasq(j)=sina2*pp*pp
      goto 800
      endif
700   continue
800   gamma(m+1)=g
      npas=npas+1
      if(betasq(m).GT.rhosq)goto 600
      EIG(m+1)=gamma(m+1)+sum
      goto 400
900   return
      
      end
C* :1 * 
      
