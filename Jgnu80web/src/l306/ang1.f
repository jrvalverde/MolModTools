
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ang1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ang1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "ang1.web"
      subroutine ang1(N,L,M,XK,YK,ZK,ANG)
      implicit none
      real*8 aint,ANG,angt,Dfac,Fpi,one,Pi,Pi3haf,Pi5hf2,Piqurt,pre,Sqpi
     &,Sqpi2,Twopi,XK,xkp,YK,ykp,zero,ZK
      real*8 zkp,Zlm
      integer i,iend,indx,indy,indz,istart,L,l2,lambda,Lf,lmb,Lmf,Lml,Lm
     &x,Lmy,Lmz,loc,M,mu,mu1
      integer N,nlm,nzero
      
      
      integer iand
      common/ztabcm/Zlm(130),Lf(7),Lmf(49),Lml(49),Lmx(130),Lmy(130),Lmz
     &(130)
      common/dfac/Dfac(23)
      common/pifac/Pi,Twopi,Fpi,Pi3haf,Pi5hf2,Piqurt,Sqpi,Sqpi2
      dimension ANG(7)
      save nzero,zero,one
      data nzero/0/,zero/0.0D0/,one/1.0D0/
      
      nlm=N+L+M+1
      do 100 lambda=1,nlm,2
      lmb=nlm-lambda
      l2=lmb+lmb+1
      angt=zero
      loc=Lf(lmb+1)
      do 50 mu1=1,l2
      mu=mu1-1
      istart=Lmf(loc+mu)
      if(iand(N,1).EQ.iand(Lmx(istart),1))then
      if(iand(L,1).EQ.iand(Lmy(istart),1))then
      if(iand(M,1).EQ.iand(Lmz(istart),1))then
      pre=zero
      aint=zero
      iend=Lml(loc+mu)
      do 2 i=istart,iend
      indx=Lmx(i)
      indy=Lmy(i)
      indz=Lmz(i)
      if(indx.NE.0)then
      xkp=XK**indx
      else
      xkp=one
      endif
      if(indy.NE.0)then
      ykp=YK**indy
      else
      ykp=one
      endif
      if(indz.NE.0)then
      zkp=ZK**indz
      else
      zkp=one
      endif
      pre=pre+Zlm(i)*xkp*ykp*zkp
      aint=aint+Zlm(i)*Dfac(N+indx+1)*Dfac(L+indy+1)*Dfac(M+indz+1)/Dfac
     &(nlm-1+indx+indy+indz+3)
2     continue
      angt=angt+pre*aint*Fpi
      endif
      endif
      endif
50    continue
      ANG(lmb+1)=angt
100   continue
      return
      end
C* :1 * 
      
