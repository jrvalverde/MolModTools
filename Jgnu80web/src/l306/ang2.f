
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ang2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ang2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "ang2.web"
      subroutine ang2(NA,LA,MA,L,XK,YK,ZK,ANG)
      implicit none
      real*8 aint,ANG,angt,Dfac,Fpi,one,Pi,Pi3haf,Pi5hf2,Piqurt,pre,Sqpi
     &,Sqpi2,Twopi,XK,xkp,YK,ykp,zero,ZK
      real*8 zkp,Zlm
      integer i,ia,iabc,ib,ic,iend,indx,indy,indz,istart,j,L,l2,LA,la1,l
     &amb,lambhi,lamblo,Lf,Lmf
      integer Lml,Lmx,Lmy,Lmz,loc1,loc2,m,MA,ma1,mend,mhi,mndx,mndy,mndz
     &,mstart,mu,NA,na1
      
      
      integer iand
      common/ztabcm/Zlm(130),Lf(7),Lmf(49),Lml(49),Lmx(130),Lmy(130),Lmz
     &(130)
      common/dfac/Dfac(23)
      common/pifac/Pi,Twopi,Fpi,Pi3haf,Pi5hf2,Piqurt,Sqpi,Sqpi2
      dimension ANG(8,7,7)
      save zero,one
      data zero/0.0D0/,one/1.0D0/
      
      loc1=Lf(L+1)
      mhi=L+L+1
      iabc=0
      na1=NA+1
      la1=LA+1
      ma1=MA+1
      do 100 ia=1,na1
      do 50 ib=1,la1
      do 40 ic=1,ma1
      iabc=iabc+1
      lambhi=ia+ib+ic-3+L+1
      lamblo=max0(L-ia-ib-ic+3,0)+1
      if(mod(lambhi-lamblo,2).NE.0)lamblo=lamblo+1
      do 20 m=1,mhi
      mstart=Lmf(loc1+m-1)
      mend=Lml(loc1+m-1)
      do 10 lamb=1,lambhi
      ANG(iabc,m,lamb)=zero
      if(lamb.GE.lamblo)then
      if(iand(lamb-lamblo,1).EQ.0)then
      l2=lamb+lamb-1
      angt=zero
      loc2=Lf(lamb)
      do 6 mu=1,l2
      istart=Lmf(loc2+mu-1)
      if(iand(ia-1+Lmx(mstart)+Lmx(istart),1).EQ.0)then
      if(iand(ib-1+Lmy(mstart)+Lmy(istart),1).EQ.0)then
      if(iand(ic-1+Lmz(mstart)+Lmz(istart),1).EQ.0)then
      pre=zero
      iend=Lml(loc2+mu-1)
      aint=zero
      do 4 i=istart,iend
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
      do 2 j=mstart,mend
      mndx=Lmx(j)
      mndy=Lmy(j)
      mndz=Lmz(j)
      aint=aint+Zlm(i)*Zlm(j)*Dfac(ia+indx+mndx)*Dfac(ib+indy+mndy)*Dfac
     &(ic+indz+mndz)/Dfac(ia+ib+ic+indx+mndx+indy+mndy+indz+mndz)
2     continue
4     continue
      angt=angt+pre*aint*sqrt(Fpi**3)
      endif
      endif
      endif
6     continue
      ANG(iabc,m,lamb)=angt
      endif
      endif
10    continue
20    continue
40    continue
50    continue
100   continue
      return
      end
C* :1 * 
      
