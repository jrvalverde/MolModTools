
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 vibfrq"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "vibfrq.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 33 "vibfrq.web"
      subroutine vibfrq(NATOMS,MULTIP,IAN,C,NAT3,FFX,ATMASS,ORTHOG,VV,VE
     &COUT,TRIALV,E2,EIG,TABLE,PHYCON)
      implicit none
      double precision amass,amassi,amassj,amasst,ATMASS,avog,C,cmcoor,c
     &onver,cutoff,cx,cxp,cy,cyp,cz,czp,E2,EIG,factor,FFX
      double precision flag,four,gatan,gsqrt,hartre,one,ORTHOG,PHYCON,pi
     &,pmom,scmom,secmom,slight,sqrtmi,TABLE,ten18,ten5,toang,totmas,TRI
     &ALV
      double precision VECOUT,VV,x,y,zero
      integer i,iaind,IAN,iani,iatom,icoor,In,ind,indout,indvv,iofreq,Io
     &ut,Ipunch,j,jaind,jatom,jcoor,jk,jlim,k
      integer kat,kk,lind,mn,MULTIP,NAT3,nat3tt,NATOMS,nimag,ntrro,ntrro
     &p,nvib
      logical linear
      dimension IAN(*),C(*),FFX(*),PHYCON(*)
      dimension ATMASS(*),ORTHOG(NAT3,NAT3),VV(*),VECOUT(*),TRIALV(*),E2
     &(*),EIG(*)
      dimension amass(18),cmcoor(3),secmom(3,3),scmom(6)
      dimension pmom(3)
      dimension TABLE(NAT3,3)
      common/io/In,Iout,Ipunch
      data iofreq,flag/558,-999999.D0/
      data zero,one,four/0.D0,1.D0,4.D0/
      data ten5,ten18/1.D05,1.D18/
      data cutoff/1.D-12/
      data amass(1)/1.00782522D0/
      data amass(3)/7.016005D0/
      data amass(4)/9.012183D0/
      data amass(5)/11.0093053D0/
      data amass(6)/12.0D0/
      data amass(7)/14.0030744D0/
      data amass(8)/15.9949150D0/
      data amass(9)/18.998405D0/
      data amass(10)/19.9924405D0/
      data amass(11)/22.989773D0/
      data amass(12)/23.985045D0/
      data amass(13)/26.981535D0/
      data amass(14)/27.976927D0/
      data amass(15)/30.973763D0/
      data amass(16)/31.972074D0/
      data amass(17)/34.968854D0/
      data amass(18)/39.962384D0/
      
      
      
      
      
      
      
      
      
      
      
99001 format(1x,'PRINCIPAL SECOND MOMENTS OF INERTIA (NUCLEI ONLY) ','IN
     & ATOMIC UNITS')
99002 format(1x,3F12.4)
99003 format(' ****** ',i2,' IMAGINARY FREQUENCIES (NEGATIVE SIGNS)',' *
     &*****')
      
      
      lind(i,j)=(i*(i-1))/2+j
      
      
      avog=PHYCON(5)
      slight=PHYCON(9)
      hartre=PHYCON(8)
      toang=PHYCON(1)
      
      
      pi=four*gatan(one)
      factor=(ten5/(four*pi*pi))*(avog/slight/slight)
      conver=(ten18*hartre)/(toang*toang)
      linear=.FALSE.
      
      
      nat3tt=(NAT3*(NAT3+1))/2
      call ascale(nat3tt,conver,FFX,FFX)
      
      
      totmas=zero
      do 100 i=1,3
      cmcoor(i)=zero
      do 50 j=1,3
      secmom(i,j)=zero
50    continue
100   continue
      do 200 i=1,NAT3
      do 150 j=1,NAT3
      ORTHOG(i,j)=zero
150   continue
200   continue
      
      do 300 i=1,NATOMS
      iani=IAN(i)
      ATMASS(i)=amass(iani)
300   continue
      ind=0
      do 400 i=1,NAT3
      iatom=(i-1)/3+1
      iaind=3*(iatom-1)
      amassi=ATMASS(iatom)
      icoor=i-3*((i-1)/3)
      if(icoor.EQ.1)totmas=totmas+amassi
      cmcoor(icoor)=cmcoor(icoor)+C(icoor+iaind)*amassi
      do 350 j=1,NAT3
      jatom=(j-1)/3+1
      jaind=3*(jatom-1)
      amassj=ATMASS(jatom)
      jcoor=j-3*((j-1)/3)
      ind=lind(i,j)
      amasst=amassi*amassj
      if(j.LE.i)FFX(ind)=FFX(ind)/gsqrt(amasst)
      if(iatom.EQ.jatom)secmom(icoor,jcoor)=secmom(icoor,jcoor)+amassi*C
     &(icoor+iaind)*C(jcoor+jaind)
350   continue
400   continue
      do 500 i=1,3
      cmcoor(i)=cmcoor(i)/totmas
500   continue
      
      
      do 600 icoor=1,3
      do 550 jcoor=1,3
      secmom(icoor,jcoor)=secmom(icoor,jcoor)-totmas*cmcoor(icoor)*cmcoo
     &r(jcoor)
550   continue
600   continue
      ind=0
      do 700 i=1,3
      do 650 j=1,i
      ind=ind+1
      scmom(ind)=secmom(i,j)
650   continue
700   continue
      
      
      call diagd(scmom,VV,pmom,3,TRIALV,E2,3,.FALSE.)
      write(Iout,99001)
      write(Iout,99002)(pmom(i),i=1,3)
      x=pmom(1)*pmom(2)+pmom(2)*pmom(3)+pmom(3)*pmom(1)
      if(x.LT.cutoff)linear=.TRUE.
      
      
      ntrro=6
      if(linear)ntrro=5
      do 800 i=1,NATOMS
      iaind=3*(i-1)
      amassi=ATMASS(i)
      sqrtmi=gsqrt(amassi)
      cx=C(1+iaind)-cmcoor(1)
      cy=C(2+iaind)-cmcoor(2)
      cz=C(3+iaind)-cmcoor(3)
      cxp=cx*VV(1)+cy*VV(2)+cz*VV(3)
      cyp=cx*VV(4)+cy*VV(5)+cz*VV(6)
      czp=cx*VV(7)+cy*VV(8)+cz*VV(9)
      k=3*(i-1)
      ORTHOG(k+1,1)=sqrtmi
      ORTHOG(k+2,2)=sqrtmi
      ORTHOG(k+3,3)=sqrtmi
      ORTHOG(k+1,4)=(cyp*VV(7)-czp*VV(4))*sqrtmi
      ORTHOG(k+2,4)=(cyp*VV(8)-czp*VV(5))*sqrtmi
      ORTHOG(k+3,4)=(cyp*VV(9)-czp*VV(6))*sqrtmi
      ORTHOG(k+1,5)=(czp*VV(1)-cxp*VV(7))*sqrtmi
      ORTHOG(k+2,5)=(czp*VV(2)-cxp*VV(8))*sqrtmi
      ORTHOG(k+3,5)=(czp*VV(3)-cxp*VV(9))*sqrtmi
      ORTHOG(k+1,6)=(cxp*VV(4)-cyp*VV(1))*sqrtmi
      ORTHOG(k+2,6)=(cxp*VV(5)-cyp*VV(2))*sqrtmi
      ORTHOG(k+3,6)=(cxp*VV(6)-cyp*VV(3))*sqrtmi
800   continue
      do 900 i=1,6
      x=zero
      do 850 j=1,NAT3
      x=x+ORTHOG(j,i)**2
850   continue
      if(i.LT.6.AND.x.LT.cutoff)then
      
      do 860 j=1,NAT3
      ORTHOG(j,i)=ORTHOG(j,6)
      ORTHOG(j,6)=zero
860   continue
      endif
      if(i.LE.ntrro)then
      x=one/gsqrt(x)
      do 880 j=1,NAT3
      ORTHOG(j,i)=x*ORTHOG(j,i)
880   continue
      endif
900   continue
      
      
      ntrrop=ntrro+1
      nvib=NAT3-ntrro
      
      ind=ntrro
      do 1100 i=1,NAT3
      do 950 j=1,NAT3
      TRIALV(j)=zero
950   continue
      TRIALV(i)=one
      do 1000 k=1,ind
      x=zero
      do 960 j=1,NAT3
      x=x+ORTHOG(j,k)*TRIALV(j)
960   continue
      do 980 j=1,NAT3
      TRIALV(j)=TRIALV(j)-x*ORTHOG(j,k)
980   continue
1000  continue
      x=zero
      do 1050 j=1,NAT3
      x=x+TRIALV(j)**2
1050  continue
      if(x.GE.cutoff)then
      ind=ind+1
      x=one/gsqrt(x)
      do 1060 j=1,NAT3
      ORTHOG(j,ind)=x*TRIALV(j)
1060  continue
      if(ind.EQ.NAT3)goto 1200
      endif
1100  continue
      
      
1200  indvv=0
      do 1400 j=1,NAT3
      do 1250 i=1,j
      ind=lind(j,i)
      TRIALV(i)=FFX(ind)
1250  continue
      do 1300 i=j,NAT3
      ind=lind(i,j)
      TRIALV(i)=FFX(ind)
1300  continue
      do 1350 i=ntrrop,NAT3
      x=zero
      do 1320 k=1,NAT3
      x=x+ORTHOG(k,i)*TRIALV(k)
1320  continue
      indvv=indvv+1
      VV(indvv)=x
1350  continue
1400  continue
      ind=0
      do 1500 i=ntrrop,NAT3
      jlim=i-ntrrop+1
      do 1450 j=1,jlim
      x=0
      do 1420 k=1,NAT3
      jk=j+nvib*(k-1)
      x=x+ORTHOG(k,i)*VV(jk)
1420  continue
      ind=ind+1
      FFX(ind)=x
1450  continue
1500  continue
      
      
      call diagd(FFX,VV,EIG,nvib,TRIALV,E2,nvib,.FALSE.)
      
      
      
      nimag=0
      do 1600 i=1,nvib
      x=EIG(i)*factor
      if(x.LT.zero)then
      
      nimag=nimag+1
      EIG(i)=-gsqrt(-x)
      else
      EIG(i)=gsqrt(x)
      endif
1600  continue
      
      if(nimag.GT.0)write(Iout,99003)nimag
      
      do 1700 i=1,NATOMS
      ATMASS(i)=one/gsqrt(ATMASS(i))
1700  continue
      
      ind=0
      indout=0
      do 1900 i=1,nvib
      y=zero
      do 1750 k=1,NAT3
      x=zero
      do 1720 j=1,nvib
      x=x+ORTHOG(k,ntrro+j)*VV(ind+j)
1720  continue
      kat=(k-1)/3+1
      x=x*ATMASS(kat)
      y=y+x**2
      TRIALV(k)=x
1750  continue
      y=one/gsqrt(y)
      do 1800 k=1,NAT3
      VECOUT(indout+k)=y*TRIALV(k)
      x=VECOUT(indout+k)
      kk=indout+k
1800  continue
      indout=indout+NAT3
      ind=ind+nvib
1900  continue
      
      do 2000 i=1,NATOMS
      ATMASS(i)=one/ATMASS(i)**2
2000  continue
      call vibsym(VECOUT,NAT3,nvib,TRIALV,TABLE,VV,E2,ATMASS)
      
      mn=nvib*NAT3
      call norout(VECOUT,EIG,mn,nvib,IAN,TRIALV)
      
      
      EIG(nvib+1)=flag
      call twrite(iofreq,EIG,60,1,60,1,0)
      do 2100 i=1,nvib
      EIG(i)=EIG(i)*slight
2100  continue
      
      
      call thermo(NATOMS,IAN,C,MULTIP,amass,EIG(nimag+1),nimag,PHYCON)
      
      
      return
      
      end
C* :1 * 
      
