
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 setpot"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "setpot.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "setpot.web"
      subroutine setpot(NATOMS,IAN,C,NAE,NBE,NE,ATMCHG)
      implicit none
      double precision ATMCHG,C,Clp,coef,expon,Zlp
      integer i,I2edsc,I2esf,I5d6d,iadd,IAN,iatno,ibas,Ibasis,Ibmod,Ibpr
     &,icor,iflag,im1,In,into,Iosc,Iout,ip,ipoint
      integer Iprin,Ipt,Ipunch,ipwr,Irot,isub,j,Jprinp,Jpseud,Jpunch,Jre
     &adp,jstart,jstop,Jsubp,kf,Kfirst,kl,Klast,l,Llink
      integer Lmax,Lpskip,lskp,max,mx1,mxl,NAE,NATOMS,NBE,NE,Nfroz,Ngic,
     &Nlp,nval
      integer subval,core
      character*4 nameb1(18),pottyp(104)
      character*4 type,typelp,void
      dimension IAN(*),C(*),ATMCHG(*)
      dimension nval(40),coef(40),expon(40),kf(5),kl(5),core(35)
      dimension typelp(35),ipoint(35)
      dimension subval(6,36)
      common/ops301/Ibasis,Ngic,Ipt,I5d6d,Iosc,Ibmod,Ibpr,Llink,I2edsc,I
     &rot,Jpunch,I2esf
      common/io/In,Iout,Ipunch
      common/lp2/Nlp(400),Clp(400),Zlp(400),Kfirst(35,5),Klast(35,5),Lma
     &x(35),Lpskip(35),Nfroz(35)
      common/cprint/Iprin
      common/potpar/Jpseud,Jreadp,Jprinp,Jsubp
      data nameb1/'H-1 ','He-1','Li-1','Be-1','B-1 ','C-1 ','N-1 ','O-1 
     &','F-1 ','Ne-1','Na-1','Mg-1','Al-1','Si-1','P-1 ','S-1 ','Cl-1','
     &Ar-1'/
      data void/'    '/
      data subval/216*0/
      data pottyp/'H-4 ','He-4','Li-4','Be-4','B-4 ','C-4 ','N-4 ','O-4 
     &','F-4 ','Ne-4','Na-4','Mg-4','Al-4','Si-4','P-4 ','S-4 ','Cl-4','
     &Ar-4','K-4 ','Ca-4','Sc-4','Ti-4','V-4 ','Cr-4','Mn-4','Fe-4','Co-
     &4','Ni-4','Cu-4','Zn-4','Ga-4','Ge-4','As-4','Se-4','Br-4','Kr-4',
     &'Rb-4','Sr-4','Y-4 ','Zr-4','Nb-4','Mo-4','Tc-4','Ru-4','Rh-4','Pd
     &-4','Ag-4','Cd-4','In-4','Sn-4','Sb-4','Te-4','I-4 ','Xe-4','Cs-4'
     &,'Ba-4','La-4','Ce-4','Pr-4','Nd-4','Pm-4','Sm-4','Eu-4','Gd-4','T
     &b-4','Dy-4','Ho-4','Er-4','Tm-4','Yb-4','Lu-4','Hf-4','Ta-4','W-4 
     &','Re-4','Os-4','Ir-4','Pt-4','Au-4','Hg-4','Tl-4','Pb-4','Bi-4','
     &Po-4','At-4','Rn-4','Fr-4','Ra-4','Ac-4','Th-4','Pa-4','U-4 ','Np-
     &4','Pu-4','Am-4','Cm-4','Bk-4','Cf-4','Es-4','Fm-4','Md-4','No-4',
     &'Lr-4','Ky-4'/
      
      
      
      
      
99001 format(20I4)
99002 format(1x,'CALL TO UNAVAILABLE POTENTIAL NUMBER',i5)
99003 format(a6,4x,2I5)
      
      
      do 100 i=1,35
      typelp(i)=void
100   continue
      ibas=Jreadp
      isub=Jsubp
      if(ibas.LT.7)then
      
      
      do 150 i=1,NATOMS
      iatno=IAN(i)
      if(isub.LE.0)then
      ipoint(i)=ibas
      if((ibas.EQ.3.AND.IAN(i).LT.11).AND.(Ngic.EQ.2.OR.Ngic.EQ.3))ipoin
     &t(i)=0
      
      elseif(subval(isub,iatno).LE.0)then
      ipoint(i)=ibas
      else
      
      ipoint(i)=isub+10
      endif
150   continue
      elseif(ibas.EQ.7)then
      
      
      read(In,*)(ipoint(i),i=1,NATOMS)
      write(Iout,99001)(ipoint(i),i=1,NATOMS)
      else
      
      
      do 200 i=1,NATOMS
      ipoint(i)=ibas
200   continue
      endif
      
      
      do 300 i=1,NATOMS
      if((ipoint(i).LE.9).AND.(ipoint(i).GE.0))ipoint(i)=ipoint(i)+1
      if(ipoint(i).LT.0)ipoint(i)=10
300   continue
      iadd=1
      do 800 i=1,NATOMS
      ip=ipoint(i)
      iflag=0
      iatno=IAN(i)
      if(ip.EQ.1)type=nameb1(iatno)
      if(ip.EQ.4)type=pottyp(iatno)
      if(ip.EQ.8)read(In,99003)type,max,icor
      if(ip.LT.10)then
      if(i.EQ.1)then
      if(ip.EQ.1)goto 400
      if(ip.EQ.2)then
      elseif(ip.EQ.3)then
      elseif(ip.EQ.4)then
      goto 450
      elseif(ip.EQ.5)then
      elseif(ip.EQ.6)then
      elseif(ip.EQ.7)then
      elseif(ip.EQ.8)then
      goto 500
      else
      goto 320
      endif
      goto 550
      endif
320   im1=i-1
      do 340 j=1,im1
      if(type.EQ.typelp(j))goto 360
340   continue
      if(ip.EQ.1)goto 400
      if(ip.EQ.2)then
      elseif(ip.EQ.3)then
      elseif(ip.EQ.4)then
      goto 450
      elseif(ip.EQ.5)then
      elseif(ip.EQ.6)then
      elseif(ip.EQ.7)then
      elseif(ip.EQ.8)then
      goto 500
      else
      goto 360
      endif
      goto 550
360   lskp=Lpskip(j)
      if(lskp.EQ.0)then
      
      iflag=j
      mx1=Lmax(j)+1
      do 370 l=1,mx1
      Kfirst(i,l)=Kfirst(j,l)
      Klast(i,l)=Klast(j,l)
370   continue
      core(i)=core(j)
      Lmax(i)=Lmax(j)
      icor=core(i)
      if(ip.EQ.2)then
      elseif(ip.EQ.3)then
      elseif(ip.EQ.4)then
      goto 450
      elseif(ip.EQ.5)then
      elseif(ip.EQ.6)then
      elseif(ip.EQ.7)then
      elseif(ip.EQ.8)then
      goto 500
      else
      goto 400
      endif
      goto 550
      else
      lskp=1
      if(Iprin.EQ.1)call prnpot(IAN,C,nval,coef,expon,kf,kl,max,lskp,ico
     &r,i,iflag)
      Lpskip(i)=1
      goto 800
      endif
      elseif(ip.EQ.10)then
      Lpskip(i)=1
      goto 800
      else
      
      if(ip.EQ.1)then
      elseif(ip.EQ.2)then
      elseif(ip.EQ.3)then
      elseif(ip.EQ.4)then
      elseif(ip.EQ.5)then
      
      
      
      
      
      
      endif
      goto 550
      endif
      
400   if(iflag.EQ.0)call bas1(nval,coef,expon,kf,kl,max,lskp,type,icor,i
     &atno)
      if(Iprin.EQ.1)call prnpot(IAN,C,nval,coef,expon,kf,kl,max,lskp,ico
     &r,i,iflag)
      if(lskp.EQ.1)then
      Lpskip(i)=1
      goto 800
      else
      
      if(iflag.EQ.0)goto 600
      goto 700
      endif
      
450   if(iflag.EQ.0)call lospot(nval,coef,expon,kf,kl,max,lskp,type,icor
     &,iatno)
      if(Iprin.EQ.1)call prnpot(IAN,C,nval,coef,expon,kf,kl,max,lskp,ico
     &r,i,iflag)
      
      
      
      if(iflag.EQ.0)goto 600
      goto 700
      
500   if(iflag.EQ.0)call rdpot(nval,coef,expon,kf,kl,max,lskp,type,icor)
      if(Iprin.EQ.1)call prnpot(IAN,C,nval,coef,expon,kf,kl,max,lskp,ico
     &r,i,iflag)
      if(iflag.EQ.0)goto 600
      goto 700
550   ipwr=ip-1
      write(6,99002)ipwr
      stop
      
600   core(i)=icor
      Lpskip(i)=lskp
      if(lskp.EQ.1)goto 800
      typelp(i)=type
      Lmax(i)=max
      mx1=max+1
      do 650 l=1,mx1
      Kfirst(i,l)=iadd
      mxl=kl(l)-kf(l)+1
      Klast(i,l)=iadd+mxl-1
      iadd=iadd+mxl
      into=Kfirst(i,l)
      jstart=kf(l)
      jstop=kl(l)
      do 620 j=jstart,jstop
      Nlp(into)=nval(j)
      Clp(into)=coef(j)
      Zlp(into)=expon(j)
      into=into+1
620   continue
650   continue
700   Lpskip(i)=0
      NAE=NAE-core(i)/2
      NBE=NBE-core(i)/2
      NE=NE-core(i)
      ATMCHG(i)=ATMCHG(i)-dfloat(core(i))
800   continue
      return
      
      end
C* :1 * 
      
