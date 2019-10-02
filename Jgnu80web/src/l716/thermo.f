
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 thermo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "thermo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "thermo.web"
      subroutine thermo(NATOMS,IAN,C,MULTIP,AMASS,FREQ,NIMAG,PHYCON)
      implicit none
      double precision akilo,AMASS,arg,avog,boltz,C,ccont,con,crot,ctot,
     &ctran,cv,cvib,cvibn,dum,dum1,dum2,e,econt,eight
      double precision em1,erot,esum,etot,etovt,etran,evib,evibn,ezau,ez
     &j,ezkc,ezpe,four,FREQ,gas,gatan,Gen,gexp,glog,gsqrt
      double precision half,hartre,one,onept5,p,patm,PHYCON,pi,pipi,plan
     &ck,pmom,pstd,pt2,rt,rtemp,rtemp1,rtemp2,rtemp3,s,scont
      double precision sn,srot,stot,stran,svib,svibn,symnum,t,thresh,toc
     &al,tokcal,tokg,tomet,tovt,tstd,two,twopt5,vtemp,weight,zero
      integer i,ia,IAN,iat,In,Iout,Ipunch,itop,lofreq,MULTIP,NATOMS,ndof
     &,NIMAG
      logical linear
      double precision jpcal
      dimension IAN(*),AMASS(*),FREQ(*),C(*),PHYCON(30)
      dimension vtemp(300),evibn(20),cvibn(20),svibn(20),pmom(3)
      common/io/In,Iout,Ipunch
      common/gen/Gen(47)
      data tstd/298.15D00/
      data pstd/1.01325D+05/
      data zero,pt2,half,one,onept5/0.0D0,0.2D0,0.5D0,1.0D0,1.5D0/
      data two,twopt5,four,eight,akilo/2.0D0,2.5D0,4.0D0,8.0D0,1000.D0/
      data thresh/900.D0/
      
      
      
      
      
      
      
      
      
      
      
99001 format(1x,19('-')/1x,'- THERMOCHEMISTRY -'/1x,19('-'))
99002 format(1x,'MOLECULAR MASS (PRINCIPAL ISOTOPES) ',f11.5,' AMU')
99003 format(1x,'TEMPERATURE ',f9.3,' KELVIN'/1x,'PRESSURE    ',f9.5,' A
     &TM')
99004 format(1x,'WARNING-- ASSUMPTIONS MADE ABOUT THE ELECTRONIC ','PART
     &ITION FUNCTION'/1x,'          ARE NOT VALID FOR MULTIPLETS!')
99005 format(1x,'INTERNAL ENERGY:   ',f10.3,' JOULE/MOL',9x,f10.3,' KCAL
     &/MOL'/1x,'ENTROPY:           ',f10.3,' JOULE/K-MOL',7x,f10.3,' CAL
     &/K-MOL'/1x,'HEAT CAPACITY CV:  ',f10.3,' JOULE/K-MOL',7x,f10.3,' C
     &AL/K-MOL')
99006 format(1x,'ROTATIONAL SYMMETRY NUMBER ',f3.0)
99007 format(1x,'WARNING-- ASSUMPTION OF CLASSICAL BEHAVIOR FOR ','ROTAT
     &ION'/1x,'          MAY CAUSE SIGNIFICANT ERROR')
99008 format(1x,'ROTATIONAL TEMPERATURES (KELVIN) ',3F12.5)
99009 format(1x,'ROTATIONAL TEMPERATURE (KELVIN) ',f12.5)
99010 format(1x,'ZERO POINT VIBRATIONAL ENERGY ',f12.1,' (JOULES/MOL) ',
     &/1x,30x,f12.5,' (KCAL/MOL)'/1x,30x,f12.7,' (HARTREE/PARTICLE)')
99011 format(1x,'WARNING-- EXPLICIT CONSIDERATION OF ',i3,' DEGREES ''OF
     & FREEDOM AS '/1x,'          VIBRATIONS MAY CAUSE SIGNIFICANT ERROR
     &')
99012 format(1x,'VIBRATIONAL TEMPERATURES: ',5F9.2)
99013 format(1x,9x,'(KELVIN)',9x,5F9.2)
99014 format(1x,26x,5F9.2)
99015 format(1x,15x,9x,'E',9x,9x,'CV',8x,9x,'S',9x)
99016 format(1x,15x,5x,'JOULES/MOL',4x,1x,'JOULES/MOL-KELVIN',1x,2x,'JOU
     &LES/MOL-KELVIN')
99017 format(1x,'TOTAL',10x,3(4x,f11.3,4x))
99018 format(1x,'TRANSLATIONAL',2x,3(4x,f11.3,4x))
99019 format(1x,'ROTATIONAL',5x,3(4x,f11.3,4x))
99020 format(1x,'VIBRATIONAL',4x,3(4x,f11.3,4x))
99021 format(1x,'VIBRATION',i3,3x,3(4x,f11.3,4x))
99022 format(1x,15x,6x,'KCAL/MOL',5x,3x,'CAL/MOL-KELVIN',2x,2x,'CAL/MOL-
     &KELVIN')
99023 format(1x,i3,' IMAGINARY FREQUENCIES IGNORED')
99024 format(1x,'PRINCIPAL MOMENTS OF INERTIA (NUCLEII ONLY) IN ','ATOMI
     &C UNITS:'/1x,5x,3F12.4)
99025 format(1x,'SUM OF HARTREE-FOCK AND THERMAL ENERGIES: ',f12.7,' (HA
     &RTREE/PARTICLE)')
      
      
      
      tokg=PHYCON(2)
      boltz=PHYCON(10)
      planck=PHYCON(4)
      avog=PHYCON(5)
      jpcal=PHYCON(6)
      tomet=PHYCON(7)
      hartre=PHYCON(8)
      
      
      gas=avog*boltz
      pi=four*gatan(one)
      pipi=pi*pi
      e=gexp(one)
      tocal=one/jpcal
      tokcal=tocal/akilo
      
      
      t=tstd
      p=pstd
      patm=p/pstd
      write(Iout,99001)
      write(Iout,99003)t,patm
      rt=gas*t
      
      
      weight=zero
      do 100 iat=1,NATOMS
      ia=IAN(iat)
      weight=weight+AMASS(ia)
100   continue
      write(Iout,99002)weight
      weight=weight*tokg
      
      
      if(MULTIP.NE.1)write(Iout,99004)
      
      
      dum1=boltz*t
      dum2=(two*pi)**onept5
      arg=dum1**onept5/planck
      arg=(arg/p)*(dum1/planck)
      arg=arg*dum2*(weight/planck)
      arg=arg*gsqrt(weight)*e**twopt5
      stran=gas*glog(arg)
      etran=onept5*rt
      ctran=onept5*gas
      
      
      
      
      if(NATOMS.GT.1)then
      
      
      
      
      call mofi(NATOMS,IAN,C,AMASS,pmom)
      write(Iout,99024)(pmom(i),i=1,3)
      sn=symnum(linear)
      write(Iout,99006)sn
      con=planck/(boltz*eight*pipi)
      con=(con/tokg)*(planck/(tomet*tomet))
      if(linear)then
      
      rtemp=con/pmom(3)
      if(rtemp.LT.pt2)write(Iout,99007)
      write(Iout,99009)rtemp
      else
      rtemp1=con/pmom(1)
      rtemp2=con/pmom(2)
      rtemp3=con/pmom(3)
      
      if(rtemp1.LT.pt2)write(Iout,99007)
      write(Iout,99008)rtemp1,rtemp2,rtemp3
      endif
      
      
      if(linear)then
      
      erot=rt
      crot=gas
      arg=(t/rtemp)*(e/sn)
      srot=gas*glog(arg)
      else
      erot=onept5*rt
      crot=onept5*gas
      arg=gsqrt(pi*e*e*e)/sn
      dum=(t/rtemp1)*(t/rtemp2)*(t/rtemp3)
      arg=arg*gsqrt(dum)
      srot=gas*glog(arg)
      endif
      
      
      
      
      ndof=3*NATOMS-6-NIMAG
      if(NIMAG.NE.0)write(Iout,99023)NIMAG
      if(linear)ndof=ndof+1
      con=planck/boltz
      ezpe=zero
      do 150 i=1,ndof
      vtemp(i)=FREQ(i)*con
      ezpe=ezpe+FREQ(i)
150   continue
      ezpe=half*planck*ezpe
      ezj=ezpe*avog
      ezkc=ezpe*tokcal*avog
      ezau=ezpe/hartre
      write(Iout,99010)ezj,ezkc,ezau
      
      
      lofreq=0
      do 200 i=1,ndof
      if(vtemp(i).LT.thresh)lofreq=lofreq+1
200   continue
      if(lofreq.NE.0)write(Iout,99011)lofreq
      
      itop=min0(ndof,5)
      write(Iout,99012)(vtemp(i),i=1,itop)
      if(ndof.LE.5)write(Iout,99013)
      itop=min0(ndof,10)
      if(ndof.GT.5)write(Iout,99013)(vtemp(i),i=6,itop)
      if(ndof.GT.10)write(Iout,99014)(vtemp(i),i=11,ndof)
      
      
      evib=zero
      cvib=zero
      svib=zero
      do 250 i=1,ndof
      
      
      tovt=vtemp(i)/t
      etovt=gexp(tovt)
      em1=etovt-one
      
      
      econt=tovt*(half+one/em1)
      ccont=etovt*(tovt/em1)**2
      scont=tovt/em1-glog(one-one/etovt)
      if(lofreq.GE.i)then
      evibn(i)=econt*rt
      cvibn(i)=ccont*gas
      svibn(i)=scont*gas
      endif
      
      evib=evib+econt
      cvib=cvib+ccont
      svib=svib+scont
250   continue
      evib=evib*rt
      cvib=cvib*gas
      svib=svib*gas
      
      
      etot=etran+erot+evib
      ctot=ctran+crot+cvib
      stot=stran+srot+svib
      
      
      call tread(501,Gen,47,1,47,1,0)
      esum=Gen(32)+etot/avog/hartre
      write(Iout,99025)esum
      
      write(Iout,99015)
      write(Iout,99016)
      write(Iout,99017)etot,ctot,stot
      write(Iout,99018)etran,ctran,stran
      write(Iout,99019)erot,crot,srot
      write(Iout,99020)evib,cvib,svib
      if(lofreq.NE.0)then
      do 260 i=1,lofreq
      write(Iout,99021)i,evibn(i),cvibn(i),svibn(i)
260   continue
      endif
      
      
      etran=etran*tokcal
      ctran=ctran*tocal
      stran=stran*tocal
      erot=erot*tokcal
      crot=crot*tocal
      srot=srot*tocal
      evib=evib*tokcal
      cvib=cvib*tocal
      svib=svib*tocal
      etot=etran+erot+evib
      ctot=ctran+crot+cvib
      stot=stran+srot+svib
      if(lofreq.NE.0)then
      do 280 i=1,lofreq
      evibn(i)=evibn(i)*tokcal
      cvibn(i)=cvibn(i)*tocal
      svibn(i)=svibn(i)*tocal
280   continue
      endif
      
      write(Iout,99015)
      write(Iout,99022)
      write(Iout,99017)etot,ctot,stot
      write(Iout,99018)etran,ctran,stran
      write(Iout,99019)erot,crot,srot
      write(Iout,99020)evib,cvib,svib
      if(lofreq.NE.0)then
      do 300 i=1,lofreq
      write(Iout,99021)i,evibn(i),cvibn(i),svibn(i)
300   continue
      endif
      else
      s=stran*tocal
      e=etran*tokcal
      cv=ctran*tocal
      write(Iout,99005)etran,e,stran,s,ctran,cv
      return
      endif
      
      return
      
      end
C* :1 * 
      
