
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 bprint"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "bprint.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "bprint.web"
      subroutine bprint(KOP,NPRIMS)
      implicit none
      double precision C1,C2,C3,c3a,C4,c4a,diff,Exx,exxi,Scale,scalei,th
     &r1,X,x1,x2,x3,Y,Z,zero
      integer i,ia,Ifbp,igauss,In,incr,iord,Iout,ip,Ipunch,ishell,iskip,
     &j,Jan,jp,k,KOP,LENB,maxan,MAXPRM
      integer MAXS21,MAXSH1,MAXSHL,Maxtyp,ndiff,nend,ngauss,NPRIMS,Nshel
     &l,nstart
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon,Shladf
      integer skip1,skip2,star,star2
      character*12 iatom(87),dummy,qmarks
      dimension incr(4,3)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/io/In,Iout,Ipunch
      common/scale/Scale(MAXSHL)
      common/ifbp/Ifbp
      data thr1/1.0D-8/
      data incr/4*0,0,1,4,10,2*0,4,10/
      data(iatom(k),k=1,31)/'  Hydrogen  ','  Helium    ','  Lithium   '
     &,'  Beryllium ','  Boron     ','  Carbon    ','  Nitrogen  ','  Ox
     &ygen    ','  Fluorine  ','  Neon      ','  Sodium    ','  Magnesiu
     &m ','  Aluminium ','  Silicon   ','  Phosphorus','  Sulphur   ',' 
     & Chlorine  ','  Argon     ','  Potassium ','  Calcium   ','  Scand
     &ium  ','  Titanium  ','  Vanadium  ','  Chromium  ','  Manganese '
     &,'  Iron      ','  Cobalt    ','  Nickel    ','  Copper    ','  Zi
     &nc      ','  Gallium   '/
      data(iatom(k),k=32,87)/'  Germanium ','  Arsenic   ','  Selenium  
     &','  Bromine   ','  Krypton   ','  Rubidium  ','  Strontium ','  Y
     &ttrium   ','  Zirconium ','  Niobium   ','  Molybdenum','  Technic
     &ium','  Ruthenium ','  Rhodium   ','  Palladium ','  Silver    ','
     &  Cadmium   ','  Indium    ','  Tin       ','  Antimony  ','  Tell
     &urium ','  Iodine    ','  Xenon     ','  Cesium    ','  Barium    
     &','  Lanthanum ',14*' Lanthanide ','  Hafnium   ','  Tantalum  ','
     &  Tungsten  ','  Rhenium   ','  Osmium    ','  Iridium   ','  Plat
     &inum  ','  Gold      ','  Mercury   ','  Thallium  ','  Lead      
     &','  Bismuth   ','  Polonium  ','  Astatine  ','  Radon     ','  B
     &ANQUO    '/
      data maxan/86/
      data qmarks/'????????????'/
      data zero/0.0D0/
      
      
      
      
      
      
99001 format(' *',76('-'),'*')
99002 format(' *',a12,3F9.5,38x,'*')
99003 format(2A1,45x,i3,3x,a6,5x,f5.2,62x,a1)
99004 format(2A1,41x,i3,'-',i3,3x,a6,5x,f5.2,62x,a1)
99005 format(2A1,69x,5D12.6,a1)
99006 format(1x,77('*'))
99007 format(2H *,13x,13HATOMIC CENTER,13x,1H*,8x,14HATOMIC ORBITAL,7x,1
     &H*,21x,18HGAUSSIAN FUNCTIONS,20x,1H*)
99008 format(2H *,39x,1H*,1x,8HFUNCTION,4x,5HSHELL,4x,5HSCALE,2x,1H*,59x
     &,1H*)
99009 format(2H *,4x,4HATOM,3x,7HX-COORD,2x,7HY-COORD,2x,7HZ-COORD,3x,1H
     &*,2x,6HNUMBER,5x,4HTYPE,5x,6HFACTOR,1x,1H*,2x,8HEXPONENT,4x,6HS-CO
     &EF,6x,6HP-COEF,6x,6HD-COEF,6x,6HF-COEF,3x,1H*)
99010 format(2H *,129x,1H*)
99011 format(' THERE ARE',i4,' PRIMITIVE GAUSSIANS.',/)
      
      if((KOP.EQ.2).OR.(KOP.EQ.0.AND.Ifbp.EQ.0))then
      
      NPRIMS=0
      do 50 ishell=1,Nshell
      i=Shellt(ishell)+1
      j=Shellc(ishell)+1
      ip=Shellt(ishell+1)+1
      jp=Shellc(ishell+1)+1
      NPRIMS=NPRIMS+((Aos(ishell+1)-1+incr(ip,jp))-(Aos(ishell)+incr(i,j
     &))+1)*Shelln(ishell)
50    continue
      else
      write(Iout,99006)
      write(Iout,99007)
      write(Iout,99006)
      write(Iout,99008)
      write(Iout,99009)
      write(Iout,99006)
      
      NPRIMS=0
      
      do 100 ishell=1,Nshell
      
      if(ishell.NE.1)then
      
      diff=dabs(x1-X(ishell))+dabs(x2-Y(ishell))+dabs(x3-Z(ishell))
      if(diff.LE.thr1)goto 60
      endif
      
      ia=Jan(ishell)
      if(ia.EQ.0)ia=maxan+1
      x1=X(ishell)
      x2=Y(ishell)
      x3=Z(ishell)
      if(ishell.GT.1)write(Iout,99001)
      dummy=qmarks
      if(ia.LE.(maxan+1))dummy=iatom(ia)
      write(Iout,99002)dummy,x1,x2,x3
      skip1=iord('+')
      star=iord('*')
      iskip=0
      
60    i=Shellt(ishell)+1
      j=Shellc(ishell)+1
      ip=Shellt(ishell+1)+1
      jp=Shellc(ishell+1)+1
      
      nstart=Aos(ishell)+incr(i,j)
      nend=Aos(ishell+1)-1+incr(ip,jp)
      ndiff=nend-nstart+1
      scalei=Scale(ishell)
      if(nstart.NE.nend)then
      
      write(Iout,99004)skip1,star,nstart,nend,Aon(ishell),scalei,star
      else
      write(Iout,99003)skip1,star,nstart,Aon(ishell),scalei,star
      endif
      skip1=iord(' ')
      star=iord('*')
      iskip=1
      
      ngauss=Shelln(ishell)
      i=Shella(ishell)-1
      j=Shladf(ishell)
      do 80 igauss=1,ngauss
      skip2=iord(' ')
      star2=iord('*')
      if(igauss.EQ.1)then
      skip2=iord('+')
      star2=iord(' ')
      endif
      exxi=Exx(igauss+i)/(scalei**2)
      c3a=zero
      c4a=zero
      if(j.GT.0)then
      c3a=C3(j)
      c4a=C4(j)
      j=j+1
      endif
      write(Iout,99005)skip2,star2,exxi,C1(igauss+i),C2(igauss+i),c3a,c4
     &a,star2
80    continue
      
      NPRIMS=NPRIMS+ndiff*ngauss
100   continue
      write(Iout,99006)
      endif
      
      return
      
      end
C* :1 * 
      
