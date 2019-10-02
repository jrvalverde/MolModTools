
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 zprint"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "zprint.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "zprint.web"
      subroutine zprint(NZ,IANZ,IZ,BL,ALPHA,BETA,CONVER)
      implicit none
      double precision ALPHA,BETA,BL,CONVER,f45,gatan,one,pa,pb,pbl,tode
     &g
      integer IANZ,icard,icent,idx,iel,In,Iout,Ipunch,IZ,np1,np2,np3,NZ
      dimension IANZ(*),BL(*),ALPHA(*),BETA(*)
      dimension IZ(50,4)
      dimension iel(106)
      common/io/In,Iout,Ipunch
      data one,f45/1.0D0,45.0D0/
      data iel/'X ','Bq','H ','He','Li','Be','B ','C ','N ','O ','F ','N
     &e','Na','Mg','Al','Si','P ','S ','Cl','Ar','K ','Ca','Sc','Ti','V 
     &','Cr','Mn','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr'
     &,'Rb','Sr','Y ','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In',
     &'Sn','Sb','Te','I ','Xe','Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','
     &Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta','W ','Re','O
     &s','Ir','Pt','Au','Hg','Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac
     &','Th','Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es','Fm','Md','No'
     &,'Lr','Ky'/
      
      
      
      
      
      
      
      
      
99001 format(1x,72('-'))
99002 format(1x,24x,'Z-MATRIX (ANGSTROMS AND DEGREES)')
99003 format(1x,'CD CENT ATOM  N1',6x,'LENGTH',6x,'N2',5x,'ALPHA',6x,'N3
     &',6x,'BETA',7x,'J')
99004 format(1x,i2,2x,i2,3x,a2)
99005 format(1x,i2,7x,a2)
99006 format(1x,i2,2x,i2,3x,a2,3x,i2,2x,f9.6,' (',i3,')')
99007 format(1x,i2,7x,a2,3x,i2,2x,f9.6,' (',i3,')')
99008 format(1x,i2,2x,i2,3x,a2,3x,i2,2x,f9.6,' (',i3,') ',i2,1x,f8.3,' (
     &',i3,')')
99009 format(1x,i2,7x,a2,3x,i2,2x,f9.6,' (',i3,') ',i2,1x,f8.3,' (',i3,'
     &)')
99010 format(1x,i2,2x,i2,3x,a2,3x,i2,2x,f9.6,' (',i3,') ',i2,1x,f8.3,' (
     &',i3,') ',i2,1x,f8.3,' (',i3,') ',i2)
99011 format(1x,i2,7x,a2,3x,i2,2x,f9.6,' (',i3,') ',i2,1x,f8.3,' (',i3,'
     &) ',i2,1x,f8.3,' (',i3,') ',i2)
      
      
      todeg=f45/gatan(one)
      
      
      write(Iout,99001)
      write(Iout,99002)
      write(Iout,99003)
      write(Iout,99001)
      
      
      icard=1
      idx=IANZ(1)+2
      if(IANZ(1).LT.0)then
      
      icent=0
      write(Iout,99005)icard,iel(idx)
      else
      icent=1
      write(Iout,99004)icard,icent,iel(idx)
      endif
      if(NZ.NE.1)then
      
      
      np1=1
      icard=2
      idx=IANZ(2)+2
      pbl=BL(2)*CONVER
      if(IANZ(2).LT.0)then
      
      write(Iout,99007)icard,iel(idx),IZ(2,1),pbl,np1
      else
      icent=icent+1
      write(Iout,99006)icard,icent,iel(idx),IZ(2,1),pbl,np1
      endif
      if(NZ.NE.2)then
      
      
      np1=2
      np2=NZ
      icard=3
      idx=IANZ(3)+2
      pbl=BL(3)*CONVER
      pa=ALPHA(3)*todeg
      if(IANZ(3).LT.0)then
      
      write(Iout,99009)icard,iel(idx),IZ(3,1),pbl,np1,IZ(3,2),pa,np2
      else
      icent=icent+1
      write(Iout,99008)icard,icent,iel(idx),IZ(3,1),pbl,np1,IZ(3,2),pa,n
     &p2
      endif
      if(NZ.NE.3)then
      
      
      do 10 icard=4,NZ
      np1=icard-1
      np2=NZ+icard-3
      np3=NZ*2+icard-6
      idx=IANZ(icard)+2
      pbl=BL(icard)*CONVER
      pa=ALPHA(icard)*todeg
      pb=BETA(icard)*todeg
      if(IANZ(icard).LT.0)then
      
      write(Iout,99011)icard,iel(idx),IZ(icard,1),pbl,np1,IZ(icard,2),pa
     &,np2,IZ(icard,3),pb,np3,IZ(icard,4)
      else
      icent=icent+1
      write(Iout,99010)icard,icent,iel(idx),IZ(icard,1),pbl,np1,IZ(icard
     &,2),pa,np2,IZ(icard,3),pb,np3,IZ(icard,4)
      endif
10    continue
      endif
      endif
      endif
      
      
      write(Iout,99001)
      
      return
      
      end
C* :1 * 
      
