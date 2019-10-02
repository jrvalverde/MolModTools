
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fzprnt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fzprnt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "fzprnt.web"
      subroutine fzprnt(MAXNZ,NZ,IANZ,IZ,F,IOUT)
      implicit none
      double precision F
      integer IANZ,icard,icent,idx,iel,IOUT,IZ,MAXNZ,np1,np2,np3,NZ
      dimension IANZ(4),IZ(MAXNZ,4),F(10)
      dimension iel(106)
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
99002 format(1x,19x,'INTERNAL COORDINATE FORCES (HARTREES/BOHR OR ','/RA
     &DIAN)'/1x,'CENT ATOM N1',6x,'LENGTH',6x,'N2',6x,'ALPHA',7x,'N3',7x
     &,'BETA',7x,' J')
99003 format(1x,i2,3x,a2)
99004 format(1x,5x,a2)
99005 format(1x,i2,3x,a2,2x,i3,1x,f10.6,' (',i3,')')
99006 format(1x,5x,a2,2x,i3,1x,f10.6,' (',i3,')')
99007 format(1x,i2,3x,a2,2x,i3,1x,f10.6,' (',i3,')',1x,i2,1x,f10.6,' (',
     &i3,')')
99008 format(1x,5x,a2,2x,i3,1x,f10.6,' (',i3,')',1x,i2,1x,f10.6,' (',i3,
     &')')
99009 format(1x,i2,3x,a2,2x,i3,1x,f10.6,' (',i3,')',1x,i2,1x,f10.6,' (',
     &i3,')',1x,i2,1x,f10.6,' (',i3,')',i3)
99010 format(1x,5x,a2,2x,i3,1x,f10.6,' (',i3,')',1x,i2,1x,f10.6,' (',i3,
     &')',1x,i2,1x,f10.6,' (',i3,')',i3)
      
      
      
      write(IOUT,99001)
      write(IOUT,99002)
      write(IOUT,99001)
      idx=IANZ(1)+2
      if(IANZ(1).LT.0)then
      
      icent=0
      write(IOUT,99004)iel(idx)
      else
      icent=1
      write(IOUT,99003)icent,iel(idx)
      endif
      if(NZ.NE.1)then
      np1=1
      idx=IANZ(2)+2
      if(IANZ(2).LT.0)then
      
      write(IOUT,99006)iel(idx),IZ(2,1),F(1),np1
      else
      icent=icent+1
      write(IOUT,99005)icent,iel(idx),IZ(2,1),F(1),np1
      endif
      if(NZ.NE.2)then
      np1=2
      np2=NZ
      idx=IANZ(3)+2
      if(IANZ(3).LT.0)then
      
      write(IOUT,99008)iel(idx),IZ(3,1),F(2),np1,IZ(3,2),F(NZ),np2
      else
      icent=icent+1
      write(IOUT,99007)icent,iel(idx),IZ(3,1),F(2),np1,IZ(3,2),F(NZ),np2
      endif
      if(NZ.NE.3)then
      do 10 icard=4,NZ
      np1=icard-1
      np2=NZ+icard-3
      np3=NZ*2+icard-6
      idx=IANZ(icard)+2
      if(IANZ(icard).LT.0)then
      
      write(IOUT,99010)iel(idx),IZ(icard,1),F(np1),np1,IZ(icard,2),F(np2
     &),np2,IZ(icard,3),F(np3),np3,IZ(icard,4)
      else
      icent=icent+1
      write(IOUT,99009)icent,iel(idx),IZ(icard,1),F(np1),np1,IZ(icard,2)
     &,F(np2),np2,IZ(icard,3),F(np3),np3,IZ(icard,4)
      endif
10    continue
      endif
      endif
      endif
      
      write(IOUT,99001)
      
      return
      
      end
C* :1 * 
      
