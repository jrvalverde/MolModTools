
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lospot"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lospot.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "lospot.web"
      subroutine lospot(NVAL,DK,ZETAK,KF,KL,MAX,LSKP,TYPE,ICOR,IA)
      implicit none
      double precision DK,ZETAK
      integer IA,ICOR,In,Iout,Ipunch,KF,KL,LSKP,MAX,NVAL
      
      
      dimension NVAL(*),DK(*),ZETAK(*),KF(*),KL(*)
      character*4 TYPE,pottyp(104)
      common/io/In,Iout,Ipunch
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
99001 format(' NO Lanthanides or Z>83 atoms in the LOSPOT set. ',i4)
      
      if((IA.GE.58.AND.IA.LE.71).OR.IA.GT.83)write(Iout,99001)IA
      if((IA.GE.58.AND.IA.LE.71).OR.IA.GT.83)call lnk1e
      
      TYPE=pottyp(IA)
      if(IA.LE.10)then
      ICOR=0
      LSKP=1
      MAX=2
      return
      endif
      LSKP=0
      if((IA.GE.11).AND.(IA.LE.18))call los2nd(NVAL,DK,ZETAK,KF,KL,MAX,L
     &SKP,ICOR,IA)
      if((IA.GE.19).AND.(IA.LE.36))call los3rd(NVAL,DK,ZETAK,KF,KL,MAX,L
     &SKP,ICOR,IA)
      if((IA.GE.37).AND.(IA.LE.54))call los4th(NVAL,DK,ZETAK,KF,KL,MAX,L
     &SKP,ICOR,IA)
      if((IA.GE.55).AND.(IA.LE.83))call los5th(NVAL,DK,ZETAK,KF,KL,MAX,L
     &SKP,ICOR,IA)
      return
      end
C* :1 * 
      
