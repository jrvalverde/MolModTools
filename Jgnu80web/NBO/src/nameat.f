
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nameat"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nameat.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "nameat.web"
      function nameat(IZ)
      implicit none
      integer iblank,ighost,IZ,name,nameat
      
      
      dimension name(103)
      data ighost/'gh'/iblank/'  '/
      data name/' H','He','Li','Be',' B',' C',' N',' O',' F','Ne','Na','
     &Mg','Al','Si',' P',' S','Cl','Ar',' K','Ca','Sc','Ti',' V','Cr','M
     &n','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr','Rb','Sr
     &',' Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn','Sb'
     &,'Te',' I','Xe','Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd',
     &'Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta',' W','Re','Os','Ir','
     &Pt','Au','Hg','Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th','P
     &a',' U','Np','Pu','Am','Cm','Bk','Cf','Es','Fm','Md','No','Lr'/
      
      if(IZ.LT.0.OR.IZ.GT.103)nameat=iblank
      if(IZ.GT.0)nameat=name(IZ)
      if(IZ.EQ.0)nameat=ighost
      return
      end
C* :1 * 
      
