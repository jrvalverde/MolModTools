
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 minbas"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "minbas.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "minbas.web"
      subroutine minbas(MBASIS,NATOMS,IAN,C)
      implicit none
      double precision C,C1,C2,C3,Exx,X,Y,Z
      integer IAN,In,Iout,Ipunch,Jan,LENB,MAXPRM,MAXS21,MAXSH1,MAXSHL,Ma
     &xtyp,MBASIS,NATOMS,Nshell
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon
      dimension IAN(*),C(*)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      common/io/In,Iout,Ipunch
      
      
      
      call sto(0,NATOMS,IAN,C,MBASIS,3,0)
      call renorm
      return
      
      end
C* :1 * 
      
