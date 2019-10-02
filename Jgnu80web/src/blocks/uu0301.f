
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uu0301"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uu0301.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 16 "uu0301.web"
      blockdata uu0301
      implicit none
      double precision C1,C2,C3,Exx,X,Y,Z
      integer Iao,Jan,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,Nshell
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon
      common/iao/Iao(4)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      data Iao/4H  1S,4H 2SP,4H 3SP,4HD4SP/
      data Exx,C1,C2,C3/MAXPRM*0.,MAXPRM*0.,MAXPRM*0.,MAXPRM*0./,X,Y,Z/M
     &AXSHL*0.,MAXSHL*0.,MAXSHL*0./,Jan,Shella,Shelln,Shellt,Shellc/MAXS
     &HL*0,MAXSHL*0,MAXSHL*0,MAXSHL*0,MAXSHL*0/,Aos,Aon/MAXSHL*0,MAXSHL*
     &0/,Nshell,Maxtyp/0,0/
      end
C* :1 * 
      
