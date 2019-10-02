
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lentqf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lentqf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "lentqf.web"
      integer function lentqf(ISH,ISTM)
      implicit none
      double precision C1,C2,C3,Exx,X,Y,Z
      integer ISH,ISTM,Jan,lamax,Lbound,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxt
     &yp,N10ord,N5ord,N6ord,N7ord,Nordr,Nshell
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon
      integer sconap
      integer Ubound,Ulpure
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      
      
      
      sconap=Shellc(ISH)+1
      lamax=Shellt(ISH)+1
      lentqf=Ubound(lamax)-Lbound(lamax,sconap)+1
      ISTM=Lbound(lamax,sconap)-1
      
      return
      
      end
C* :1 * 
      
