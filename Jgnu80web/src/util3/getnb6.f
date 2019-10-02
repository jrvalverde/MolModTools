
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getnb6"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getnb6.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "getnb6.web"
      subroutine getnb6(NBAS6D)
      implicit none
      double precision C1,C2,C3,Exx,X,Y,Z
      integer iend,ishell,istart,itype,Jan,lamax,Lbound,MAXPRM,MAXS21,MA
     &XSH1,MAXSHL,Maxtyp,N10ord,N5ord,N6ord,N7ord,NBAS6D,Nordr,Nshell
      integer Shella,Shelln,Shellt,Shellc,shladf,Aos,Aon
      integer scona,sconb
      integer Ubound,Ulpure
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      
      
      
      
      
      NBAS6D=0
      do 100 ishell=1,Nshell
      itype=Shellt(ishell)
      lamax=itype+1
      scona=Shellc(ishell)
      iend=Ubound(lamax)
      istart=Lbound(lamax,scona+1)
      NBAS6D=NBAS6D+(iend-istart+1)
100   continue
      return
      
      end
C* :1 * 
      
