
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 filmax"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "filmax.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "filmax.web"
      subroutine filmax
      implicit none
      double precision a1,a2,Auxvar,C1,C2,C3,Cmax,Cmaxa,Cmaxb,Cmaxc,Cmax
     &d,Error1,Error2,Exx,fiften,five,pt0001,Var1,Var2,X
      double precision Y,Z
      integer i,Isml,Ismlp,Ismlq,j,Jan,l,MAXPRM,MAXS21,MAXSH1,MAXSHL,Max
     &typ,n,Nshell
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon
      common/auxvar/Auxvar,Var1,Var2
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      common/maxc/Cmax(240),Cmaxa(10),Cmaxb(10),Cmaxc(10),Cmaxd(10),Isml
     &p(100),Ismlq,Isml,Error1,Error2
      data five/5.0D0/,fiften/15.0D0/
      data pt0001/1.0D-4/
      
      
      
      
      
      
      
      
      do 100 i=1,Nshell
      l=Shella(i)
      n=l+Shelln(i)-1
      do 50 j=l,n
      a1=dabs(C1(j))
      a2=dabs(C2(j))
      Cmax(j)=dmax1(a1,a2)
50    continue
100   continue
      Error1=pt0001
      Var1=fiften
      Var2=five
      Error2=Error1*Error1
      return
      
      end
C* :1 * 
      
