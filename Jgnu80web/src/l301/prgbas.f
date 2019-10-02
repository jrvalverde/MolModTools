
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 prgbas"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "prgbas.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "prgbas.web"
      subroutine prgbas
      implicit none
      integer i,iatom,In,Iout,Ipunch,j,LENB,mm,mmdf,name,namesh
      common/io/In,Iout,Ipunch
      integer MAXSHL,MAXPRM,MAXSH1,MAXS21,Jan,Shella,Shelln,Shellt,Shell
     &c,Shladf,Aos,Aon,Nshell,Maxtyp,ishl
      real*8 Exx,C1,C2,C3,C4,X,Y,Z
      double precision rshell,crit
      
      
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      dimension ishl(MAXSHL)
      dimension namesh(4,3)
      save namesh
      data namesh/3H  S,3H SP,3HSPD,3H???,3H???,3H  P,3H  D,3H???,3H  S,
     &3H SP,3H  D,3H  F/
99001 format(' ****')
99002 format(10I2)
99003 format(11x,a3,i2,' 1.00')
99004 format(4D18.10)
99005 format(' GBASIS BASIS SET in a form suitable for input')
99006 format(' ****',/)
      
      rshell(i,j)=sqrt((x(i)-x(j))**2+(y(i)-y(j))**2+(z(i)-z(j))**2)
      write(Iout,99005)
      
      crit=0.1d00
      
      j=0
      do 40 i=1,nshell
      j=j+1
      if((i.EQ.nshell).OR.(abs(rshell(i,i+1)).GT.crit))then
      ishl(j)=shelln(i)
      j=j+1
      ishl(j)=0
      else
      ishl(j)=shelln(i)
      endif
40    continue
      ishl(j)=9
      write(iout,1140)(ishl(i),i=1,j)
1140  format(80i1)
      
      iatom=0
      do 100 i=1,Nshell
      if(i.GT.1.AND.Jan(i).NE.Jan(i-1))write(Iout,99001)
      if(i.GT.1.AND.abs(rshell(i,i-1)).GT.crit)then
      iatom=iatom+1
      write(Iout,99002)iatom
      endif
      
      name=namesh(Shellt(i)+1,Shellc(i)+1)
      write(Iout,99003)name,Shelln(i)
      mm=Shella(i)
      mmdf=Shladf(i)
      do 50 j=1,Shelln(i)
      write(Iout,99004)Exx(mm),C1(mm),C2(mm),C3(mmdf)
      mm=mm+1
      mmdf=mmdf+1
50    continue
100   continue
      write(Iout,99006)
      return
      end
C* :1 * 
      
