
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 corpr1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "corpr1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "corpr1.web"
      subroutine corpr1(NATOMS,IAN,C,CONVER)
      implicit none
      double precision C,cloc,CONVER
      integer i,iaind,IAN,iat,icent,In,Iout,Ipunch,ixyz,NATOMS
      dimension IAN(*),C(*),cloc(3)
      common/io/In,Iout,Ipunch
      
      
      
      
      
      
99001 format(1x,72('-'))
99002 format(1x,'CENTER',8x,'ATOMIC',18x,'COORDINATES (ANGSTROMS)'/1x,'N
     &UMBER',8x,'NUMBER',15x,'X',13x,'Y',13x,'Z')
99003 format(1x,i4,10x,i4,12x,3F14.8)
99004 format(1x,14x,i4,12x,3F14.8)
      
      write(Iout,99001)
      write(Iout,99002)
      write(Iout,99001)
      icent=0
      iaind=0
      do 100 iat=1,NATOMS
      do 50 i=1,3
      cloc(i)=CONVER*C(i+iaind)
50    continue
      if(IAN(iat).LT.0)then
      
      write(Iout,99004)IAN(iat),(cloc(ixyz),ixyz=1,3)
      else
      icent=icent+1
      write(Iout,99003)icent,IAN(iat),(cloc(ixyz),ixyz=1,3)
      endif
      iaind=iaind+3
100   continue
      write(Iout,99001)
      return
      
      end
C* :1 * 
      
