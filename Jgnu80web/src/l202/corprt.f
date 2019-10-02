
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 corprt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "corprt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "corprt.web"
      subroutine corprt(MAXAT,NATOMS,IAN,C,CONVER)
      implicit none
      integer IAN,iat,icent,In,Iout,Ipunch,ixyz,MAXAT,NATOMS
      dimension IAN(*)
      double precision C(MAXAT,3),cloc(100,3),CONVER
      common/io/In,Iout,Ipunch
      
      
      
      
      
      
99001 format(1x,72('-'))
99002 format(1x,'CENTER',8x,'ATOMIC',18x,'COORDINATES (ANGSTROMS)'/1x,'N
     &UMBER',8x,'NUMBER',15x,'X',13x,'Y',13x,'Z')
99003 format(1x,i4,10x,i4,12x,3F14.8)
99004 format(1x,14x,i4,12x,3F14.8)
      
      do 100 iat=1,NATOMS
      do 50 ixyz=1,3
      cloc(iat,ixyz)=C(iat,ixyz)*CONVER
50    continue
100   continue
      
      write(Iout,99001)
      write(Iout,99002)
      write(Iout,99001)
      icent=0
      do 200 iat=1,NATOMS
      if(IAN(iat).LT.0)then
      
      write(Iout,99004)IAN(iat),(cloc(iat,ixyz),ixyz=1,3)
      else
      icent=icent+1
      write(Iout,99003)icent,IAN(iat),(cloc(iat,ixyz),ixyz=1,3)
      endif
200   continue
      write(Iout,99001)
      return
      
      end
C* :1 * 
      
