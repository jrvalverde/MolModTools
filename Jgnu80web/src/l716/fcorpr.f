
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fcorpr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fcorpr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "fcorpr.web"
      subroutine fcorpr(NATOMS,IAN,F,IOUT)
      implicit none
      double precision cloc,F
      integer i,iaind,IAN,iat,icent,IOUT,NATOMS
      dimension IAN(*),F(*),cloc(3)
      
      
      
      
      
      
      
      
99001 format(1x,58('-'))
99002 format(1x,'CENTER',5x,'ATOMIC',14x,' FORCES (HARTREES/BOHR)'/1x,'N
     &UMBER',5x,'NUMBER',13x,'X',11x,'Y',11x,'Z')
99003 format(1x,i4,7x,i4,7x,3F12.6)
99004 format(1x,11x,i4,7x,3F12.6)
      
      write(IOUT,99001)
      write(IOUT,99002)
      write(IOUT,99001)
      icent=0
      iaind=0
      do 100 iat=1,NATOMS
      do 50 i=1,3
      cloc(i)=F(i+iaind)
50    continue
      if(IAN(iat).LT.0)then
      
      write(IOUT,99004)IAN(iat),cloc(1),cloc(2),cloc(3)
      else
      icent=icent+1
      write(IOUT,99003)icent,IAN(iat),cloc(1),cloc(2),cloc(3)
      endif
      iaind=iaind+3
100   continue
      write(IOUT,99001)
      return
      
      end
C* :1 * 
      
