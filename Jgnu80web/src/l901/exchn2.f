
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 exchn2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "exchn2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "exchn2.web"
      subroutine exchn2(NO,NV,IBUC1,IBUC2,IBUC3)
      implicit none
      integer IBUC1,IBUC2,IBUC3,Mdv,NO,nov,novtot,NV
      double precision one,V
      common/v/V(20000),Mdv
      data one/1.D0/
      
      
      
      
      
      
      
      call track('EXCHN2')
      
      nov=NO*NV
      if(nov.LE.0)return
      novtot=nov*nov
      call sumn(IBUC2,IBUC1,novtot,-one)
      call trsfr(novtot,IBUC1,IBUC3)
      
      return
      
      end
C* :1 * 
      
