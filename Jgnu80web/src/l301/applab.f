
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 applab"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "applab.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "applab.web"
      subroutine applab(ELEMNT,N,STR,LABEL,LCURSR,MOD)
      implicit none
      integer ELEMNT,In,Iout,Ipunch,LABEL,LCURSR,MOD,N,STR,tcur
      integer modify(3)
      dimension ELEMNT(*),LABEL(*),STR(*)
      common/io/In,Iout,Ipunch
      data modify/'(I)','(M)','(O)'/
      if(LCURSR.GT.3890)then
      write(Iout,*)'APPLAB; Not Enough ROOM in /LABEL/'
      call lnk1e
      endif
      call putb(ELEMNT,6,LABEL,LCURSR)
      
      tcur=0
      call pad(ELEMNT,tcur,6,1H )
      
      if(N.LT.10)call putchr(' ',LABEL,LCURSR)
      if(N.EQ.0)call putchr(' ',LABEL,LCURSR)
      if(N.NE.0)call decchr(N,LABEL,LCURSR)
      
      call putb(STR,3,LABEL,LCURSR)
      
      if(MOD.EQ.2)call putb(modify(1),3,LABEL,LCURSR)
      if(MOD.EQ.3)call putb(modify(2),3,LABEL,LCURSR)
      if(MOD.EQ.4)call putb(modify(3),3,LABEL,LCURSR)
      
      call putdel(2,LABEL,LCURSR)
      return
      
      end
C* :1 * 
      
