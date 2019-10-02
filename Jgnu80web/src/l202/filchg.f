
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 filchg"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "filchg.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "filchg.web"
      subroutine filchg(NATOMS,IAN,ATMCHG,CHARGE,NDCHG,IPRINT)
      implicit none
      double precision ATMCHG,ch,CHARGE,gfloat
      integer i,IAN,In,Iout,IPRINT,Ipunch,NATOMS,NDCHG
      dimension IAN(*),ATMCHG(*),CHARGE(*)
      common/io/In,Iout,Ipunch
      
      
      
      
      if(NDCHG.NE.0)then
      
      if(IPRINT.NE.0)write(Iout,99001)
      do 50 i=1,NATOMS
      ch=CHARGE(i)
      if(IPRINT.NE.0)write(Iout,99002)i,ch
      ATMCHG(i)=ch
50    continue
      return
      
99001 format('  NON-DEFAULT ATOMIC CHARGES USED')
99002 format(' ',i10,g15.5)
      endif
      do 100 i=1,NATOMS
      ATMCHG(i)=gfloat(IAN(i))
100   continue
      return
      end
C* :1 * 
      
