
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 typcon"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "typcon.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "typcon.web"
      subroutine typcon(ITYPE,STYPE,SCON)
      implicit none
      integer i,isc,ist,itest,ITYPE
      integer STYPE,SCON
      dimension itest(6),ist(6),isc(6)
      data itest/4H   S,4H   P,4H   D,4H   F,4H  SP,4H SPD/
      data ist/0,1,2,3,1,2/,isc/2,1,2,2,2,0/
      
      
      
      
      do 100 i=1,6
      if(ITYPE.EQ.itest(i))then
      STYPE=ist(i)
      SCON=isc(i)
      return
      endif
      
100   continue
      call berror(8)
      call killer
      stop
      
      end
C* :1 * 
      
