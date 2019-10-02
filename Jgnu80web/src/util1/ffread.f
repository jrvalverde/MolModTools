
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ffread"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ffread.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "ffread.web"
      subroutine ffread(IEOF)
      implicit none
      integer i,Idump,IEOF,In,Iold,Iout,Ipunch,Lcursr,Line,Ncom
      integer lline(20)
      common/io/In,Iout,Ipunch
      common/fffcom/Idump,Lcursr,Iold,Ncom,Line(40)
      
99001 format(20A4)
99002 format('  END-OF-FILE IN FFREAD.')
99003 format('  FFREAD:',/,1x,20A4)
      
      IEOF=0
      Lcursr=0
      Iold=0
      Ncom=1
      read(In,99001,end=100)(lline(i),i=1,20)
      call captlz(lline,Line,80)
      if(Idump.NE.0)write(Iout,99003)(Line(i),i=1,20)
      return
      
100   IEOF=1
      if(Idump.EQ.0)write(Iout,99002)
      return
      
      end
C* :1 * 
      
