
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rysset"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rysset.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "rysset.web"
      subroutine rysset(KOP1,KOP2)
      implicit none
      integer Iop1,Iop2,KOP1,KOP2
      common/rys/Iop1,Iop2
      
      Iop1=KOP1
      Iop2=KOP2
      if(KOP2.NE.0)call fmtset(0,0,0)
      call setr1
      
      return
      
      end
C* :1 * 
      
