
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 deltyc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "deltyc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "deltyc.web"
      integer function deltyc(CHR)
      implicit none
      integer i
      character*1 CHR
      
      i=0
      if(CHR.EQ.' '.OR.CHR.EQ.','.OR.CHR.EQ.'=')i=1
      if(CHR.EQ.'/')i=2
      deltyc=i
      return
      
      end
C* :1 * 
      
