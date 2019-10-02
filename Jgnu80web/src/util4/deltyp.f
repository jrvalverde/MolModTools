
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 deltyp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "deltyp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "deltyp.web"
      integer function deltyp(CHR)
      implicit none
      integer CHR,i,iord,lf
      data lf/10/
      
      i=0
      if(CHR.EQ.iord(' ').OR.CHR.EQ.iord(',').OR.CHR.EQ.iord('='))i=1
      if(CHR.EQ.iord('/'))i=2
      if(CHR.EQ.lf)i=4
      deltyp=i
      return
      
      end
C* :1 * 
      
