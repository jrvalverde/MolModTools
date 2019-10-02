
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 conddf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "conddf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "conddf.web"
      subroutine conddf(IBUCK,LNG)
      implicit none
      integer IBUCK,leng,LNG
      
      
      
      
      call tquery(IBUCK,leng)
      if(leng.EQ.0)call fileio(0,IBUCK,LNG)
      
      return
      
      end
C* :1 * 
      
