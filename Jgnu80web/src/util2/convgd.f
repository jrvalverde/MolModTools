
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 convgd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "convgd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "convgd.web"
      subroutine convgd(A,B,RESULT)
      implicit none
      double precision A,B
      integer RESULT(3),y,e,s,n,o,blank
      data y,e,s,n,o,blank/'Y','E','S','N','O',' '/
      
      
      
      
      
      
      if(A.GE.B)then
      RESULT(1)=n
      RESULT(2)=o
      RESULT(3)=blank
      return
      endif
      
      RESULT(1)=y
      RESULT(2)=e
      RESULT(3)=s
      return
      
      end
C* :1 * 
      
