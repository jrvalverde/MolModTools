
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dagtyd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dagtyd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "dagtyd.web"
      subroutine dagtyd(I)
      implicit none
      integer k
      integer I(8),ii(8)
      data ii/' Thi','s is',' a m','essa','ge  ','from','  DA','GTYD'/
      
      do 100 k=1,8
      I(k)=ii(k)
100   continue
      return
      
      end
C* :1 * 
      
