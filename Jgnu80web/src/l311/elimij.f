
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 elimij"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "elimij.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "elimij.web"
      subroutine elimij(IC)
      implicit none
      integer i,IC
      dimension IC(*)
      
      do 100 i=17,64
      IC(i)=0
100   continue
      do 200 i=97,128
      IC(i)=0
200   continue
      do 300 i=177,192
      IC(i)=0
300   continue
      return
      
      end
C* :1 * 
      
