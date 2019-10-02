
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 replct"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "replct.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "replct.web"
      subroutine replct(LENTQ,TQ1,TQ2,I1,I2)
      implicit none
      integer i,I1,I2,Idmp,Idump,Is,j,LENTQ
      double precision TQ1,TQ2
      dimension TQ1(*),TQ2(*)
      common/dump/Idmp,Idump
      common/site/Is(10,3,3)
      
      
      
      
      do 100 i=1,LENTQ
      TQ2(i)=TQ1(i)
100   continue
      
      do 200 j=1,3
      do 150 i=1,10
      Is(i,j,I2)=Is(i,j,I1)
150   continue
200   continue
      
      return
      
      end
C* :1 * 
      
