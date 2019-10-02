
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repci"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repci.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "repci.web"
      subroutine repci
      implicit none
      integer Icnt,Isave,Nsave
      double precision citbl(2,2)
      common/reploc/Nsave,Icnt,Isave(15)
      data citbl/0.0,1.0,0.0,-1.0/
      
      Nsave=1
      Icnt=0
      Isave(1)=1
      call puttbl(2,2,citbl)
      call labrep('AG,AU,',2)
      return
      
      end
C* :1 * 
      
