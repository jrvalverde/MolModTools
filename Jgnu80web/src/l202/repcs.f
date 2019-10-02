
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repcs"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repcs.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "repcs.web"
      subroutine repcs
      implicit none
      integer Icnt,Isave,Nsave
      double precision cstbl(2,2)
      common/reploc/Nsave,Icnt,Isave(15)
      data cstbl/0.0,1.0,0.0,-1.0/
      
      Nsave=1
      Icnt=0
      Isave(1)=1
      call puttbl(2,2,cstbl)
      call labrep('A'',A",',2)
      return
      
      end
C* :1 * 
      
