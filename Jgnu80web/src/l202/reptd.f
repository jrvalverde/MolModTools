
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 reptd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "reptd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "reptd.web"
      subroutine reptd
      implicit none
      integer Icnt,Isave,Nsave
      double precision tdtbl(3,5)
      common/reploc/Nsave,Icnt,Isave(15)
      data tdtbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,0.0,0.0,0.0,-1.0,0.0,
     &0.0,1.0/
      
      
      
      
      Nsave=2
      Icnt=0
      Isave(1)=1
      Isave(2)=18
      call puttbl(3,5,tdtbl)
      call labrep('A1,A2,E,T1,T2,',5)
      return
      
      end
C* :1 * 
      
