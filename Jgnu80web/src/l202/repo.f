
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "repo.web"
      subroutine repo
      implicit none
      integer Icnt,Isave,Nsave
      double precision otbl(3,5)
      common/reploc/Nsave,Icnt,Isave(15)
      data otbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,0.0,0.0,0.0,-1.0,0.0,0
     &.0,1.0/
      
      
      
      
      Nsave=2
      Icnt=0
      Isave(1)=10
      Isave(2)=19
      call puttbl(3,5,otbl)
      call labrep('A1,A2,E,T1,T2,',5)
      return
      
      end
C* :1 * 
      
