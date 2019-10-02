
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repoh"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repoh.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "repoh.web"
      subroutine repoh
      implicit none
      integer Icnt,Isave,Nsave
      double precision ohtbl(4,10)
      common/reploc/Nsave,Icnt,Isave(15)
      data ohtbl/0.0,1.0,1.0,1.0,0.0,1.0,1.0,-1.0,0.0,1.0,-1.0,1.0,0.0,1
     &.0,-1.0,-1.0,0.0,-1.0,0.0,2.0,0.0,-1.0,0.0,-2.0,0.0,0.0,-1.0,3.0,0
     &.0,0.0,-1.0,-3.0,0.0,0.0,1.0,3.0,0.0,0.0,1.0,-3.0/
      
      
      
      
      Nsave=3
      Icnt=0
      Isave(1)=10
      Isave(2)=18
      Isave(3)=24
      call puttbl(4,10,ohtbl)
      call labrep('A1G,A1U,A2G,A2U,EG,EU,T1G,T1U,T2G,T2U,',10)
      return
      
      end
C* :1 * 
      
