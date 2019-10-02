
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repdn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repdn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "repdn.web"
      subroutine repdn(N)
      implicit none
      integer Icnt,Isave,N,nd2,nd3,nd4,nd6,Nsave
      double precision d2tbl(3,4),d3tbl(3,3)
      double precision d4tbl(3,5),d6tbl(3,6)
      common/reploc/Nsave,Icnt,Isave(15)
      data nd2/4/
      data d2tbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,1.0,0.0,-1.0,-1.0/
      data nd3/3/
      data d3tbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,0.0/
      data nd4/5/
      data d4tbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,1.0,0.0,-1.0,-1.0,0.0
     &,0.0,0.0/
      data nd6/6/
      data d6tbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,1.0,0.0,-1.0,-1.0,0.0
     &,1.0,0.0,0.0,-1.0,0.0/
      
      
      
      
      
      
      
      
      
      
      Icnt=0
      Nsave=2
      Isave(1)=1
      Isave(2)=N
      
      if(N.EQ.2)then
      call puttbl(Nsave+1,nd2,d2tbl)
      call labrep('A,B1,B2,B3,',4)
      return
      
      elseif(N.EQ.3)then
      call puttbl(Nsave+1,nd3,d3tbl)
      call labrep('A1,A2,E,',3)
      return
      
      elseif(N.EQ.4)then
      call puttbl(Nsave+1,nd4,d4tbl)
      call labrep('A1,A2,B1,B2,E,',5)
      return
      
      elseif(N.EQ.6)then
      call puttbl(Nsave+1,nd6,d6tbl)
      call labrep('A1,A2,B1,B2,E1,E2,',6)
      return
      endif
      
      call puttbl(Nsave+1,0,0)
      return
      
      end
C* :1 * 
      
