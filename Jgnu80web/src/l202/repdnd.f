
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repdnd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repdnd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "repdnd.web"
      subroutine repdnd(N)
      implicit none
      double precision d2dtbl,d3dtbl,d4dtbl,gsqrt,two
      integer Icnt,Isave,N,nd2d,nd3d,nd4d,Nsave
      dimension d2dtbl(3,5)
      dimension d3dtbl(3,6)
      dimension d4dtbl(3,7)
      common/reploc/Nsave,Icnt,Isave(15)
      data two/2.0D0/
      data nd2d/5/
      data d2dtbl/0.0,1.0,1.0,0.0,-1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,-1.0,0.
     &0,0.0,0.0/
      data nd3d/6/
      data d3dtbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,1.0,0.0,-1.0,-1.0,0.
     &0,0.0,-1.0,0.0,0.0,1.0/
      data nd4d/7/
      data d4dtbl/0.0,1.0,1.0,0.0,-1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,-1.0,0.
     &0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0/
      
      
      
      
      
      
      
      Icnt=0
      Nsave=2
      Isave(1)=1
      Isave(2)=2*N+1
      
      if(N.EQ.2)then
      call puttbl(Nsave+1,nd2d,d2dtbl)
      call labrep('A1,A2,B1,B2,E,',5)
      return
      
      elseif(N.EQ.3)then
      call puttbl(Nsave+1,nd3d,d3dtbl)
      call labrep('A1G,A1U,A2G,A2U,EG,EU,',6)
      return
      
      elseif(N.EQ.4)then
      d4dtbl(3,5)=gsqrt(two)
      d4dtbl(3,7)=-gsqrt(two)
      call puttbl(Nsave+1,nd4d,d4dtbl)
      call labrep('A1,A2,B1,B2,E1,E2,E3,',7)
      return
      endif
      
      call puttbl(Nsave+1,0,0)
      return
      
      end
C* :1 * 
      
