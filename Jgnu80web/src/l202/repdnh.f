
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repdnh"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repdnh.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "repdnh.web"
      subroutine repdnh(N)
      implicit none
      double precision f144,f180,f72,four,gatan,gcos,one,pi,two
      integer Icnt,Isave,N,nd2h,nd3h,nd4h,nd5h,nd6h,Nsave
      double precision d2htbl(4,8),d3htbl(4,6)
      double precision d4htbl(4,10),d6htbl(4,12)
      double precision d5htbl(4,8)
      common/reploc/Nsave,Icnt,Isave(15)
      data nd2h/8/
      data d2htbl/0.0,1.0,1.0,1.0,0.0,1.0,1.0,-1.0,0.0,1.0,-1.0,1.0,0.0,
     &1.0,-1.0,-1.0,0.0,-1.0,1.0,-1.0,0.0,-1.0,1.0,1.0,0.0,-1.0,-1.0,-1.
     &0,0.0,-1.0,-1.0,1.0/
      data nd3h/6/
      data d3htbl/0.0,1.0,1.0,1.0,0.0,1.0,1.0,-1.0,0.0,1.0,-1.0,1.0,0.0,
     &1.0,-1.0,-1.0,0.0,-1.0,0.0,2.0,0.0,-1.0,0.0,-2.0/
      data nd4h/10/
      data d4htbl/0.0,1.0,1.0,1.0,0.0,1.0,1.0,-1.0,0.0,1.0,-1.0,1.0,0.0,
     &1.0,-1.0,-1.0,0.0,-1.0,1.0,1.0,0.0,-1.0,1.0,-1.0,0.0,-1.0,-1.0,1.0
     &,0.0,-1.0,-1.0,-1.0,0.0,0.0,0.0,-2.0,0.0,0.0,0.0,2.0/
      data nd5h/8/
      data d5htbl/0.0,1.0,1.0,1.0,0.0,1.0,1.0,-1.0,0.0,1.0,-1.0,1.0,0.0,
     &1.0,-1.0,-1.0,0.0,0.0,0.0,2.0,0.0,0.0,0.0,-2.0,0.0,0.0,0.0,2.0,0.0
     &,0.0,0.0,-2.0/
      data nd6h/12/
      data d6htbl/0.0,1.0,1.0,1.0,0.0,1.0,1.0,-1.0,0.0,1.0,-1.0,1.0,0.0,
     &1.0,-1.0,-1.0,0.0,-1.0,1.0,-1.0,0.0,-1.0,1.0,1.0,0.0,-1.0,-1.0,-1.
     &0,0.0,-1.0,-1.0,1.0,0.0,1.0,0.0,-2.0,0.0,1.0,0.0,2.0,0.0,-1.0,0.0,
     &2.0,0.0,-1.0,0.0,-2.0/
      data one/1.0D0/,two/2.0D0/,four/4.0D0/
      data f72/72.0D0/,f144/144.0D0/,f180/180.0D0/
      
      
      
      
      
      
      
      
      
      
      Icnt=0
      Nsave=3
      Isave(1)=1
      Isave(2)=N
      Isave(3)=3*N
      
      if(N.EQ.2)then
      call puttbl(Nsave+1,nd2h,d2htbl)
      call labrep('AG,AU,B1G,B1U,B2G,B2U,B3G,B3U,',8)
      return
      
      elseif(N.EQ.3)then
      call puttbl(Nsave+1,nd3h,d3htbl)
      call labrep('A1'',A1",A2'',A2",E'',E",',6)
      return
      
      elseif(N.EQ.4)then
      call puttbl(Nsave+1,nd4h,d4htbl)
      call labrep('A1G,A1U,A2G,A2U,B1G,B1U,B2G,B2U,EG,EU,',10)
      return
      
      elseif(N.EQ.5)then
      pi=four*gatan(one)
      d5htbl(2,5)=two*gcos(f72*pi/f180)
      d5htbl(2,6)=two*gcos(f72*pi/f180)
      d5htbl(2,7)=two*gcos(f144*pi/f180)
      d5htbl(2,8)=two*gcos(f144*pi/f180)
      call puttbl(Nsave+1,nd5h,d5htbl)
      call labrep('A1'',A1",A2'',A2",E1'',E1",E2'',E2",',8)
      return
      
      elseif(N.EQ.6)then
      call puttbl(Nsave+1,nd6h,d6htbl)
      call labrep('A1G,A1U,A2G,A2U,B1G,B1U,B2G,B2U,E1G,E1U,E2G,E2U,',12)
      return
      endif
      
      call puttbl(Nsave+1,0,0)
      return
      
      end
C* :1 * 
      
