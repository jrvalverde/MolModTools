
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repcnh"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repcnh.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "repcnh.web"
      subroutine repcnh(N)
      implicit none
      double precision c2htbl,c3htbl,c4htbl,c6htbl
      integer Icnt,Isave,N,nc2h,nc3h,nc4h,nc6h,Nsave
      dimension c2htbl(3,4)
      dimension c3htbl(3,4)
      dimension c4htbl(3,6)
      dimension c6htbl(3,8)
      common/reploc/Nsave,Icnt,Isave(15)
      data nc2h/4/
      data c2htbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,-1.0,0.0,-1.0,1.0/
      data nc3h/4/
      data c3htbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,2.0,0.0,-1.0,-2.0/
      data c4htbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,1.0,0.0,-1.0,-1.0,0.
     &0,-2.0,-2.0,0.0,-2.0,2.0/
      data nc6h/8/
      data c6htbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,-1.0,0.0,-1.0,1.0,0.
     &0,1.0,-2.0,0.0,1.0,2.0,0.0,-1.0,2.0,0.0,-1.0,-2.0/
      
      
      
      
      
      
      
      
      Icnt=0
      Nsave=2
      Isave(1)=1
      Isave(2)=N
      
      if(N.EQ.2)then
      call puttbl(Nsave+1,nc2h,c2htbl)
      call labrep('AG,AU,BG,BU,',4)
      return
      
      elseif(N.EQ.3)then
      call puttbl(Nsave+1,nc3h,c3htbl)
      call labrep('A'',A",E'',E",',4)
      return
      
      elseif(N.EQ.4)then
      call puttbl(Nsave+1,nc4h,c4htbl)
      call labrep('AG,AU,BG,BU,EG,EU,',6)
      return
      
      elseif(N.EQ.6)then
      call puttbl(Nsave+1,nc6h,c6htbl)
      call labrep('AG,AU,BG,BU,E1G,E1U,E2G,E2U,',8)
      return
      endif
      
      call puttbl(Nsave+1,0,0)
      return
      
      end
C* :1 * 
      
