
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repcn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repcn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "repcn.web"
      subroutine repcn(N)
      implicit none
      double precision c2tbl,c3tbl,c6tbl
      integer Icnt,Isave,N,nc2,nc3,nc6,Nsave
      dimension c2tbl(2,2)
      dimension c3tbl(2,2)
      dimension c6tbl(3,4)
      common/reploc/Nsave,Icnt,Isave(15)
      data nc2/2/
      data c2tbl/0.0,1.0,0.0,-1.0/
      data nc3/2/
      data c3tbl/0.0,1.0,0.0,-1.0/
      data nc6/4/
      data c6tbl/0.0,1.0,1.0,0.0,-1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,-1.0/
      
      
      
      
      
      
      
      
      Icnt=0
      Nsave=1
      if(N.GT.3)Nsave=2
      Isave(1)=1
      Isave(2)=2
      
      if(N.EQ.2)then
      call puttbl(Nsave+1,nc2,c2tbl)
      call labrep('A,B,',2)
      return
      
      elseif(N.EQ.3)then
      call puttbl(Nsave+1,nc3,c3tbl)
      call labrep('A,E,',2)
      return
      
      elseif(N.EQ.6)then
      call puttbl(Nsave+1,nc6,c6tbl)
      call labrep('A,B,E1,E2,',4)
      return
      endif
      
      call puttbl(Nsave+1,0,0)
      return
      
      end
C* :1 * 
      
