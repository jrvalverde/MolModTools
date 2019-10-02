
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repcnv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repcnv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "repcnv.web"
      subroutine repcnv(N)
      implicit none
      double precision f144,f180,f72,four,gatan,gcos,one,pi,two
      integer Icnt,Isave,N,nc2v,nc3v,nc4v,nc5v,nc6v,Nsave
      double precision c2vtbl(3,4),c3vtbl(3,3)
      double precision c4vtbl(3,5),c6vtbl(3,6)
      double precision c5vtbl(3,4)
      common/reploc/Nsave,Icnt,Isave(15)
      data nc2v/4/
      data c2vtbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,1.0,0.0,-1.0,-1.0/
      data nc3v/3/
      data c3vtbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,0.0/
      data nc4v/5/
      data c4vtbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,1.0,0.0,-1.0,-1.0,0.
     &0,0.0,0.0/
      data nc5v/4/
      data c5vtbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,0.0,0.0,0.0,0.0,0.0/
      data nc6v/6/
      data c6vtbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,-1.0,1.0,0.0,-1.0,-1.0,0.
     &0,1.0,0.0,0.0,-1.0,0.0/
      data one/1.0D0/,two/2.0D0/,four/4.0D0/
      data f72/72.0D0/,f144/144.0D0/,f180/180.0D0/
      
      
      
      
      
      
      
      
      
      
      Icnt=0
      Nsave=2
      Isave(1)=1
      Isave(2)=N+1
      
      if(N.EQ.2)then
      call puttbl(Nsave+1,nc2v,c2vtbl)
      call labrep('A1,A2,B1,B2,',4)
      return
      
      elseif(N.EQ.3)then
      call puttbl(Nsave+1,nc3v,c3vtbl)
      call labrep('A1,A2,E,',3)
      return
      
      elseif(N.EQ.4)then
      call puttbl(Nsave+1,nc4v,c4vtbl)
      call labrep('A1,A2,B1,B2,E,',5)
      return
      
      elseif(N.EQ.5)then
      pi=four*gatan(one)
      c5vtbl(2,3)=two*gcos(f72*pi/f180)
      c5vtbl(2,4)=two*gcos(f144*pi/f180)
      call puttbl(Nsave+1,nc5v,c5vtbl)
      call labrep('A1,A2,E1,E2,',4)
      return
      
      elseif(N.EQ.6)then
      call puttbl(Nsave+1,nc6v,c6vtbl)
      call labrep('A1,A2,B1,B2,E1,E2,',6)
      return
      endif
      
      call puttbl(Nsave+1,0,0)
      return
      
      end
C* :1 * 
      
