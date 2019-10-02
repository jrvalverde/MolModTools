
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fixrep"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fixrep.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "fixrep.web"
      subroutine fixrep(T)
      implicit none
      integer i,imin,iop,irwrep,j,lrwrep
      double precision T(3,3),c(9),temp
      double precision Symops,Chrtbl
      integer Nsymop,Nreps,Lblrep,Iprmut
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      data irwrep/562/,lrwrep/767/
      
      if(Nsymop.EQ.0)return
      call tread(irwrep,Nsymop,lrwrep,1,lrwrep,1,0)
      
      do 100 iop=1,Nsymop
      call mul3x3(Symops(1,iop),T,c)
      do 50 i=1,9
      Symops(i,iop)=c(i)
50    continue
100   continue
      
      
      do 200 i=2,3
      imin=i-1
      do 150 j=1,imin
      temp=T(i,j)
      T(i,j)=T(j,i)
      T(j,i)=temp
150   continue
200   continue
      
      
      do 300 iop=1,Nsymop
      call mul3x3(T,Symops(1,iop),c)
      do 250 i=1,9
      Symops(i,iop)=c(i)
250   continue
300   continue
      call twrite(irwrep,Nsymop,lrwrep,1,lrwrep,1,0)
      
      return
      
      end
C* :1 * 
      
