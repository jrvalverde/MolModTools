
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 puttbl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "puttbl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "puttbl.web"
      subroutine puttbl(NSYMP,NRP,CTBL)
      implicit none
      integer i,j,NRP,NSYMP
      double precision CTBL(NSYMP,*)
      double precision Symops,Chrtbl
      integer Nsymop,Nreps,Lblrep,Iprmut
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      
      Nreps=NRP
      Nsymop=NSYMP-1
      
      if(NRP.EQ.0)return
      do 100 i=1,NRP
      do 50 j=1,Nsymop
      Chrtbl(j,i)=CTBL(j+1,i)
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
