
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 labrep"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "labrep.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "labrep.web"
      subroutine labrep(STRING,NUM)
      implicit none
      integer i,incur,len,NUM,outcur
      save
      double precision Symops,Chrtbl
      integer Nsymop,Nreps,Lblrep,Iprmut
      character*1 tmp(10),STRING(*)
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      
      
      incur=0
      outcur=0
      
      do 100 i=1,NUM
      call getbc(1,tmp,len,STRING,incur)
      call putbc(tmp,len,Lblrep,outcur)
      call putdel(2,Lblrep,outcur)
100   continue
      return
      
      end
C* :1 * 
      
