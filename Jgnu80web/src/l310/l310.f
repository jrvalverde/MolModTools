
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 l310"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "l310.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "l310.web"
      subroutine l310(NCHAIN)
      implicit none
      integer i,Iop,Iop3,jump,NCHAIN,nextov
      
      double precision d(1),f(1)
      common/iop/Iop(50)
      common/iop3/Iop3(50)
      do 100 i=1,50
      Iop3(i)=Iop(i)
100   continue
      jump=0
      if(Iop(45).EQ.0)call genl2e(d,f,Iop,jump)
      NCHAIN=nextov(jump)
      return
      
      end
C* :1 * 
      
