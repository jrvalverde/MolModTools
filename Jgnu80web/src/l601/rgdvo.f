
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rgdvo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rgdvo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "rgdvo.web"
      subroutine rgdvo(IOP)
      implicit none
      integer i,Icmp,Ifcond,Ifdens,Ifdist,Ifgros,Ifmo,Ifoorc,Ifpop,Ifspi
     &n,IOP,Jop,Mnchrg,nops
      dimension IOP(*)
      dimension Jop(10)
      common/iopt/Ifoorc,Ifdist,Ifmo,Ifdens,Ifpop,Ifgros,Mnchrg,Ifcond,I
     &fspin,Icmp
      equivalence(Jop(1),Ifoorc)
      data nops/8/
      
      
      
      
      
      
      
      
      
      call ilsw(2,1,Ifoorc)
      Ifdist=0
      Ifmo=1
      Ifdens=0
      Ifpop=1
      Ifgros=1
      Ifcond=1
      
      
      do 100 i=1,nops
      if(IOP(i+4).LT.1)then
      elseif(IOP(i+4).EQ.1)then
      Jop(i)=1
      else
      
      Jop(i)=0
      endif
100   continue
      Icmp=Ifoorc/2
      Ifoorc=mod(Ifoorc,2)
      return
      
      end
C* :1 * 
      
