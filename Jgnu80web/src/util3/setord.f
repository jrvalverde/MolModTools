
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 setord"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "setord.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "setord.web"
      subroutine setord
      implicit none
      integer i,Ipurd,Ipurf,Lbound,N10ord,N5ord,N6ord,N7ord,Nordr
      integer Ubound,Ulpure
      common/ipure/Ipurd,Ipurf
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      
      
      
      
      
      if(Ipurd.NE.0)then
      
      do 50 i=1,10
      Nordr(i)=N6ord(i)
50    continue
      else
      do 100 i=1,9
      Nordr(i)=N5ord(i)
100   continue
      endif
      if(Ipurf.NE.0)then
      
      do 150 i=1,10
      Nordr(i+10)=N10ord(i)
150   continue
      else
      do 200 i=1,7
      Nordr(i+10)=N7ord(i)
200   continue
      endif
      return
      
      end
C* :1 * 
      
