
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 elim"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "elim.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "elim.web"
      subroutine elim
      implicit none
      integer i,Ic,j
      common/shdups/Ic(256,5)
      
      
      do 100 j=1,5
      do 50 i=1,256
      Ic(i,j)=1
50    continue
100   continue
      
      call elimkl(Ic(1,1))
      
      call elimij(Ic(1,2))
      
      call elimij(Ic(1,3))
      call elimkl(Ic(1,3))
      
      call elimik(Ic(1,4))
      
      call elimij(Ic(1,5))
      call elimkl(Ic(1,5))
      call elimik(Ic(1,5))
      
      
      return
      
      end
C* :1 * 
      
