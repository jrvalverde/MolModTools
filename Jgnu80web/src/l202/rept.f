
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rept"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rept.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "rept.web"
      subroutine rept
      implicit none
      integer Icnt,Isave,Nsave
      double precision ttbl(3,3)
      common/reploc/Nsave,Icnt,Isave(15)
      data ttbl/0.0,1.0,1.0,0.0,-1.0,2.0,0.0,0.0,-1.0/
      
      
      
      
      Nsave=2
      Icnt=0
      Isave(1)=1
      Isave(2)=9
      call puttbl(3,3,ttbl)
      call labrep('A,E,T,',3)
      return
      
      end
C* :1 * 
      
