
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dout"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dout.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "dout.web"
      subroutine dout(X,LB,LA)
      implicit none
      integer i,In,Iout,Ipunch,j,LA,LB
      real X
      dimension X(4,8)
      common/io/In,Iout,Ipunch
      
      
      
99001 format(2x,8F12.5)
      
      do 100 i=1,LB
      write(Iout,99001)(X(i,j),j=1,LA)
100   continue
      return
      
      end
C* :1 * 
      
