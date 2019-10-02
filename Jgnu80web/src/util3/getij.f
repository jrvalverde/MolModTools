
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getij"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getij.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "getij.web"
      subroutine getij(N,I,J)
      implicit none
      double precision fpi,gfloat,gsqrt,one,pt5,thr1
      integer I,J,N
      data thr1/0.001/,pt5/0.5/,one/1.0/
      
      
      
      fpi=pt5*(one+gsqrt(one+gfloat(8*N)))
      I=idint(fpi+thr1)
      J=N-((I*(I-1))/2)
      if(J.LE.0)then
      I=I-1
      J=N-((I*(I-1))/2)
      endif
      return
      
      end
C* :1 * 
      
