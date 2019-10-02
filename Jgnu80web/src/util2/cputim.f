
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cputim"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cputim.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "cputim.web"
      
      real function cputim(I)
      implicit none
      integer I,j,l
      call timeo(l,j)
      cputim=0.001*j
      return
      
      end
C* :1 * 
      
