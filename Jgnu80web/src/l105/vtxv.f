
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 vtxv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "vtxv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "vtxv.web"
      subroutine vtxv(VT,V,DOT,NMAX,N)
      implicit none
      double precision DOT,V,VT,zero
      integer i,N,NMAX
      dimension VT(NMAX),V(NMAX)
      data zero/0.0D0/
      
      
      
      
      
      DOT=zero
      do 100 i=1,N
      DOT=DOT+VT(i)*V(i)
100   continue
      return
      
      end
C* :1 * 
      
