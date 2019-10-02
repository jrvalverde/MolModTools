
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 axmxv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "axmxv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "axmxv.web"
      subroutine axmxv(R,CONST,M,V,NMAX,N)
      implicit none
      double precision CONST,M,R,V,zero
      integer i,j,N,NMAX
      dimension R(NMAX),V(NMAX),M(NMAX,NMAX)
      data zero/0.0D0/
      
      
      
      
      
      
      do 100 i=1,N
      R(i)=zero
      do 50 j=1,N
      if(i.GT.j)then
      
      R(i)=R(i)+CONST*M(j,i)*V(j)
      else
      R(i)=R(i)+CONST*M(i,j)*V(j)
      endif
50    continue
100   continue
      return
      
      end
C* :1 * 
      
