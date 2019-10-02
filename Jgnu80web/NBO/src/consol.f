
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 consol"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "consol.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "consol.web"
      subroutine consol(AUT,ALT,NDIM,N)
      implicit none
      double precision ALT,AUT
      integer i,j,jp1,N,NDIM,nm1
      
      
      dimension AUT(NDIM,NDIM),ALT(NDIM,NDIM)
      nm1=N-1
      do 100 j=1,nm1
      jp1=j+1
      do 50 i=jp1,N
      AUT(i,j)=ALT(i,j)
50    continue
100   continue
      return
      end
C* :1 * 
      
