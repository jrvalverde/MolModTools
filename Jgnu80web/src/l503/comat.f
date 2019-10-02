
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 comat"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "comat.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "comat.web"
      subroutine comat(A,B,MDIM,NDIM)
      implicit none
      double precision A,B
      integer i,j,MDIM,NDIM
      dimension A(MDIM,MDIM),B(NDIM,NDIM)
      
      do 100 j=1,NDIM
      do 50 i=1,NDIM
      B(i,j)=A(i,j)
50    continue
100   continue
      return
      
      end
C* :1 * 
      
