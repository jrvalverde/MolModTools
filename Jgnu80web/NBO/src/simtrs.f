
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 simtrs"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "simtrs.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "simtrs.web"
      subroutine simtrs(A,S,V,NDIM,N)
      implicit none
      double precision A,S,V
      integer N,NDIM
      
      
      dimension A(NDIM,NDIM),S(NDIM,NDIM),V(NDIM)
      call matmlt(A,S,V,NDIM,N)
      call matml2(S,A,V,NDIM,N)
      return
      end
C* :1 * 
      
