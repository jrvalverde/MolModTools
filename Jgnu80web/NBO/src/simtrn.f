
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 simtrn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "simtrn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "simtrn.web"
      subroutine simtrn(A,T,V,NDIM,N)
      implicit none
      double precision A,T,V
      integer N,NDIM
      
      
      dimension A(NDIM,NDIM),T(NDIM,NDIM),V(NDIM)
      call matmlt(A,T,V,NDIM,N)
      call trnspo(A,NDIM,N)
      call matmlt(A,T,V,NDIM,N)
      call trnspo(A,NDIM,N)
      return
      end
C* :1 * 
      
