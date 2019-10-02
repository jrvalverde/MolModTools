
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 simtrm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "simtrm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "simtrm.web"
      
      
      
      subroutine simtrm(A,S,V,NDIM,N,IWMULP,IWCUBF)
      implicit none
      double precision A,S,V
      integer i1,IWCUBF,IWMULP,N,NDIM
      
      
      dimension A(NDIM,NDIM),S(NDIM,NDIM),V(1)
      call matmlt(A,S,V,NDIM,N)
      i1=NDIM+1
      if(IWMULP.NE.0)call mulana(A,V(1),V(i1),IWMULP,IWCUBF)
      call matml2(S,A,V,NDIM,N)
      return
      end
C* :1 * 
      
