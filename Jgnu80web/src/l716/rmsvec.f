
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rmsvec"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rmsvec.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "rmsvec.web"
      subroutine rmsvec(N,V,VRMS,VMAX)
      implicit none
      double precision gabs,gfloat,gsqrt,V,VMAX,VRMS,zero
      integer i,N
      dimension V(*)
      data zero/0.0D0/
      
      
      
      
      
      VMAX=zero
      VRMS=zero
      
      do 100 i=1,N
      if(gabs(V(i)).GT.VMAX)VMAX=gabs(V(i))
      VRMS=VRMS+(V(i)**2)
100   continue
      VRMS=gsqrt(VRMS/gfloat(N))
      
      return
      
      end
C* :1 * 
      
