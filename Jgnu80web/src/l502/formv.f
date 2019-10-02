
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 formv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "formv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "formv.web"
      subroutine formv(NBASIS,MAXNBF,V,D,VV,DD)
      implicit none
      double precision D,DD,fact,gsqrt,one,V,VV
      integer i,irws,irwtm,j,MAXNBF,NBASIS
      dimension V(MAXNBF,MAXNBF),D(MAXNBF,MAXNBF),VV(*),DD(*)
      data one/1.0D0/
      data irws/514/,irwtm/2555/
      
      
      
      
      call tread(irws,V,MAXNBF,MAXNBF,NBASIS,NBASIS,1)
      call diag(NBASIS,MAXNBF,V,D,VV,DD)
      do 100 j=1,NBASIS
      fact=one/gsqrt(VV(j))
      do 50 i=1,NBASIS
      D(i,j)=D(i,j)*fact
50    continue
100   continue
      do 200 i=1,NBASIS
      do 150 j=1,NBASIS
      V(i,j)=D(j,i)
150   continue
200   continue
      call twrite(irwtm,V,MAXNBF,MAXNBF,NBASIS,NBASIS,0)
      return
      
      end
C* :1 * 
      
