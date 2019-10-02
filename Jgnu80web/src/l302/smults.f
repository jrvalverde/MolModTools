
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 smults"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "smults.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "smults.web"
      subroutine smults(MAXDIM,NBASIS,I1,I2,VALUE,V,S)
      implicit none
      integer i,I1,I2,j,MAXDIM,NBASIS
      double precision S,temp,V,VALUE,zero
      dimension V(MAXDIM,MAXDIM),S(MAXDIM,MAXDIM)
      data zero/0.0D0/
      
      
      
      VALUE=zero
      do 100 i=1,NBASIS
      temp=zero
      do 50 j=1,NBASIS
      temp=temp+S(i,j)*V(j,I2)
50    continue
      VALUE=VALUE+V(i,I1)*temp
100   continue
      
      return
      
      end
C* :1 * 
      
