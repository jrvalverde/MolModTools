
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 pack"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "pack.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "pack.web"
      subroutine pack(T,NDIM,NBAS,L2)
      implicit none
      integer i,ii,iptr,j,jptr,L2,NBAS,NDIM
      double precision T,zero
      dimension T(1)
      
      data zero/0.0D0/
      
      
      if(NBAS.GT.NDIM)stop 'NBAS IS GREATER THAN NDIM'
      ii=0
      do 100 j=1,NBAS
      jptr=(j-1)*NDIM
      do 50 i=1,j
      iptr=jptr+i
      ii=ii+1
      T(ii)=T(iptr)
50    continue
100   continue
      if(ii.NE.L2)stop 'ERROR IN ROUTINE PACK'
      
      do 200 i=ii+1,NDIM*NDIM
      T(i)=zero
200   continue
      return
      end
C* :1 * 
      
