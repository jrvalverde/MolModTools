
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 unpack"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "unpack.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "unpack.web"
      subroutine unpack(T,NDIM,NBAS,L2)
      implicit none
      integer i,icol,iptr,j,jptr,k,L2,NBAS,NDIM
      double precision T
      dimension T(1)
      
      
      
      j=0
      k=1
      iptr=(NDIM+1)*(NBAS-k)+1
      do 100 i=L2,1,-1
      T(iptr-j)=T(i)
      if(j.LT.NBAS-k)then
      j=j+1
      else
      j=0
      k=k+1
      iptr=(NDIM+1)*(NBAS-k)+1
      endif
100   continue
      
      
      do 200 j=1,NBAS-1
      icol=(j-1)*NDIM
      do 150 i=j+1,NBAS
      iptr=icol+i
      jptr=(i-1)*NDIM+j
      T(iptr)=T(jptr)
150   continue
200   continue
      
      return
      end
C* :1 * 
      
