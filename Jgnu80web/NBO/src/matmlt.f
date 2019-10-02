
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 matmlt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "matmlt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "matmlt.web"
      subroutine matmlt(A,B,V,NDIM,N)
      implicit none
      double precision A,B,temp,V,zero
      integer i,ij,ik,ikk,j,k,kj,N,ndif,NDIM
      dimension A(1),B(1),V(NDIM)
      data zero/0.0D0/
      
      
      ndif=NDIM-N
      do 200 i=1,N
      kj=0
      ikk=i-NDIM
      do 50 j=1,N
      ik=ikk
      temp=zero
      do 20 k=1,N
      ik=ik+NDIM
      kj=kj+1
      temp=temp+A(ik)*B(kj)
20    continue
      kj=kj+ndif
      V(j)=temp
50    continue
      ij=i-NDIM
      do 100 j=1,N
      ij=ij+NDIM
      A(ij)=V(j)
100   continue
200   continue
      return
      end
C* :1 * 
      
