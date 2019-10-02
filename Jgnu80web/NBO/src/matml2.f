
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 matml2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "matml2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "matml2.web"
      subroutine matml2(A,B,V,NDIM,N)
      implicit none
      double precision A,B,temp,V,vv,zero
      integer i,ij,ijj,j,ji,jm1,k,ki,kii,kj,kjj,N,NDIM
      dimension A(1),B(1),V(NDIM)
      data zero/0.0D0/
      
      ij=0
      ijj=-NDIM
      kjj=-NDIM
      do 200 j=1,N
      kii=-NDIM
      kjj=kjj+NDIM
      do 50 i=1,j
      kii=kii+NDIM
      ki=kii
      kj=kjj
      temp=zero
      do 20 k=1,N
      ki=ki+1
      kj=kj+1
      temp=temp+A(ki)*B(kj)
20    continue
      V(i)=temp
50    continue
      ijj=ijj+NDIM
      ij=ijj
      ji=j-NDIM
      jm1=j-1
      do 100 i=1,jm1
      ij=ij+1
      ji=ji+NDIM
      vv=V(i)
      B(ij)=vv
      B(ji)=vv
100   continue
      ij=ij+1
      B(ij)=V(j)
200   continue
      return
      end
C* :1 * 
      
