
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lsexs"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lsexs.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "lsexs.web"
      subroutine lsexs(A,N)
      implicit none
      double precision A
      integer i,i1,i2,j,j1,mm,N
      dimension A(*)
      
      
      
      
      
      mm=N*(N+1)/2
      
      i1=N**2
      i2=i1
      do 100 i=1,N
      j1=i1+i2
      do 50 j=i2,i1
      A(j1-j)=A(mm)
      mm=mm-1
50    continue
      i1=i1-N
      i2=i1-i
100   continue
      
      i1=0
      do 200 i=1,N
      j1=i+i1
      do 150 j=i,N
      A(j1)=A(i1+j)
      j1=j1+N
150   continue
      i1=i1+N
200   continue
      
      return
      
      end
C* :1 * 
      
