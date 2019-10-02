
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lsexa"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lsexa.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "lsexa.web"
      subroutine lsexa(V,IDIM)
      implicit none
      integer i,i1,i2,i3,i4,IDIM,ij,j,jr,mm,n1
      double precision V,zero
      dimension V(*)
      data zero/0.D0/
      
      
      
      
      
      
      call track('LSEXA ')
      
      n1=IDIM-1
      if(n1.LT.0)goto 300
      if(n1.NE.0)then
      
      mm=IDIM*(IDIM-1)
      i2=mm-IDIM
      mm=mm/2
      do 50 i=2,IDIM
      i1=i-1
      do 20 j=1,i1
      jr=IDIM-j+1
      ij=i2+jr
      V(ij)=V(mm)
      mm=mm-1
20    continue
      i2=i2-IDIM
50    continue
      i3=0
      do 100 i=1,n1
      i1=i+1
      i4=i3+i+IDIM
      do 60 j=i1,IDIM
      ij=i3+j
      V(i4)=-V(i3+j)
      i4=i4+IDIM
60    continue
      i3=i3+IDIM
100   continue
      endif
      i2=0
      do 200 i=1,IDIM
      V(i2+i)=zero
      i2=i2+IDIM
200   continue
      
300   return
      
      end
C* :1 * 
      
