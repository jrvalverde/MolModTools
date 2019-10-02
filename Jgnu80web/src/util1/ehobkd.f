
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ehobkd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ehobkd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "ehobkd.web"
      subroutine ehobkd(A,N,M1,M2,Z,IZ)
      implicit none
      double precision A,h,s,Z,zero
      integer i,ia,IZ,j,k,l,M1,M2,N
      dimension A(*),Z(IZ,*)
      data zero/0.D0/
      
      
      
      if(N.NE.1)then
      do 50 i=2,N
      l=i-1
      ia=i*l/2
      h=A(ia+i)
      if(h.NE.0)then
      do 20 j=M1,M2
      s=zero
      do 5 k=1,l
      s=s+A(ia+k)*Z(k,j)
5     continue
      s=s/h
      do 10 k=1,l
      Z(k,j)=Z(k,j)-s*A(ia+k)
10    continue
20    continue
      endif
50    continue
      endif
      return
      
      end
C* :1 * 
      
