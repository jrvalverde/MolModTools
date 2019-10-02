
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 traopn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "traopn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "traopn.web"
      double precision function traopn(A,B,N)
      implicit none
      double precision A,B,two,zero
      integer i,j,k,l,N
      dimension A(*),B(*)
      data zero/0.0D0/,two/2.0D0/
      
      
      
      
      
      traopn=zero
      k=0
      do 100 l=1,2
      do 50 j=1,N
      do 20 i=1,j
      k=k+1
      traopn=traopn+two*A(k)*B(k)
20    continue
      traopn=traopn-A(k)*B(k)
50    continue
100   continue
      return
      
      end
C* :1 * 
      
