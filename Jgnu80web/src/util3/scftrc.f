
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 scftrc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "scftrc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "scftrc.web"
      double precision function scftrc(A,B,N,L)
      implicit none
      double precision A,B,temp,zero
      integer i,j,k,L,m,N
      dimension A(*),B(*)
      data zero/0.0D0/
      
      
      
      
      k=0
      scftrc=zero
      do 100 m=1,L
      do 50 i=1,N
      do 20 j=1,i
      k=k+1
      temp=A(k)*B(k)
      scftrc=scftrc+temp+temp
20    continue
      scftrc=scftrc-temp
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
