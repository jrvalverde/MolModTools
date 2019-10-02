
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 simltr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "simltr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "simltr.web"
      subroutine simltr(N,NDIM,F,U,R,S,KNTROL)
      implicit none
      double precision F,R,S,sum,U
      integer i,ij,in,j,jk,jn,k,kn,KNTROL,N,NDIM,nt
      dimension F(1),U(NDIM,1),S(1),R(1)
      
      in=0
      do 200 i=1,N
      jn=0
      do 50 j=1,N
      sum=0.
      kn=0
      do 20 k=1,N
      jk=jn+k
      if(j.LT.k)jk=kn+j
      sum=sum+F(jk)*U(k,i)
      kn=kn+k
20    continue
      S(j)=sum
      jn=jn+j
50    continue
      do 100 j=1,i
      sum=0.
      do 60 k=1,N
      sum=sum+S(k)*U(k,j)
60    continue
      ij=in+j
      R(ij)=sum
100   continue
      in=in+i
200   continue
      if(KNTROL.EQ.0)return
      nt=N*(N+1)/2
      do 300 i=1,nt
      F(i)=R(i)
300   continue
      return
      end
C* :1 * 
      
