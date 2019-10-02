
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 vdagt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "vdagt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "vdagt.web"
      subroutine vdagt(N,V,T,F,S)
      implicit none
      double precision F,S,sum,T,V,zero
      integer i,ijf,ijt,ijv,irow,jcol,k,N
      dimension V(*),T(*),F(*),S(*)
      data zero/0.0D0/
      
      
      
      
      
      
      
      
      ijt=0
      ijf=0
      
      
      do 200 irow=1,N
      
      do 50 i=1,N
      ijt=ijt+1
      S(i)=T(ijt)
50    continue
      
      ijv=0
      do 100 jcol=1,irow
      
      sum=zero
      do 60 k=1,N
      ijv=ijv+1
      sum=sum+S(k)*V(ijv)
60    continue
      
      ijf=ijf+1
      F(ijf)=sum
100   continue
200   continue
      
      
      return
      
      end
C* :1 * 
      
