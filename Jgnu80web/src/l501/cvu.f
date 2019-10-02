
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cvu"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cvu.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "cvu.web"
      subroutine cvu(N,V,U,S)
      implicit none
      integer icol,iju,indv,irow,jcol,k,N
      double precision S,sum,U,V,zero
      dimension V(*),U(*),S(*)
      data zero/0.0D0/
      
      
      
      
      
      
      
      
      do 200 irow=1,N
      
      indv=irow
      do 50 icol=1,N
      S(icol)=V(indv)
      indv=indv+N
50    continue
      
      indv=irow
      iju=0
      do 100 jcol=1,N
      
      sum=zero
      do 60 k=1,N
      iju=iju+1
      sum=sum+S(k)*U(iju)
60    continue
      
      V(indv)=sum
      indv=indv+N
100   continue
200   continue
      
      return
      
      end
C* :1 * 
      
