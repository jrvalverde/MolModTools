
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 limtrn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "limtrn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "limtrn.web"
      subroutine limtrn(T,M,A,B,NDIM,NBAS,NCDIM,NC,IOPT)
      implicit none
      double precision A,B,sum,T
      integer i,IOPT,j,k,M,NBAS,NC,NCDIM,NDIM
      dimension T(NDIM,NDIM),M(NCDIM),A(NCDIM,NCDIM),B(NCDIM)
      
      
      
      if(IOPT.NE.1)then
      do 50 j=1,NBAS
      do 20 k=1,NC
      B(k)=T(M(k),j)
20    continue
      do 40 i=1,NC
      sum=0.0D0
      do 30 k=1,NC
      sum=sum+A(k,i)*B(k)
30    continue
      T(M(i),j)=sum
40    continue
50    continue
      if(IOPT.EQ.-1)return
      endif
      do 200 i=1,NBAS
      do 100 k=1,NC
      B(k)=T(i,M(k))
100   continue
      do 150 j=1,NC
      sum=0.0D0
      do 120 k=1,NC
      sum=sum+B(k)*A(k,j)
120   continue
      T(i,M(j))=sum
150   continue
200   continue
      return
      end
C* :1 * 
      
