
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rank"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rank.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "rank.web"
      subroutine rank(EIG,N,NDIM,ARCRNK)
      implicit none
      double precision differ,EIG,temp
      integer i,i1,itemp,j,N,NDIM
      
      
      integer ARCRNK
      dimension ARCRNK(NDIM),EIG(NDIM)
      data differ/5.0D-8/
      do 100 i=1,N
      ARCRNK(i)=i
100   continue
      do 200 i=1,N
      if(i.NE.N)then
      i1=i+1
      do 120 j=i1,N
      if((EIG(j)-EIG(i)).GE.differ)then
      temp=EIG(i)
      EIG(i)=EIG(j)
      EIG(j)=temp
      itemp=ARCRNK(i)
      ARCRNK(i)=ARCRNK(j)
      ARCRNK(j)=itemp
      endif
120   continue
      endif
200   continue
      return
      end
C* :1 * 
      
