
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 orderr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "orderr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "orderr.web"
      subroutine orderr(RANK,LIST,N,NDIM,ARCRNK)
      implicit none
      integer i,i1,j,k,LIST,N,NDIM
      
      
      integer RANK,ARCRNK,temp
      dimension RANK(NDIM),LIST(NDIM),ARCRNK(NDIM)
      do 100 i=1,N
      ARCRNK(i)=i
100   continue
      do 200 i=1,N
      if(i.NE.N)then
      i1=i+1
      do 120 j=i1,N
      if(LIST(j).LT.LIST(i))then
      temp=LIST(i)
      LIST(i)=LIST(j)
      LIST(j)=temp
      temp=ARCRNK(i)
      ARCRNK(i)=ARCRNK(j)
      ARCRNK(j)=temp
      endif
120   continue
      endif
      RANK(ARCRNK(i))=i
      if(LIST(i).LE.0)goto 300
200   continue
      return
300   do 400 k=i,N
      RANK(ARCRNK(k))=0
400   continue
      return
      end
C* :1 * 
      
