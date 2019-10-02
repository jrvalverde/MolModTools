
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 loadav"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "loadav.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "loadav.web"
      
      
      
      subroutine loadav(LISTAO,NL,M,S,NDIM,A,B,MXAOLM)
      implicit none
      double precision A,ave,B,one,S,sum,zero
      integer i,iao,im,j,jao,LISTAO,M,MXAOLM,NDIM,NL
      dimension S(NDIM,NDIM),LISTAO(MXAOLM,9),A(NL,NL),B(NL,NL)
      data one,zero/1.0D0,0.0D0/
      
      
      do 100 j=1,NL
      do 50 i=1,j
      sum=zero
      do 20 im=1,M
      iao=LISTAO(i,im)
      jao=LISTAO(j,im)
      sum=sum+S(iao,jao)
20    continue
      ave=sum/M
      A(i,j)=ave
      A(j,i)=ave
      B(i,j)=S(jao,iao)
      B(j,i)=B(i,j)
50    continue
      B(j,j)=one
100   continue
      return
      end
C* :1 * 
      
