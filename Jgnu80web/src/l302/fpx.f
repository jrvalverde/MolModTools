
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fpx"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fpx.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "fpx.web"
      subroutine fpx(NDEG,NFD,A,AP)
      implicit none
      double precision A,AP,gfloat
      integer i,k,NDEG,NFD
      dimension A(*),AP(*)
      
      NFD=NDEG-1
      k=NDEG
      do 100 i=1,NDEG
      AP(i)=A(i)*gfloat(k)
      k=k-1
100   continue
      return
      
      end
C* :1 * 
      
