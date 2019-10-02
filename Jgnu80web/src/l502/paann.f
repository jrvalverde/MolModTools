
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 paann"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "paann.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "paann.web"
      subroutine paann(NSQ,IRWV,IRW,ISAVE,A,B,FACTOR,FNORM)
      implicit none
      double precision A,B,fact,FACTOR,FNORM,zero
      integer i,IRW,IRWV,ISAVE,itemp,j,NSQ
      dimension A(*),B(*),FACTOR(*),IRWV(*),IRW(*)
      data zero/0.0D0/
      
      do 100 i=1,NSQ
      A(i)=zero
100   continue
      
      do 200 i=1,9
      itemp=IRWV(i)
      call tread(IRW(itemp),B,NSQ,1,NSQ,1,0)
      fact=FACTOR(i)
      do 150 j=1,NSQ
      A(j)=A(j)+fact*B(j)
150   continue
200   continue
      
      do 300 i=1,NSQ
      A(i)=A(i)*FNORM
300   continue
      call twrite(ISAVE,A,NSQ,1,NSQ,1,0)
      
      return
      
      end
C* :1 * 
      
