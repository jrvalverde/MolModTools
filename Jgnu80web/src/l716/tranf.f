
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tranf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tranf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "tranf.web"
      subroutine tranf(NPARM,NZ,IANZ,FX,F,IB,B,G,LL)
      implicit none
      double precision B,F,FX,G,r,zero
      integer i,IANZ,IB,ii,j,k,k1,l,LL,NPARM,NZ
      dimension IANZ(*),FX(*),F(*),IB(4,NPARM),B(3,4,NPARM),G(*),LL(*)
      data zero/0.D0/
      
      
      
      
      
      
      
      do 100 i=1,NPARM
      F(i)=zero
100   continue
      ii=1
      do 200 i=1,NZ
      LL(i)=0
      if(IANZ(i).NE.-1)then
      LL(i)=ii
      ii=ii+1
      endif
200   continue
      do 400 i=1,NPARM
      ii=NPARM*(i-1)
      r=zero
      do 250 k1=1,4
      k=IB(k1,i)
      if(k.EQ.0)goto 300
      k=LL(k)
      if(k.NE.0)then
      k=3*(k-1)
      do 210 l=1,3
      r=r+B(l,k1,i)*FX(k+l)
210   continue
      endif
250   continue
300   do 350 j=1,NPARM
      F(j)=F(j)+G(ii+j)*r
350   continue
400   continue
      
      return
      
      end
C* :1 * 
      
