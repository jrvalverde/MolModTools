
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 vibovl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "vibovl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "vibovl.web"
      subroutine vibovl(A,B,AA,NAT3,NVIB,AMASS)
      implicit none
      integer i,j,k,kat,NAT3,NVIB
      double precision A(NAT3,NVIB),B(NAT3,NVIB),AA(NVIB),AMASS(36)
      double precision zero,t
      data zero/0.0D0/
      
      do 200 i=1,NVIB
      do 50 j=1,NVIB
      AA(j)=zero
      do 20 k=1,NAT3
      t=A(k,j)*B(k,i)
      kat=(k-1)/3+1
      if(j.NE.i)t=t*AMASS(kat)
      AA(j)=AA(j)+t
20    continue
50    continue
      
      do 100 j=1,NVIB
      B(j,i)=AA(j)
100   continue
200   continue
      return
      
      end
C* :1 * 
      
