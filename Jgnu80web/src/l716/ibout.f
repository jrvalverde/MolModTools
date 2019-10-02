
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ibout"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ibout.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "ibout.web"
      subroutine ibout(IB,NMAX,MMAX,NP,MP)
      implicit none
      integer i,IB,ii,In,Iout,Ipunch,j,jj,l1,l2,MMAX,MP,NMAX,NP
      dimension IB(NMAX,MMAX)
      common/io/In,Iout,Ipunch
      
      ii=1+MP/10
      do 100 i=1,ii
      l1=(i-1)*10+1
      l2=l1+9
      if(l2.GT.MP)l2=MP
      write(Iout,99001)(j,j=l1,l2)
      do 50 j=1,NP
      write(Iout,99002)j,(IB(j,jj),jj=l1,l2)
50    continue
100   continue
      
99001 format('0',4x,10I10)
99002 format(' ',i5,10I10)
      
      return
      
      end
C* :1 * 
      
