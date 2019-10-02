
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rotff"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rotff.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "rotff.web"
      subroutine rotff(NATOMS,TR,FFXIN,FFXOUT)
      implicit none
      double precision FFXIN,FFXOUT,t,t1,TR,x,zero
      integer i,iat,j,jat,k,ki,l,lj,loc,m,NATOMS
      dimension FFXIN(*),TR(3,3),FFXOUT(*)
      dimension t(3,3),t1(3,3)
      data zero/0.0D0/
      
      
      
      
      
      do 200 iat=1,NATOMS
      i=(iat-1)*3
      do 100 jat=1,iat
      j=(jat-1)*3
      
      do 20 k=1,3
      do 10 l=1,3
      ki=k+i
      lj=l+j
      loc=(ki*(ki-1))/2+lj
      if(lj.GT.ki)loc=(lj*(lj-1))/2+ki
      t(k,l)=FFXIN(loc)
10    continue
20    continue
      
      do 40 k=1,3
      do 30 l=1,3
      x=zero
      do 25 m=1,3
      x=x+TR(m,l)*t(k,m)
25    continue
      t1(k,l)=x
30    continue
40    continue
      do 60 l=1,3
      do 50 k=1,3
      x=zero
      do 45 m=1,3
      x=x+TR(m,k)*t1(m,l)
45    continue
      t(k,l)=x
50    continue
60    continue
      
      do 80 k=1,3
      do 70 l=1,3
      ki=k+i
      lj=l+j
      loc=(ki*(ki-1))/2+lj
      if(lj.GT.ki)loc=(lj*(lj-1))/2+ki
      FFXOUT(loc)=t(k,l)
70    continue
80    continue
100   continue
200   continue
      
      return
      
      end
C* :1 * 
      
