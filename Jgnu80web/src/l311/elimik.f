
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 elimik"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "elimik.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "elimik.web"
      subroutine elimik(IC)
      implicit none
      integer i,IC,ind,j,k,l
      dimension IC(*)
      
      ind=0
      do 100 i=1,4
      do 50 j=1,4
      do 20 k=1,4
      do 10 l=1,4
      ind=ind+1
      if(i.LT.k)then
      elseif(i.EQ.k)then
      if(j.GE.l)goto 10
      else
      goto 10
      endif
      IC(ind)=0
10    continue
20    continue
50    continue
100   continue
      return
      
      end
C* :1 * 
      
