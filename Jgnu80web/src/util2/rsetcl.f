
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rsetcl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rsetcl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "rsetcl.web"
      subroutine rsetcl(NBASIS,PAR,PAI,COMPLX)
      implicit none
      integer i,j,k,NBASIS
      double precision PAI,PAR,pt5
      logical COMPLX
      dimension PAR(*),PAI(*)
      data pt5/0.5D0/
      
      
      
      
      
      
      
      k=0
      if(COMPLX)then
      
      do 50 i=1,NBASIS
      do 20 j=1,i
      k=k+1
      PAR(k)=PAR(k)+PAR(k)
      PAI(k)=PAI(k)+PAI(k)
20    continue
      PAR(k)=pt5*PAR(k)
      PAI(k)=pt5*PAI(k)
50    continue
      else
      
      do 100 i=1,NBASIS
      do 60 j=1,i
      k=k+1
      PAR(k)=PAR(k)+PAR(k)
60    continue
      PAR(k)=pt5*PAR(k)
100   continue
      endif
      
      return
      
      end
C* :1 * 
      
