
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 shmdt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "shmdt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "shmdt.web"
      subroutine shmdt(T,S,NDIM,NBAS,NOCC,LSTOCC,NEMT,LSTEMT,SBLK)
      implicit none
      integer i,ip,j,jp,k,LSTEMT,LSTOCC,NBAS,NDIM,NEMT,NOCC
      double precision S,SBLK,sji,T,zero
      
      
      dimension T(NDIM,NDIM),S(NDIM,NDIM),LSTOCC(NDIM),LSTEMT(NDIM),SBLK
     &(NDIM,NDIM)
      data zero/0.0D0/
      do 100 i=1,NBAS
      do 50 j=1,NOCC
      jp=LSTOCC(j)
      sji=zero
      do 20 k=1,NBAS
      sji=sji+T(k,jp)*S(k,i)
20    continue
      SBLK(i,j)=sji
50    continue
100   continue
      do 200 i=1,NEMT
      ip=LSTEMT(i)
      do 150 j=1,NOCC
      jp=LSTOCC(j)
      sji=zero
      do 120 k=1,NBAS
      sji=sji+SBLK(k,j)*T(k,ip)
120   continue
      do 140 k=1,NBAS
      T(k,ip)=T(k,ip)-sji*T(k,jp)
140   continue
150   continue
200   continue
      return
      end
C* :1 * 
      
