
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 permut"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "permut.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "permut.web"
      subroutine permut(A,VEC,MAP,IPRMUT,NATOMS,NB,NCOL)
      implicit none
      integer i,iat,icol,iend,int,IPRMUT,ist,MAP,NATOMS,NB,NCOL
      double precision A(NB,NB),VEC(NB)
      dimension MAP(*),IPRMUT(*)
      
      MAP(NATOMS+1)=NB+1
      do 200 icol=1,NCOL
      do 50 iat=1,NATOMS
      ist=MAP(iat)
      iend=MAP(iat+1)-1
      int=IPRMUT(iat)
      int=MAP(int)-1
      do 20 i=ist,iend
      int=int+1
      VEC(int)=A(i,icol)
20    continue
50    continue
      
      do 100 i=1,NB
      A(i,icol)=VEC(i)
100   continue
200   continue
      return
      
      end
C* :1 * 
      
