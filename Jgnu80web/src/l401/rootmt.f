
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rootmt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rootmt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "rootmt.web"
      subroutine rootmt(A,B,AA,BB,MDIM,NBAS,INV)
      implicit none
      double precision A,AA,B,BB,gsqrt,one,term
      integer i,In,INV,Iout,Ipunch,j,Jdump,Jjdump,MDIM,NBAS
      dimension A(MDIM,MDIM),B(MDIM,MDIM),AA(MDIM),BB(MDIM)
      common/io/In,Iout,Ipunch
      common/dump/Jdump,Jjdump
      data one/1.0D0/
      
      
      
      if(Jdump.NE.0)write(Iout,99001)
      
99001 format('  NVSQRT')
      
      call diag(NBAS,MDIM,A,B,AA,BB)
      
      do 100 i=1,NBAS
      if(AA(i).LE.0)call geserr(7)
      term=gsqrt(AA(i))
      if(INV.NE.0)term=one/term
      
      do 50 j=1,NBAS
      A(j,i)=B(j,i)*term
50    continue
100   continue
      
      call matrec(A,B,BB,MDIM,NBAS,NBAS,NBAS,3)
      return
      
      end
C* :1 * 
      
