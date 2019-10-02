
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getmo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getmo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "getmo.web"
      subroutine getmo(A,B,AA,BB,MD,NB,IOV)
      implicit none
      double precision A,AA,B,BB
      integer IOV,MD,NB
      dimension A(*),B(*),AA(*),BB(*)
      
      
      call tread(IOV,B(1),MD,MD,NB,NB,0)
      call matrec(B(1),A(1),AA(1),MD,NB,NB,NB,4)
      call matrec(A(1),B(1),AA(1),MD,NB,NB,NB,1)
      
      call diag(NB,MD,A(1),B(1),AA(1),BB(1))
      
      call tread(IOV,A(1),MD,MD,NB,NB,0)
      call matrec(A(1),B(1),BB(1),MD,NB,NB,NB,1)
      return
      
      end
C* :1 * 
      
