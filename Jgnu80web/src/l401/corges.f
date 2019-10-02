
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 corges"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "corges.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "corges.web"
      subroutine corges(A,B,AA,BB,NB,IOCORE,IOV)
      implicit none
      double precision A,AA,B,BB
      integer IOCORE,IOV,NB
      dimension A(*),B(*),AA(*),BB(*)
      
      
      call tread(IOCORE,A,NB,NB,NB,NB,1)
      call getmo(A,B,AA,BB,NB,NB,IOV)
      return
      
      end
C* :1 * 
      
