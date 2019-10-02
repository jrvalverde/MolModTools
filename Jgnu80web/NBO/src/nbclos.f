
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nbclos"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nbclos.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "nbclos.web"
      subroutine nbclos
      implicit none
      integer Inbo,Ionbo,Nav,NBDAR,nfile,nx
      double precision X
      parameter(NBDAR=100)
      common/nbodaf/Inbo,Nav,Ionbo(NBDAR)
      dimension X(NBDAR/2+1)
      equivalence(X(1),Inbo)
      
      nfile=1
      nx=NBDAR/2+1
      call nbwrit(X,nx,nfile)
      close(unit=Inbo,status='KEEP')
      return
      end
C* :1 * 
      
