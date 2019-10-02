
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 normch"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "normch.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "normch.web"
      subroutine normch(A,NB)
      implicit none
      double precision A,dmax,gabs,omax,one,term,zero
      integer i,In,Iout,Ipunch,j,NB
      dimension A(NB,NB)
      common/io/In,Iout,Ipunch
      data one,zero/1.0D0,0.0D0/
      
99001 format(30H TEST ORTHONORMALITY OF GUESS.,/,39H   THE LARGEST OFF-D
     &IAGONAL ELEMENT IS ,d20.10,/,47H   THE LARGEST DEVIATION FROM NORM
     &ALIZATION IS ,d20.10)
      
      omax=zero
      dmax=zero
      
      do 100 i=1,NB
      do 50 j=1,NB
      if(i.EQ.j)then
      
      term=gabs(one-A(i,j))
      if(term.GE.dmax)dmax=term
      else
      term=gabs(A(i,j))
      if(term.GE.omax)omax=term
      endif
50    continue
100   continue
      
      write(Iout,99001)omax,dmax
      return
      
      end
C* :1 * 
      
