
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 trnfrm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "trnfrm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "trnfrm.web"
      subroutine trnfrm(A,SYMOPS,IPRMUT,NB,NCOL,NATOMS,AA)
      implicit none
      double precision A,AA,SYMOPS
      integer i56d,IPRMUT,Mapper,Maprot,MAXATM,MAXBAS,NATOMS,NB,NCOL,Nro
     &t
      parameter(MAXBAS=150,MAXATM=100)
      double precision rotd(6,6),rotf(10,10)
      dimension A(*),SYMOPS(*),IPRMUT(*),AA(*)
      common/maps/Nrot,Maprot(MAXBAS),Mapper(MAXATM)
      call ilsw(2,2,i56d)
      call frmrot(SYMOPS,rotd,rotf,3,i56d)
      call dorot(SYMOPS,rotd,rotf,A,NB,NCOL,Maprot,Nrot)
      call permut(A,AA,Mapper,IPRMUT,NATOMS,NB,NCOL)
      return
      
      end
C* :1 * 
      
