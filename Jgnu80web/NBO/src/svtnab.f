
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 svtnab"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "svtnab.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "svtnab.web"
      subroutine svtnab(T)
      implicit none
      integer Ispin,l3,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,nfile
      double precision T
      
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      dimension T(Ndim,Ndim)
      
      
      nfile=48
      l3=Ndim*Ndim
      call nbwrit(T,l3,nfile)
      return
      end
C* :1 * 
      
