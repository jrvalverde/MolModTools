
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fenlmo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fenlmo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "fenlmo.web"
      subroutine fenlmo(T)
      implicit none
      integer Ispin,l3,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,nfile
      double precision T
      
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      dimension T(Ndim,Ndim)
      
      
      nfile=46
      l3=Ndim*Ndim
      call nbread(T,l3,nfile)
      return
      end
C* :1 * 
      
