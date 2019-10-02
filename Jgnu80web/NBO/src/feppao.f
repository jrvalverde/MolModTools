
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 feppao"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "feppao.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "feppao.web"
      subroutine feppao(DM)
      implicit none
      double precision DM
      integer Ispin,l2,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,nfile
      
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      dimension DM(Ndim,Ndim)
      
      
      nfile=22
      l2=Ndim*(Ndim+1)/2
      call nbread(DM,l2,nfile)
      call unpack(DM,Ndim,Nbas,l2)
      return
      end
C* :1 * 
      
