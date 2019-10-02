
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fesnao"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fesnao.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "fesnao.web"
      subroutine fesnao(S)
      implicit none
      integer Ispin,l2,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,nfile
      double precision S
      
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      
      dimension S(Ndim,Ndim)
      
      
      nfile=11
      l2=Ndim*(Ndim+1)/2
      call nbread(S,l2,nfile)
      call unpack(S,Ndim,Nbas,l2)
      return
      end
C* :1 * 
      
