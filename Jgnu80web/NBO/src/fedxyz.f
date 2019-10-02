
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fedxyz"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fedxyz.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "fedxyz.web"
      subroutine fedxyz(DXYZ,I)
      implicit none
      double precision DXYZ
      integer I,Ispin,l2,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,nfile,n
     &filex,nfiley,nfilez
      dimension DXYZ(1)
      
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      
      data nfilex,nfiley,nfilez/50,51,52/
      
      
      if(I.EQ.1)nfile=nfilex
      if(I.EQ.2)nfile=nfiley
      if(I.EQ.3)nfile=nfilez
      
      call nbinqr(nfile)
      if(nfile.GT.0)then
      l2=Ndim*(Ndim+1)/2
      call nbread(DXYZ,l2,nfile)
      call unpack(DXYZ,Ndim,Nbas,l2)
      else
      I=0
      endif
      return
      end
C* :1 * 
      
