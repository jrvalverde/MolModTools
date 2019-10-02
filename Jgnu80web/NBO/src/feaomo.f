
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 feaomo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "feaomo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "feaomo.web"
      subroutine feaomo(T,IT)
      implicit none
      integer Ispin,IT,l3,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,nfile,
     &nfilea,nfileb
      double precision T
      dimension T(1)
      
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      
      data nfilea,nfileb/40,41/
      
      
      nfile=nfilea
      if(Beta)nfile=nfileb
      call nbinqr(nfile)
      if(nfile.GT.0)then
      IT=1
      l3=Ndim*Ndim
      call nbread(T,l3,nfile)
      else
      IT=0
      endif
      return
      end
C* :1 * 
      
