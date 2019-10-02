
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fenewd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fenewd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "fenewd.web"
      subroutine fenewd(DM)
      implicit none
      double precision DM
      integer Ispin,l2,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,nfile
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      dimension DM(1)
      
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      
      
      nfile=25
      if(Beta)nfile=26
      l2=Ndim*(Ndim+1)/2
      call nbread(DM,l2,nfile)
      return
      end
C* :1 * 
      
