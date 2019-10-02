
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fefnbo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fefnbo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "fefnbo.web"
      subroutine fefnbo(F)
      implicit none
      double precision F
      integer Ispin,l2,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,nfile
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      dimension F(1)
      
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      
      
      nfile=34
      if(Beta)nfile=35
      l2=Ndim*(Ndim+1)/2
      call nbread(F,l2,nfile)
      return
      end
C* :1 * 
      
