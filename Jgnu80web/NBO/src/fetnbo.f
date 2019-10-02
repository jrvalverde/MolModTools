
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fetnbo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fetnbo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "fetnbo.web"
      subroutine fetnbo(T)
      implicit none
      integer Ispin,l3,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,
     &Ndim,nfile
      double precision T
      dimension T(1)
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      
      
      l3=Ndim*Ndim
      nfile=44
      if(Beta)nfile=45
      call nbread(T,l3,nfile)
      return
      end
C* :1 * 
      
