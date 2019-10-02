
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 svfnbo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "svfnbo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "svfnbo.web"
      subroutine svfnbo(F)
      implicit none
      double precision F
      integer Ispin,l2,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,nfile
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      dimension F(Ndim,Ndim)
      
      
      nfile=34
      if(Beta)nfile=35
      l2=Ndim*(Ndim+1)/2
      call pack(F,Ndim,Nbas,l2)
      call nbwrit(F,l2,nfile)
      call unpack(F,Ndim,Nbas,l2)
      return
      end
C* :1 * 
      
