
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fetnao"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fetnao.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "fetnao.web"
      subroutine fetnao(T)
      implicit none
      integer i,Ispin,j,l3,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,nfile
      double precision one,T,zero
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      dimension T(Ndim,Ndim)
      
      data zero,one/0.0D0,1.0D0/
      
      
      if(Ortho)then
      do 50 j=1,Ndim
      do 20 i=1,Ndim
      T(i,j)=zero
20    continue
      T(j,j)=one
50    continue
      else
      nfile=43
      l3=Ndim*Ndim
      call nbread(T,l3,nfile)
      endif
      return
      end
C* :1 * 
      
