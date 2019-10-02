
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fefao"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fefao.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "fefao.web"
      subroutine fefao(F,IWFOCK)
      implicit none
      double precision F
      integer Ispin,IWFOCK,l2,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,nf
     &ile,nfilea,nfileb
      dimension F(1)
      
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      
      data nfilea,nfileb/30,31/
      
      
      l2=Ndim*(Ndim+1)/2
      nfile=nfilea
      if(Beta)nfile=nfileb
      call nbinqr(nfile)
      if(nfile.GT.0)then
      call nbread(F,l2,nfile)
      call unpack(F,Ndim,Nbas,l2)
      else
      IWFOCK=0
      endif
      return
      end
C* :1 * 
      
