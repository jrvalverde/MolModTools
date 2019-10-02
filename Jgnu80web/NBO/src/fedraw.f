
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fedraw"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fedraw.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "fedraw.web"
      subroutine fedraw(DM,SCR)
      implicit none
      double precision DM,SCR
      integer i,Ispin,l2,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,nfile,n
     &filea,nfileb
      dimension DM(1),SCR(1)
      
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      
      data nfilea,nfileb/20,21/
      
      
      l2=Ndim*(Ndim+1)/2
      nfile=nfilea
      if(Beta)nfile=nfileb
      call nbread(DM,l2,nfile)
      
      if(Open)then
      if(.NOT.(Alpha.OR.Beta))then
      call nbread(SCR,l2,nfileb)
      
      
      do 20 i=1,l2
      DM(i)=DM(i)+SCR(i)
20    continue
      endif
      endif
      
      call unpack(DM,Ndim,Nbas,l2)
      return
      end
C* :1 * 
      
