
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rdcore"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rdcore.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "rdcore.web"
      subroutine rdcore(JCORE)
      implicit none
      integer i,Iatcr,Iatno,ii,Ino,Ispin,Iznuc,JCORE,jj,Lfnao,Lfnarc,Lfn
     &daf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho
      integer Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Ll,Lu,MAXA
     &TM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,Norbs
      logical error
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      
      do 100 i=1,Natoms
      Iatcr(i)=-1
100   continue
      
      
      if(JCORE.EQ.1)then
      write(Lfnpr,99001)
150   call ifld(ii,error)
      if(.NOT.(error))then
      if(ii.LT.1.OR.ii.GT.Natoms)then
      
      write(Lfnpr,99002)ii
      stop
      else
      call ifld(jj,error)
      if(error)then
      
      write(Lfnpr,99003)ii
      stop
      elseif(jj.LT.0)then
      
      write(Lfnpr,99004)jj,ii
      stop
      
99001 format(/1x,'Modified core list read from the $CORE keylist')
99002 format(/1x,'ATOM ',i4,' not found on this molecule')
99003 format(/1x,'No core orbitals selected for atom ',i4)
99004 format(/1x,i4,' core orbitals on atom ',i4,' does not make sense')
      else
      Iatcr(ii)=jj
      goto 150
      endif
      endif
      endif
      endif
      return
      end
C* :1 * 
      
