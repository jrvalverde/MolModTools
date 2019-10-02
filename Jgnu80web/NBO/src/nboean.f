
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nboean"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nboean.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "nboean.web"
      
      
      
      subroutine nboean(A,MEMORY,NBOOPT,IDONE)
      
      
      
      
      
      implicit none
      double precision A,aukcal,conv,echang,edel,ekcal,etot,evkcal,one,t
     &hrneg
      integer IDONE,Ispin,iswean,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,
     &Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnp
     &nl,Lfnppa
      integer Lfnpr,MEMORY,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,NBOOPT,Ndi
     &m
      logical error,new
      
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      dimension A(MEMORY),NBOOPT(10)
      
      data thrneg/-1.0D-3/
      data one,aukcal,evkcal/1.0D0,627.51,23.061/
      
      
      if(NBOOPT(10).GT.80)Lfndaf=48
      
      
      new=.FALSE.
      call nbopen(new,error)
      if(error)then
      IDONE=1
      return
      endif
      call feinfo(A,iswean)
      
      
      if(NBOOPT(1).EQ.3)then
      call fee0(edel,etot)
      echang=edel-etot
      if(Munit.EQ.0)then
      conv=aukcal
      elseif(Munit.EQ.1)then
      conv=evkcal
      else
      conv=one
      endif
      ekcal=echang*conv
      if(ekcal.LT.thrneg)write(Lfnpr,99004)
      if(Munit.EQ.0)then
      write(Lfnpr,99001)edel,etot,echang,ekcal
      elseif(Munit.EQ.1)then
      write(Lfnpr,99002)edel,etot,echang,ekcal
      else
      write(Lfnpr,99003)edel,etot,echang,ekcal
      endif
      IDONE=0
      call nbclos
      return
      endif
      
      
      
      if(iswean.EQ.1)then
      call delinp(NBOOPT,IDONE)
      if(IDONE.EQ.1)goto 100
      elseif(NBOOPT(10).GT.80)then
      call strtin(Lfnin)
      endif
      
      
      if(Rohf.OR.Mcscf.OR.Ci.OR.Auhf)then
      IDONE=1
      goto 100
      endif
      
      Ispin=0
      if(Uhf)Ispin=2
      Alpha=.FALSE.
      Beta=.FALSE.
      if(Uhf)Alpha=.TRUE.
      call nbodel(A,MEMORY,IDONE)
      if(IDONE.NE.1)then
      
      if(Uhf)then
      Ispin=-2
      Alpha=.FALSE.
      Beta=.TRUE.
      call nbodel(A,MEMORY,IDONE)
      endif
      
      write(Lfnpr,99005)
      call nbclos
      return
      endif
      
100   call nbclos
      return
      
99001 format(1x,78('-'),/,3x,'Energy of deletion : ',f20.9,/,3x,'  Total
     & SCF energy : ',f20.9,/,3x,'                       ---------------
     &----',/,3x,'     Energy change : ',f17.6,' a.u.,   ',f13.3,' kcal/
     &mol'/1x,78('-'))
99002 format(1x,78('-'),/,3x,'Energy of deletion : ',f20.9,/,3x,'  Total
     & SCF energy : ',f20.9,/,3x,'                       ---------------
     &----',/,3x,'     Energy change : ',f17.6,' e.V.,   ',f13.3,' kcal/
     &mol'/1x,78('-'))
99003 format(1x,78('-'),/,3x,'Energy of deletion : ',f13.3,/,3x,'  Total
     & SCF energy : ',f13.3,/,3x,'                       ---------------
     &----',/,3x,'     Energy change : ',f13.3,' kcal/mol,   ',f13.3,' k
     &cal/mol'/1x,78('-'))
99004 format(/,6x,'***** WARNING *****  The variational principle has be
     &en',/,5x,'  violated and the above deletion energy is invalid!',//
     &,5x,'Probable cause:  A deletion was attempted that did not ',/,5x
     &,'have as high symmetry as was employed in the integral',/,5x,'and
     & SCF computation.  REMEDY:  Redo computation without',/,5x,'symmet
     &ry if this non-symmetry-conserving deletion is still',/,5x,'desire
     &d.')
99005 format(/1x,'NEXT STEP:  Evaluate the energy of the new density mat
     &rix',/,1x,'            that has been constructed from the deleted 
     &NBO',/,1x,'            Fock matrix by doing one SCF cycle.'/)
      end
C* :1 * 
      
