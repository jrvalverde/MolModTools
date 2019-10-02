
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 geninp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "geninp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "geninp.web"
      
      
      
      
      
      subroutine geninp(NEWDAF)
      implicit none
      integer Ichoos,Iprint,Ipseud,Ispin,Iw3c,Iwapol,Iwcubf,Iwdetl,Iwdm,
     &Iwfock,Iwhybs,Iwmulp,Iwpnao,Iwtnab,Iwtnao,Iwtnbo,Jcore,Jprint,kbod
     &m,kbohr
      integer kcubf,kend,kev,keywd,kgen,knatom,knbas,kopen,Kopt,kortho,k
     &reuse,kupper,len,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo
      integer Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lf
     &npnl,Lfnppa,Lfnpr,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas
     &,Ndim
      logical NEWDAF,end,error,equal
      
      dimension keywd(6),kgen(4),kend(4),kreuse(5),knbas(4),knatom(6),ku
     &pper(5),kopen(4),kortho(5),kbohr(4),kbodm(4),kev(2),kcubf(6)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbgen/Reuse,Upper,Bohr,Denop
      logical Reuse,Upper,Bohr,Denop
      
      data kgen/1H$,1HG,1HE,1HN/,kend/1H$,1HE,1HN,1HD/,kreuse/1HR,1HE,1H
     &U,1HS,1HE/,knbas/1HN,1HB,1HA,1HS/,knatom/1HN,1HA,1HT,1HO,1HM,1HS/,
     &kupper/1HU,1HP,1HP,1HE,1HR/,kopen/1HO,1HP,1HE,1HN/,kortho/1HO,1HR,
     &1HT,1HH,1HO/,kbohr/1HB,1HO,1HH,1HR/,kbodm/1HB,1HO,1HD,1HM/,kev/1HE
     &,1HV/kcubf/1HC,1HU,1HB,1HI,1HC,1HF/
      
      
      Nbas=0
      Natoms=0
      Munit=0
      Reuse=.FALSE.
      Upper=.FALSE.
      Bohr=.FALSE.
      Denop=.TRUE.
      
      
      rewind(Lfnin)
100   call strtin(Lfnin)
      len=6
      call hfld(keywd,len,end)
      if(len.EQ.0.AND.end)stop 'No $GEN keylist in the input file'
      if(.NOT.equal(keywd,kgen,4))goto 100
      
      
200   len=6
      call hfld(keywd,len,end)
      if(equal(keywd,kend,4))then
      
      
      if(Reuse)then
      NEWDAF=.FALSE.
      return
      else
      NEWDAF=.TRUE.
      endif
      
      Ndim=Nbas
      if(Nbas.LE.0)stop 'NBAS must be specified in $GEN keylist'
      if(Nbas.GT.MAXBAS)stop 'Increase parameter MAXBAS'
      if(Natoms.LE.0)stop 'NATOMS must be specified in $GEN keylist'
      if(Natoms.GT.MAXATM)stop 'Increase parameter MAXATM'
      return
      
99001 format(1x,'Unrecognized keyword >',6A1,'<')
      endif
      
      
      if(equal(keywd,kreuse,5))then
      Reuse=.TRUE.
      goto 200
      endif
      
      
      if(equal(keywd,knbas,4))then
      call ifld(Nbas,error)
      if(error)stop 'Error reading in number of basis functions NBAS'
      goto 200
      endif
      
      
      if(equal(keywd,knatom,4))then
      call ifld(Natoms,error)
      if(error)stop 'Error reading in number of atoms NATOMS'
      goto 200
      endif
      
      
      if(equal(keywd,kupper,5))then
      Upper=.TRUE.
      goto 200
      endif
      
      
      if(equal(keywd,kopen,4))then
      Open=.TRUE.
      goto 200
      endif
      
      
      if(equal(keywd,kortho,5))then
      Ortho=.TRUE.
      goto 200
      endif
      
      
      if(equal(keywd,kbohr,4))then
      Bohr=.TRUE.
      goto 200
      endif
      
      
      if(equal(keywd,kbodm,4))then
      Denop=.FALSE.
      goto 200
      endif
      
      
      if(equal(keywd,kev,2))then
      Munit=1
      goto 200
      endif
      
      
      if(equal(keywd,kcubf,6))then
      Iwcubf=1
      goto 200
      endif
      
      
      write(Lfnpr,99001)keywd
      stop
      end
C* :1 * 
      
