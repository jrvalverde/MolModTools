
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rhfclo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rhfclo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 211 "rhfclo.web"
      subroutine rhfclo(JUMP)
      implicit none
      double precision Ab,acurcy,Atmchg,C,Dgen,e100,e100k,e101,e101k,e10
     &2,e102k,e103,e103k,e104,e104k,energy,enrk,eone,finac,hkcal
      integer i,Iab,Ian,Ibf,Icharg,idump,if503,Ilshft,In,inda,inda1,inda
     &2,indaa,indb,indb1,indb2,indbb,Inhibe,intcmo,Iop
      integer Ioscl,Iout,ipch,Ipunch,Irwc,Irwc1,Irwc2,Irwc3,Irweig,Irwf,
     &Irwgen,Irwh,Irwibf,Irwlc,Irwle,Irwlp,Irwp,Irwpt,Irws,Irwt
      integer Irwtm,Irww,iscf,Isym2e,Isymm,itemp,ititle,itqry,jcycle,JUM
     &P,Labdck,LENFIL,Length,Lind,lneq,Locrho,MAXBAS,Maxcyc,MAXSHL,MEMLE
     &N
      integer mmdim,mout,MOUTD,MPLUS1,Multip,Nae,Natoms,nb,Nbasis,nbasp1
     &,Nbe,Ncyset,Ne,Neq,neqbas,Nlshft,nlumo,Nsq,nsymop,Ntt
      integer NTTMAX,Nwiib,nwrd
      double precision t,tenrgk,tenrgy,Thresh,tk,vir,vshift,zero
      
      parameter(MAXBAS=150,MAXSHL=100,MEMLEN=50000,NTTMAX=((MAXBAS*(MAXB
     &AS+1))/2),LENFIL=(MEMLEN-4*NTTMAX),MOUTD=(8*MAXBAS+8*MAXSHL),MPLUS
     &1=(8*MAXSHL+1))
      
      integer Psave,Print,Punden,Engcon,entmod,cvgfl,dump
      integer cvgtyp
      logical ipr
      dimension ititle(20),cvgtyp(4,2)
      dimension Iab(1)
      dimension mout(MOUTD),neqbas(MAXBAS,8)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/ntt/Ntt,Length,Nsq
      common/irw501/Irwgen,Irws,Irwh,Irwt,Irweig,Irwc,Irwp,Irwpt,Irwf,Ir
     &wc1,Irwc2,Irwc3,Irwtm,Irwibf(2),Irwle,Irwlc,Irwlp,Irww
      common/memry/Ab(MEMLEN)
      common/ia/Lind(257)
      common/psave/Psave
      common/thresh/Thresh
      common/io/In,Iout,Ipunch
      common/scfops/Print,Maxcyc,Punden,Locrho,Engcon,Inhibe,Ncyset,Iosc
     &l,Ilshft,Nlshft
      common/gen/Dgen(47)
      common/ibf/Ibf(30)
      common/punlab/Labdck(4,3)
      common/isyrwf/Isymm,Neq
      equivalence(Nwiib,Ibf(29)),(Isym2e,Ibf(30))
      equivalence(mout(MPLUS1),neqbas(1,1))
      equivalence(Iab(1),Ab(1))
      data zero/0.0D0/,hkcal/627.52/
      data cvgtyp/'DENS','ITY ','MATR','IX= ','TOTA','L EN','ERGY','=   
     &'/
      lneq=MOUTD/2
      
99001 format(' RHF CLOSED SHELL SCF.'/' REQUESTED CONVERGENCE ON ',4A4,d
     &11.4,' WITHIN ',i3,' CYCLES.')
99002 format(1x,4HITER,2x,17HELECTRONIC-ENERGY,7x,11HCONVERGENCE,4x,13HE
     &XTRAPOLATION/1x,4(1H-),2x,17(1H-),7x,11(1H-),4x,13(1H-))
99003 format(12H CORE GUESS.)
99004 format(33H DENSITY MATRIX READ FROM CARDS: ,13A6)
99005 format(' ','SCF DONE:  E(RHF) = ',g19.12,' A.U. AFTER ',i4,' CYCLE
     &S'/1x,'           CONVG  = ',d13.4,12x,' -V/T = ',f10.6)
99006 format(1x,10(1H<),11HEIGENVALUES,10(1H>)/(i4,d14.7,i4,d14.7,i4,d14
     &.7,i4,d14.7,i4,d14.7))
99007 format(1x,10(1H<),31HMOLECULAR ORBITAL COEFFICIENTS.,10(1H>))
99008 format(1x,10(1H<),15HDENSITY MATRIX.,10(1H>))
99009 format('  DENSITY MATRIX READ IN HAS WRONG NBASIS.')
99010 format('  CONVERGENCE FAILURE ... ERROR TERMINATION.')
99011 format(' NBASIS=',i3,' CANNOT BE HANDLED.  LENGTH=',i9,' EXCEEDS A
     &VAILABLE MEMORY.')
99012 format(' "RHF" SPECIFIED, BUT MULTIPLICITY NOT EQUAL TO ONE.')
99013 format(1x)
      
      call drum
      call ilsw(2,1,iscf)
      if(iscf.GT.0)goto 500
      if(Multip.GT.1)write(Iout,99012)
      if(Multip.GT.1)call lnk1e
      
      cvgfl=iabs(Iop(13)-1)
      entmod=Iop(12)
      Punden=Iop(32)
      call ilsw(2,21,Psave)
      Ioscl=1
      Ilshft=Iop(9)
      Nlshft=Iop(10)
      vshift=float(Ilshft)/1000
      if(Ilshft.NE.0)write(Iout,99014)vshift
99014 format(/,'----------------------------------------------------',/,
     &' Level-Shifter of  ',f10.4,' used; Default  Cycles set to 50')
      Maxcyc=32
      if(Ilshft.NE.0)Maxcyc=50
      if(Iop(7).NE.0)Maxcyc=Iop(7)
      if(Iop(7).NE.0)write(Iout,99015)Maxcyc
99015 format(' Maximum SCF Cycles Reset to  ',i5)
      if(Nlshft.NE.0)write(Iout,99016)Nlshft
99016 format(' Level Shifters used for the FIRST  ',i4,'  CYCLES',/,' --
     &--------------------------------------------------')
      
      Locrho=Iop(5)
      Print=Iop(33)
      idump=0
      if(Iop(34).GT.2)then
      idump=1
      Print=4
      endif
      Inhibe=Iop(11)
      
      
      call incrd(Iop(6),acurcy,Iop(8),Engcon,Thresh)
      if(entmod.EQ.0)then
      if(Engcon.LE.0)then
      write(Iout,99001)(cvgtyp(i,1),i=1,4),acurcy,Maxcyc
      else
      
      write(Iout,99001)(cvgtyp(i,2),i=1,4),Thresh,Maxcyc
      endif
      if(Psave.EQ.0)write(Iout,99002)
      endif
      call tread(Irwibf(1),Ibf(1),Irwibf(2),1,Irwibf(2),1,0)
      
      if(Isym2e.EQ.1)then
      call tread(Isymm,Iab(1),1,1,1,1,0)
      nsymop=Iab(1)
      lneq=itqry(Neq)
      call tread(Neq,mout(1),lneq,1,lneq,1,0)
      endif
      
      
      Ntt=(Nbasis*(Nbasis+1))/2
      Nsq=Nbasis**2
      Length=max0(4*Ntt,2*Ntt+Nwiib)
      mmdim=MEMLEN
      if(Length.GT.mmdim)then
      write(Iout,99011)Nbasis,Length
      call lnk1e
      endif
      nbasp1=Nbasis+1
      do 100 i=1,nbasp1
      Lind(i)=(i*(i-1))/2
100   continue
      inda=1
      inda1=inda
      inda2=1+Ntt
      indaa=1+Nsq
      
      indb=1+2*Ntt
      indb1=indb
      indb2=indb+Ntt
      indbb=indb+Nsq
      
      do 200 i=1,Nsq
      Ab(i)=zero
200   continue
      
      
      if(entmod.EQ.0)then
      
      call twrite(Irwpt,Ab(1),Ntt,1,Ntt,1,0)
      call twrite(Irwf,Ab(1),Ntt,1,Ntt,1,0)
      
      if(Locrho.GT.0)then
      call binrd(Ab(1),ititle,Labdck(1,2),nwrd,nb)
      write(Iout,99004)(ititle(i),i=1,20)
      if(nb.NE.Nbasis)then
      write(Iout,99009)
      call lnk1e
      endif
      call twrite(Irwp,Ab(1),Ntt,1,Ntt,1,0)
      endif
      
      call twrite(Irwc1,Ab(1),Ntt,1,Ntt,1,0)
      call twrite(Irwc2,Ab(1),Ntt,1,Ntt,1,0)
      call twrite(Irwc3,Ab(1),Ntt,1,Ntt,1,0)
      call twrite(Irwle,Ab(1),Nbasis,1,Nbasis,1,0)
      call twrite(Irwlc,Ab(1),Nsq,1,Nsq,1,0)
      call tread(Irwp,Ab(1),Ntt,1,Ntt,1,0)
      call twrite(Irwlp,Ab(1),Ntt,1,Ntt,1,0)
      
      call frmv(Ab(inda),Ab(indb),Nbasis,Ab(indaa),Ab(indbb),Irwtm)
      
      
      call cycclo(Ab(1),energy,eone,finac,acurcy,Nbasis,Ne,jcycle,nsymop
     &,neqbas)
      
      call tread(Irwgen,Dgen,47,1,47,1,0)
      tenrgy=energy+Dgen(41)
      call virial(Nbasis,tenrgy,Ab(inda1),Ab(inda2),Irwlp,Irwt,vir,t)
      write(Iout,99013)
      write(Iout,99005)tenrgy,jcycle,acurcy,vir
99017 format(' COMPONENT                      A.U.',19x,'KCAL/MOL',/,' '
     &,63('-'))
99018 format(' TOTAL                  ',f20.12,5x,f15.3)
99019 format(' ELECTRONIC             ',f20.12,5x,f15.3)
99020 format(' NUCLEAR REPULSION      ',f20.12,5x,f15.3)
99021 format(' KINETIC                ',f20.12,5x,f15.3)
99022 format(' POTENTIAL              ',f20.12,5x,f15.3)
99023 format(' ELECTRONIC POTENTIAL   ',f20.12,5x,f15.3)
99024 format(' ONE-ELECTRON POTENTIAL ',f20.12,5x,f15.3)
99025 format(' TWO-ELECTRON POTENTIAL ',f20.12,5x,f15.3)
      
      e100=tenrgy-Dgen(41)
      e101=tenrgy-t
      e102=e101-Dgen(41)
      e103=eone-t
      e104=e102-e103
      tenrgk=hkcal*tenrgy
      enrk=hkcal*Dgen(41)
      tk=hkcal*t
      e100k=hkcal*e100
      e101k=hkcal*e101
      e102k=hkcal*e102
      e103k=hkcal*e103
      e104k=hkcal*e104
      
      if(Psave.EQ.0)then
      write(Iout,99017)
      write(Iout,99018)tenrgy,tenrgk
      write(Iout,99019)e100,e100k
      write(Iout,99020)Dgen(41),enrk
      write(Iout,99021)t,tk
      write(Iout,99022)e101,e101k
      write(Iout,99023)e102,e102k
      write(Iout,99024)e103,e103k
      write(Iout,99025)e104,e104k
      endif
      
      Dgen(23)=acurcy
      Dgen(32)=tenrgy
      Dgen(40)=vir
      Dgen(42)=t
      Dgen(43)=tenrgy
      Dgen(44)=zero
      call twrite(Irwgen,Dgen,47,1,47,1,0)
      else
      call twrite(Irwc1,Ab(1),Ntt,1,Ntt,1,0)
      call tread(Irweig,Ab(1),Nbasis,1,Nbasis,1,0)
      call twrite(Irwle,Ab(1),Nbasis,1,Nbasis,1,0)
      call tread(Irwc,Ab(1),Nsq,1,Nsq,1,0)
      call twrite(Irwlc,Ab(1),Nsq,1,Nsq,1,0)
      call tread(Irwp,Ab(1),Ntt,1,Ntt,1,0)
      call twrite(Irwlp,Ab(1),Ntt,1,Ntt,1,0)
      endif
      
      
      if(Print.GT.0)then
      call tread(Irwle,Ab(indaa),Nbasis,1,Nbasis,1,0)
      call tread(Irwlc,Ab(1),Nbasis,Nbasis,Nbasis,Nbasis,0)
      write(Iout,99006)(i,Ab(indaa-1+i),i=1,Nbasis)
      call linout(Ab(1),Nbasis,1)
      if(Print.GE.2)then
      call tread(Irwlp,Ab(1),Ntt,1,Ntt,1,0)
      write(Iout,99008)
      call ltoutd(Nbasis,Ab(1),1)
      endif
      endif
      
      
      if(Punden.LT.1)goto 300
      if(Punden.NE.1)then
      call tread(Irwlp,Ab(1),Ntt,1,Ntt,1,0)
      call binwt(Ab(1),2*Ntt,Labdck(1,2),Nbasis)
      if(ipch.EQ.2)goto 300
      endif
      call tread(Irwlc,Ab(1),Nsq,1,Nsq,1,0)
      call binwt(Ab(1),2*Nsq,Labdck(1,1),Nbasis)
      call tread(Irwle,Ab(1),Nbasis,1,Nbasis,1,0)
      call binwt(Ab(1),2*Nbasis,Labdck(1,3),Nbasis)
      
      
300   call ilsw(2,5,itemp)
      if(itemp*cvgfl.NE.0)then
      
      call tst503(if503)
      if(if503.GT.0)goto 400
      
      write(Iout,99010)
      call lnk1e
      endif
      
      if(entmod.EQ.0)then
      call tread(Irwle,Ab(1),Nbasis,1,Nbasis,1,0)
      
      if(Ilshft.NE.0)then
      nlumo=Nae+1
      do 320 i=nlumo,Nbasis
      Ab(i)=Ab(i)-vshift
320   continue
      endif
      
      call twrite(Irweig,Ab(1),Nbasis,1,Nbasis,1,0)
      call tread(Irwlc,Ab(1),Nsq,1,Nsq,1,0)
      call twrite(Irwc,Ab(1),Nsq,1,Nsq,1,0)
      endif
      call tread(Irwlp,Ab(1),Ntt,1,Ntt,1,0)
      call twrite(Irwp,Ab(1),Ntt,1,Ntt,1,0)
      call twrite(Irwpt,Ab(1),Ntt,1,Ntt,1,0)
      ipr=Print.GE.2
      call frmw(Nbasis,Nae,Nbe,Irwc,-1,-1,Irweig,Irww,Ab(indb1),Ab(indaa
     &),Ab(inda),ipr,Iout)
      
      if(Iop(14).EQ.1)call uhftst(Nbasis,Irweig,Irwc,Irwp,Ab(1))
      
400   if(intcmo.EQ.0)then
      endif
      
500   JUMP=0
      
      return
      
      end
C* :1 * 
      
