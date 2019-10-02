
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rtedef"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rtedef.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 52 "rtedef.web"
      
      subroutine rtedef(JTYPE)
      implicit none
      integer Cnoe,Coord,Direct,Dummy,Grdsym,Ialt,Ibas1,Ibas2,Ibas3,Ibas
     &4,Icmplx,Id2e,Ifau,ifpost,Ifrad,Iges,In,Intsym,Ioptyp,Iout
      integer Iprc1,Iprc2,Iprc3,Iprc4,Ipseud,Ipunch,iraf,Iraff,Iscfdm,It
     &est,Its,Itype,JTYPE,Ndchg,Noextr,Nondef,Nopop,Nostd,Nosym,Optcyc
      integer Prtges,Rpac,Savbas,Savfc,Savmo,Scfcyc,Stbint,Stbopt,Stbout
     &,Stbrxt,Stbsym,Stccxt,Stronl,Synch,Units,Vshift
      common/io/In,Iout,Ipunch
      common/ertgen/Itype,Ibas1,Ibas2,Ibas3,Ibas4,Iprc1,Iprc2,Iprc3,Iprc
     &4,Iges,Savfc,Savbas,Savmo,Ialt,Iscfdm,Its,Ipseud,Noextr,Iraff,Nopo
     &p,Itest,Synch,Nosym,Scfcyc,Nostd,Stbsym,Stbopt,Stbout,Stbint,Stbrx
     &t,Stccxt,Prtges,Icmplx,Ioptyp,Id2e,Intsym,Grdsym,Ifau,Ifrad,Units,
     &Optcyc,Coord,Stronl,Nondef,Ndchg,Cnoe,Rpac,Vshift,Direct,Dummy(51)
      if(Iprc1.EQ.0)Iprc1=1
      
      if(Its.NE.0.AND.Itype.EQ.0)Itype=2
      if(Its.NE.0.AND.Ioptyp.EQ.0)Ioptyp=1
      if(Its.NE.0.AND.Ioptyp.NE.1)then
      
      write(Iout,99002)
      else
      
      if(Itype.EQ.2.AND.Ioptyp.EQ.0.AND.Iprc1.LE.1)Ioptyp=1
      if(Itype.EQ.2.AND.Ioptyp.EQ.0.AND.Iprc1.GT.1)Ioptyp=2
      
      if(Ioptyp.EQ.1.AND.Iprc1.GT.1)then
      
      
      write(Iout,99001)
      elseif(Id2e.EQ.1)then
      write(Iout,99010)
      elseif(Iprc1.EQ.4)then
      
      write(Iout,99005)
      
      elseif(Coord.NE.0.AND.Ndchg.NE.0)then
      write(Iout,99014)
      else
      
      if(Ibas1.EQ.3)Ipseud=1
      
      if(Iprc1.EQ.4)then
      
      write(Iout,99003)
      else
      if(Iprc1.EQ.4.AND.Iprc3.EQ.0)Iprc3=4
      
      if(Iprc1.EQ.5.AND.Iprc3.EQ.0)Iprc3=2
      if(Iprc1.EQ.5.AND.Iprc3.NE.2)then
      write(Iout,99011)
      elseif(Iprc1.EQ.6.)then
      write(Iout,99005)
      else
      
      if(Iprc2.EQ.0)Iprc2=3
      
      if(Iprc4.EQ.1.AND.Itype.EQ.6)then
      write(Iout,99007)
      else
      if(Itype.EQ.7.AND.Iprc4.EQ.0)Iprc4=2
      if(Iprc4.EQ.0.AND.Id2e.EQ.1)Iprc4=2
      if(Iprc4.EQ.0)Iprc4=1
      if(Itype.EQ.6)Iprc4=2
      
      if(Ifau.EQ.0)Ifau=2
      if(Ifrad.EQ.0)Ifrad=2
      if(Ifau.EQ.2.AND.Ifrad.EQ.2)Units=0
      if(Ifau.EQ.2.AND.Ifrad.EQ.1)Units=2
      if(Ifau.EQ.1.AND.Ifrad.EQ.2)Units=1
      if(Ifau.EQ.1.AND.Ifrad.EQ.1)Units=3
      
      if(Itype.EQ.0)Itype=1
      
      if(Stbsym.EQ.0)Stbsym=2
      if(Stbopt.EQ.0)Stbopt=2
      if(Stbout.EQ.0)Stbout=4
      if(Itype.EQ.7.AND.Stbsym.EQ.1)Iscfdm=1
      
      ifpost=0
      if(Iprc1.GT.1.OR.Itype.EQ.6.OR.Itype.EQ.7.OR.Id2e.EQ.1)ifpost=1
      
      iraf=0
      if(Rpac.NE.1)then
      if(Iraff.NE.2)then
      if(Iraff.NE.1)then
      if(Icmplx.EQ.1)goto 2
      if(ifpost.EQ.1)goto 2
      elseif(Icmplx.EQ.1)then
      
      write(Iout,99006)
      goto 100
      elseif(ifpost.EQ.1)then
      
      write(Iout,99004)
      goto 100
      endif
      if(Iprc2.EQ.2.OR.Iprc2.EQ.4)iraf=2
      if(Iprc2.EQ.3)iraf=9
      if(Iprc2.EQ.1)iraf=1
      endif
      endif
2     Iraff=iraf
      
      if(Intsym.EQ.1.AND.(Icmplx.NE.0.OR.ifpost.EQ.1.OR.Rpac.EQ.1))then
      write(Iout,99008)
      else
      if(Intsym.EQ.0.AND.Icmplx.EQ.0.AND.ifpost.EQ.0)Intsym=1
      if(Intsym.EQ.0)Intsym=2
      if(Nosym.NE.0)Intsym=2
      if(Rpac.EQ.1)Intsym=2
      if(Rpac.EQ.1.AND.Coord.NE.0)Nosym=1
      
      if(Grdsym.EQ.0)Grdsym=1
      if(Nosym.NE.0)Grdsym=2
      
      
      if(Itype.EQ.1.AND.Iprc1.LE.1)JTYPE=1
      if(Itype.EQ.1.AND.Iprc1.GT.1)JTYPE=3
      if(Itype.EQ.2)then
      
      if(Ioptyp.EQ.1.AND.Iprc1.LE.1.AND.Id2e.NE.1)JTYPE=6
      if(Ioptyp.EQ.1.AND.Iprc1.LE.1.AND.Id2e.EQ.1)JTYPE=8
      if(Ioptyp.EQ.1.AND.Iprc1.GT.1.AND.Id2e.NE.1)JTYPE=10
      if(Ioptyp.EQ.1.AND.Iprc1.GT.1.AND.Id2e.EQ.1)JTYPE=12
      if(Ioptyp.EQ.4.AND.Iprc1.LE.1.AND.Id2e.NE.1)JTYPE=6
      if(Ioptyp.EQ.4.AND.Iprc1.LE.1.AND.Id2e.EQ.1)JTYPE=8
      if(Ioptyp.EQ.4.AND.Iprc1.GT.1.AND.Id2e.NE.1)JTYPE=10
      if(Ioptyp.EQ.4.AND.Iprc1.GT.1.AND.Id2e.EQ.1)JTYPE=12
      if(Ioptyp.EQ.2.AND.Iprc1.LE.1)JTYPE=13
      if(Ioptyp.EQ.2.AND.Iprc1.GT.1)JTYPE=14
      if(Ioptyp.EQ.3.AND.Iprc1.LE.1)JTYPE=16
      if(Ioptyp.EQ.3.AND.Iprc1.GT.1)JTYPE=17
      endif
      
      if(Itype.EQ.4)JTYPE=18
      if(Itype.EQ.4.AND.Iprc1.GT.1)JTYPE=19
      if(Itype.EQ.3)JTYPE=20
      if(Itype.EQ.5.AND.Iprc1.EQ.1)JTYPE=22
      if(Itype.EQ.5.AND.Iprc1.GT.1)JTYPE=23
      if(Itype.EQ.6)JTYPE=24
      if(Itype.EQ.7.AND.Stbopt.EQ.2)JTYPE=25
      if(Itype.EQ.7.AND.Stbopt.EQ.1)JTYPE=26
      if(JTYPE.EQ.24)then
      write(Iout,99009)
      elseif(Itype.EQ.7)then
      write(Iout,99012)
      elseif(Itype.EQ.23)then
      write(Iout,99013)
      else
      if(Rpac.EQ.0)return
      if(Icmplx.EQ.1)then
      write(Iout,99015)
      elseif(Iprc2.GT.1)then
      write(Iout,99016)
      else
      return
      endif
      endif
      endif
      endif
      endif
      endif
      endif
      endif
      
100   call lnk1e
      stop
      
      
99001 format('  NO POST-SCF GRAD OPT')
99002 format('  MUST DO GRADIENT OPTIMIZATION FOR TRANSITION STATE.')
99003 format('  NO MP4 YET.')
99004 format('  NO RAFFENETTI WITH POST-SCF YET.')
99005 format('  NO CC YET.')
99006 format('  NO RAFFENETTI WITH COMPLEX.')
99007 format('  NO FROZEN CORE WITH FREQUENCY CALCULATIONS.')
99008 format('  NO INTEGRAL SYMMETRY FOR POST-SCF, COMPLEX, OR SCFDM.')
99009 format('  NO FREQUENCY CALCULATIONS YET.')
99010 format('  NO ANALYTICAL SECOND DERIVATIVES YET.')
99011 format('  CAN ONLY DO CID.')
99012 format('  NO STABLITY YET.')
99013 format('  NO FORCES WITH POST-SCF YET.')
99014 format('  NO CHARGES READ IN WITH DIRECT COORDINATE INPUT')
99015 format('  NO RPAC WITH COMPLEX.')
99016 format('  NO RPAC WITH UHF OR OPEN-SHELL YET.')
      end
C* :1 * 
      
