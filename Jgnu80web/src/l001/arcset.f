
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 arcset"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "arcset.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "arcset.web"
      subroutine arcset
      implicit none
      integer Cnoe,Coord,Direct,Dummy,Grdsym,Ialt,Ian,Ibas1,Ibas2,Ibas3,
     &Ibas4,Icharg,Icmplx,Id2e,Ifau,Ifrad,Iges,Info,Intsym,Iop
      integer Ioptyp,Iprc1,Iprc2,Iprc3,Iprc4,Ipseud,Iraff,Iscfdm,isum,It
     &est,itmp,Its,Itype,Iunit,Multip,Nae,Natoms,Nbasis,Nbe,Ndchg
      integer Ne,Noextr,Nondef,Nopop,Nostd,Nosym,npop,Optcyc,Prtges,Rpac
     &,Savbas,Savfc,Savmo,Scfcyc,Stbint,Stbopt,Stbout,Stbrxt,Stbsym,Stcc
     &xt
      integer Stronl,Synch,Units,Vshift
      double precision C,Atmchg,Phycon
      integer arctyp(7)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/munit/Iunit(20)
      common/phycon/Phycon(30)
      common/info/Info(10)
      common/ertgen/Itype,Ibas1,Ibas2,Ibas3,Ibas4,Iprc1,Iprc2,Iprc3,Iprc
     &4,Iges,Savfc,Savbas,Savmo,Ialt,Iscfdm,Its,Ipseud,Noextr,Iraff,Nopo
     &p,Itest,Synch,Nosym,Scfcyc,Nostd,Stbsym,Stbopt,Stbout,Stbint,Stbrx
     &t,Stccxt,Prtges,Icmplx,Ioptyp,Id2e,Intsym,Grdsym,Ifau,Ifrad,Units,
     &Optcyc,Coord,Stronl,Nondef,Ndchg,Cnoe,Rpac,Vshift,Direct,Dummy(51)
      data arctyp/1,2,0,0,7,8,0/
      
      call ilsw(1,25,0)
      if(Itype.EQ.3.OR.Itype.EQ.7.OR.Itype.EQ.4)return
      if(Coord.NE.0)return
      if(Ioptyp.EQ.3)return
      if(Ibas1.EQ.7)return
      call ilsw(1,25,1)
      npop=Nopop
      if(Nopop.EQ.2)npop=1
      
      itmp=arctyp(Itype)
      if(Its.NE.0)itmp=4
      isum=Ibas3+10*Ibas2+100*Ibas1+1000*Iprc1+10000*itmp+100000*npop+20
     &0000*Ialt+400000*Itest
      
      Info(4)=isum
      return
      
      end
C* :1 * 
      
