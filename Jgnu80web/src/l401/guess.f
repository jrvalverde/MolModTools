
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 guess"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "guess.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 184 "guess.web"
      subroutine guess(JUMP)
      implicit none
      double precision Atmchg,C,Core,Phycon
      integer i,I56d,ia,Ialt,Ian,Ibasis,Iblock,Icharg,Icmp,Icmplt,Idgn,I
     &don1,Idon2,Idump,ifcp,Iguess,Imix,In,indaa,indb
      integer indbb,indscr,indtab,Info,Iobas,Iocmat,Iocore,Iodmat,Iodtot
     &,Iodum,Ioeig,Iogues,Iominc,Iomins,Iominv,Iop,Ioproj,Iorthg,Ioscr1,
     &Iosmat
      integer Iosvec,Iosym,Ioteig,Iout,Iovmat,Ipolh,Iprint,Iproj,Ipunch,
     &Irtcrd,irwlbl,Iscale,Ismear,Ititle,Itst,Iuhf,Iunit,jprj,JUMP,Label
      integer lrwlbl,maxhan,mb,md,Multip,Nae,Natoms,nb,Nbasis,Nbe,Ne,nsq
     &,ntt
      integer stomin(54),totbas
      common/label/Label(1000),Ititle(100),Irtcrd(100)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/munit/Iunit(20)
      common/phycon/Phycon(30)
      common/info/Info(10)
      common/rwf401/Iosmat,Iodmat,Iocmat,Iovmat,Iocore,Iobas,Iodum,Iomin
     &c,Iomins,Iominv,Iodtot,Ioeig,Iogues,Iosym,Ioproj,Iosvec,Ioscr1,Ior
     &thg,Ioteig
      common/ops401/Iguess,Iproj,Iuhf,Icmp,Ialt,Imix,Idgn,Iscale,Ismear,
     &Iblock,Icmplt,Itst,Ibasis,Ipolh,Idon1,Idon2,Iprint,Idump,I56d
      common/io/In,Iout,Ipunch
      common/memry/Core(50000)
      data irwlbl/502/,lrwlbl/600/
      data maxhan/54/
      data stomin/2*2,8*5,8*9,2*13,16*18,2*22,16*27/
      
      
99001 format('   INITIAL GUESS WAVE FUNCTION.')
      
      call tread(irwlbl,Label,lrwlbl,1,lrwlbl,1,0)
      
      call gcfact
      
      call gesopt
      
      call gesrwf(Core(1),Nbasis)
      
      nb=Nbasis
      mb=nb
      nsq=nb*nb
      ntt=nb*(nb+1)/2
      md=Nbasis
      if(Iprint.NE.0)write(Iout,99001)
      
      call ind401(nb,indb,indaa,indbb,indscr,indtab)
      
      
      call tread(Iosmat,Core(1),nb,nb,nb,nb,1)
      call rootmt(Core(1),Core(indb),Core(indaa),Core(indbb),nb,nb,1)
      call twrite(Iovmat,Core(1),nsq,1,nsq,1,0)
      if(Idump.NE.0)call gesprt(1,Core(1),0,nb,nb,nb)
      
      if(Icmplt.NE.0)call twrite(Iosvec,Core(indb),nsq,1,nsq,1,0)
      
      
      
      if(Iguess.EQ.4)then
      
      
      call chkrd(Core(1),nb,mb,ifcp,jprj)
      if(mb.GT.nb)call ind401(mb,indb,indaa,indbb,indscr,indtab)
      md=max0(nb,mb)
      
      call rdges(Core(1),Core(indb),Core(indaa),Core(indbb),Core(indscr)
     &,md,nb,mb,ifcp,jprj,Natoms,Ian,C,Nae,Nbe)
      else
      
      totbas=0
      do 50 i=1,Natoms
      ia=Ian(i)
      totbas=totbas+stomin(ia)
      if(ia.GT.maxhan)then
      Iguess=3
      write(Iout,99002)ia
99002 format(' No Huckel Parameters for Atomic Number ',i4,/,' GUESS=COR
     &E imposed')
      endif
50    continue
      if(Nbasis.GT.totbas.AND.Iguess.EQ.2)then
      Iguess=1
      write(Iout,99003)
99003 format(' Basis is not Minimal, PROJECTED Huckel GUESS imposed')
      endif
      
      if(Iguess.EQ.1)call prjhuk(Core(1),Core(indb),Core(indaa),Core(ind
     &bb),Core(indscr),Core(indtab),nb,Ian,Natoms,C,Iprint,mb)
      
      if(Iguess.EQ.2)call hukges(Core(1),Core(indb),Core(indaa),Core(ind
     &bb),Core(indscr),md,nb,Iovmat,Iosmat,Ian,Natoms,Ibasis,Ipolh,Idon1
     &,Idon2,I56d)
      
      if(Iguess.EQ.3)call corges(Core(1),Core(indb),Core(indaa),Core(ind
     &bb),nb,Iocore,Iovmat)
      
      call densty(Core(1),Core(indb),Core(indaa),Core(indbb),md,Nae,Nbe,
     &mb)
      endif
      
      call symasg(Core(1),Core(indb),Core(indaa),Core(indbb),Core(indtab
     &),nb,mb,Nae,Nbe,Natoms,1)
      
      JUMP=0
      return
      
      end
C* :1 * 
      
