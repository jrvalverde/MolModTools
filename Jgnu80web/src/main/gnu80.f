
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gnu80"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gnu80.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "gnu80.web"
      program gnu80
      
      
      implicit none
      double precision Atmchg,C,Phycon
      integer Iadrs,Ian,Icharg,Idpost,Idrum,In,Info,Iop,Iout,Ipunch,iq,I
     &unit,Jop,Len18,Ll,Lnk,Multip,Nae,Natoms,Nbasis
      integer Nbe,Nchain,Ne,Nextwr,Nlink
      integer Pad
      real Tstart,Tstop,Elapsd,cputim
      common/phycon/Phycon(30)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/iop/Iop(50)
      common/munit/Iunit(20)
      common/info/Info(10)
      common/mdrum/Idrum,Idpost,Nextwr,Iadrs(75)
      common/tmprte/Nchain,Ll,Nlink,Pad,Lnk(200),Jop(50,50)
      common/io/In,Iout,Ipunch
      common/clcks/Tstart,Tstop,Elapsd
      common/len18/Len18
      
      
      Len18=10000
      call defunt
      call versn
      Tstart=cputim(iq)
      
      Nchain=1
      call chain(Nchain)
      stop
      
      end
C* :1 * 
      
