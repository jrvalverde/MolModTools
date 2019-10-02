
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 link1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "link1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 197 "link1.web"
      subroutine link1
      implicit none
      real*8 Atmchg,C,gen,Phycon
      integer Ian,Icharg,Idump,In,Info,Iop,iord,Iout,ips,Ipunch,Irest,ir
     &wgen,irwinf,irwiop,irwmol,irwunt,Iunit,l,lrwgen,Multip
      integer Nae,Natoms,Nbasis,Nbe,Ne,nonstd,ntimes
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/munit/Iunit(20)
      common/phycon/Phycon(30)
      common/info/Info(10)
      common/fidump/Idump
      
      integer finit
      common/io/In,Iout,Ipunch
      common/restar/Irest
      
      dimension gen(47)
      data irwgen/501/,lrwgen/47/,finit/12/
      data irwiop/996/,irwmol/997/,irwunt/995/,irwinf/993/
      
      
      Irest=0
      call dollar(ntimes,ips)
      if(ntimes.NE.0)then
      if(Irest.NE.1)then
      if(ips.EQ.iord('F'))Idump=2
      call fileio(finit,l,l,l,l)
      
      if(ips.EQ.iord('P'))then
      call ilsw(1,20,0)
      call ilsw(1,21,0)
      elseif(ips.EQ.iord('F'))then
      call ilsw(1,20,0)
      call ilsw(1,21,0)
      else
      call ilsw(1,20,1)
      if(ips.NE.iord('N'))then
      call ilsw(1,21,0)
      else
      call ilsw(1,21,1)
      endif
      endif
      call inicom
      call phyfil(Phycon)
      call twrite(irwiop,Iop,25,1,25,1,0)
      call twrite(irwmol,Natoms,454,1,454,1,0)
      call twrite(irwunt,Iunit,10,1,10,1,0)
      call twrite(irwinf,Info,10,1,10,1,0)
      
      call aclear(lrwgen,gen)
      call twrite(irwgen,gen,lrwgen,1,lrwgen,1,0)
      endif
      
      call eroute(nonstd)
      if(nonstd.EQ.1)call croute
      call gamgen
      return
      endif
      call lnk1e
      stop
      end
C* :1 * 
      
