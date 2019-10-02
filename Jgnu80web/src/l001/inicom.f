
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 inicom"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "inicom.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 16 "inicom.web"
      subroutine inicom
      implicit none
      double precision Atmchg,C
      integer Ian,Icharg,Idummy,Idump,Info,Iop,Iunit,Multip,Nae,Natoms,N
     &basis,Nbe,Ne
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/munit/Iunit(20)
      common/info/Info(10)
      common/dump/Idump,Idummy
      
      
      Idump=0
      
      call iclear(50,Iop)
      
      
      Natoms=0
      Icharg=0
      Multip=0
      Nae=0
      Nbe=0
      Ne=0
      Nbasis=0
      call iclear(101,Ian)
      call aclear(100,Atmchg)
      call aclear(300,C)
      
      
      call iclear(10,Info)
      
      
      return
      
      end
C* :1 * 
      
