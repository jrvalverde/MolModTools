
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uu0401"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uu0401.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 16 "uu0401.web"
      blockdata uu0401
      implicit none
      double precision Core
      integer Iobas,Iocmat,Iocore,Iodmat,Iodtot,Iodum,Ioeig,Iogues,Iomin
     &c,Iomins,Iominv,Iopdmp,Ioproj,Iorthg,Ioscr1,Iosmat,Iosvec,Iosym,Io
     &teig,Iovmat
      common/memry/Core(50000)
      common/ops401/Iopdmp(19)
      common/rwf401/Iosmat,Iodmat,Iocmat,Iovmat,Iocore,Iobas,Iodum,Iomin
     &c,Iomins,Iominv,Iodtot,Ioeig,Iogues,Iosym,Ioproj,Iosvec,Ioscr1,Ior
     &thg,Ioteig
      data Core/50000*0./
      data Iopdmp/19*0/
      data Iosmat/514/,Iodmat/528/,Iocmat/524/,Iovmat/2551/,Iocore/515/,
     &Iobas/506/,Iodum/2574/,Iominc/2552/,Iomins/2553/,Iominv/2554/,Iodt
     &ot/532/,Ioeig/522/,Iogues/2555/,Iosym/523/,Ioproj/2556/,Iosvec/255
     &7/,Ioscr1/2558/
      data Iorthg/2559/,Ioteig/2560/
      
      end
C* :1 * 
      
