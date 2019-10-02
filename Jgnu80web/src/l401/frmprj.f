
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 frmprj"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "frmprj.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "frmprj.web"
      subroutine frmprj(A,B,AA,MDIM,NBASIS,MBASIS,IPRINT,IFCP)
      implicit none
      double precision A,AA,B
      integer IFCP,Iobas,Iocmat,Iocore,Iodmat,Iodtot,Iodum,Ioeig,Iogues,
     &Iominc,Iomins,Iominv,Ioproj,Iorthg,Ioscr1,Iosmat,Iosvec,Iosym,Iote
     &ig,Iovmat
      integer IPRINT,MBASIS,MDIM,NBASIS
      dimension A(*),B(*),AA(*)
      common/rwf401/Iosmat,Iodmat,Iocmat,Iovmat,Iocore,Iobas,Iodum,Iomin
     &c,Iomins,Iominv,Iodtot,Ioeig,Iogues,Iosym,Ioproj,Iosvec,Ioscr1,Ior
     &thg,Ioteig
      
      call ovlp(B,NBASIS,MBASIS,MDIM,IPRINT)
      if(IPRINT.GT.1)call gesprt(14,B,0,MDIM,NBASIS,MBASIS)
      
      call tread(Iovmat,A,MDIM,MDIM,NBASIS,NBASIS,0)
      call matrec(A,B,AA,MDIM,NBASIS,NBASIS,MBASIS,1)
      call twrite(Ioproj,A,MDIM,MDIM,NBASIS,MBASIS,0)
      if(IPRINT.GE.2)call gesprt(4,A,0,MDIM,NBASIS,MBASIS)
      
      return
      
      end
C* :1 * 
      
