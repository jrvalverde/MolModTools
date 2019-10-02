
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gesrwf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gesrwf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "gesrwf.web"
      subroutine gesrwf(A,NBASIS)
      implicit none
      double precision A
      integer I56d,Ialt,Ibasis,Iblock,Icmp,Icmplt,Idgn,Idon1,Idon2,Idump
     &,Iguess,Imix,In,Iobas,Iocmat,Iocore,Iodmat,Iodtot,Iodum,Ioeig
      integer Iogues,Iominc,Iomins,Iominv,Ioproj,Iorthg,Ioscr1,Iosmat,Io
     &svec,Iosym,Ioteig,Iout,Iovmat,Ipolh,Iprint,Iproj,Ipunch,Iscale,Ism
     &ear,Itst
      integer Iuhf,NBASIS,nsq,ntt
      dimension A(*)
      common/ops401/Iguess,Iproj,Iuhf,Icmp,Ialt,Imix,Idgn,Iscale,Ismear,
     &Iblock,Icmplt,Itst,Ibasis,Ipolh,Idon1,Idon2,Iprint,Idump,I56d
      common/rwf401/Iosmat,Iodmat,Iocmat,Iovmat,Iocore,Iobas,Iodum,Iomin
     &c,Iomins,Iominv,Iodtot,Ioeig,Iogues,Iosym,Ioproj,Iosvec,Ioscr1,Ior
     &thg,Ioteig
      common/io/In,Iout,Ipunch
      
      
99001 format('  GESRWF')
      
      if(Idump.NE.0)write(Iout,99001)
      nsq=NBASIS*NBASIS
      ntt=NBASIS*(NBASIS+1)/2
      
      call twrite(Iocmat,A,nsq,1,nsq,1,0)
      call twrite(Iodmat,A,ntt,1,ntt,1,0)
      call twrite(Iodtot,A,ntt,1,ntt,1,0)
      
      if(Iuhf.EQ.0)then
      
      call twrite(Ioeig,A,NBASIS,1,NBASIS,1,0)
      call twrite(Iosym,A,NBASIS,1,NBASIS,1,0)
      if(Icmp.NE.1)goto 100
      else
      call twrite(Ioeig,A,NBASIS*2,1,NBASIS*2,1,0)
      call twrite(Iosym,A,2*NBASIS,1,2*NBASIS,1,0)
      call twrite(Iocmat+2,A,nsq,1,nsq,1,0)
      call twrite(Iodmat+2,A,ntt,1,ntt,1,0)
      
      if(Icmp.EQ.0)goto 100
      call twrite(Iocmat+3,A,nsq,1,nsq,1,0)
      call twrite(Iodmat+3,A,ntt,1,ntt,1,0)
      endif
      
      call twrite(Iocmat+1,A,nsq,1,nsq,1,0)
      call twrite(Iodmat+1,A,ntt,1,ntt,1,0)
      call twrite(Iodtot+1,A,ntt,1,ntt,1,0)
      
100   return
      
      end
C* :1 * 
      
