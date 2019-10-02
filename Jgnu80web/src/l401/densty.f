
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 densty"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "densty.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "densty.web"
      subroutine densty(A,B,AA,BB,NB,NAE,NBE,MB)
      implicit none
      double precision A,AA,B,BB,one,two,x
      integer I56d,Ialt,Ibasis,Iblock,Icmp,Icmplt,Idgn,Idon1,Idon2,Idump
     &,Iguess,Imix,In,inc,Iobas,Iocmat,Iocore,Iodmat,Iodtot,Iodum
      integer Ioeig,Iogues,Iominc,Iomins,Iominv,Ioproj,Iorthg,Ioscr1,Ios
     &mat,Iosvec,Iosym,Ioteig,Iout,Iovmat,Ipolh,Iprint,Iproj,Ipunch,Isca
     &le,Ismear
      integer Itst,Iuhf,MB,NAE,NB,NBE,ne,nsq,ntt
      dimension A(NB,NB),B(NB,NB),AA(NB),BB(NB)
      common/rwf401/Iosmat,Iodmat,Iocmat,Iovmat,Iocore,Iobas,Iodum,Iomin
     &c,Iomins,Iominv,Iodtot,Ioeig,Iogues,Iosym,Ioproj,Iosvec,Ioscr1,Ior
     &thg,Ioteig
      common/ops401/Iguess,Iproj,Iuhf,Icmp,Ialt,Imix,Idgn,Iscale,Ismear,
     &Iblock,Icmplt,Itst,Ibasis,Ipolh,Idon1,Idon2,Iprint,Idump,I56d
      common/io/In,Iout,Ipunch
      data one/1.0D0/,two/2.0D0/
      
      
      
      
      
      
      inc=0
      ne=NAE
      ntt=NB*(NB+1)/2
      nsq=NB*NB
      x=one
      if(Iuhf.NE.1)x=two
      
      if(Iuhf.EQ.1)call twrite(Iogues,A,nsq,1,nsq,1,0)
      if(Iuhf.EQ.1)call twrite(Ioteig,AA,NB,1,NB,1,0)
      
100   if(Iprint.GE.2)call gesprt(9,A,4,NB,NB,MB)
      
      write(*,*)
      if(Ialt.EQ.2)call altges(A,AA,NB,inc)
      write(*,*)
      
      call twreig(AA,B,NB,inc,Ioeig)
      
      if(Icmp.EQ.0)then
      
      call twrite(Iocmat+inc,A,nsq,1,nsq,1,0)
      if(Iprint.NE.0)call gesprt(9,A,inc,NB,NB,MB)
      
      call dform(A,B,NB,NB,ne,x)
      call twrite(Iodmat+inc,B,NB,NB,NB,NB,1)
      if(Iprint.GE.2)call gesprt(8,B,inc,NB,NB,NB)
      
      if(inc.NE.0)return
      else
      call cmpden(A,B,AA,BB,NB,NB,x,inc,ne)
      if(inc.EQ.2)return
      endif
      
      if(Iuhf.EQ.0)return
      ne=NBE
      inc=2
      call tread(Iogues,A,nsq,1,nsq,1,0)
      call tread(Ioteig,AA,NB,1,NB,1,0)
      goto 100
      
      end
C* :1 * 
      
