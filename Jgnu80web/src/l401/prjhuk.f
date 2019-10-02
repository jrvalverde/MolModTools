
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 prjhuk"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "prjhuk.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 31 "prjhuk.web"
      subroutine prjhuk(A,B,AA,BB,ENEG,SCR,NB,IAN,NATOMS,C,IPRINT,MB)
      implicit none
      double precision A,AA,B,B1,B2,BB,C,ENEG,SCR
      integer i,IAN,In,Iobas,Iocmat,Iocore,Iodmat,Iodtot,Iodum,Ioeig,Iog
     &ues,Iominc,Iomins,Iominv,Ioproj,Iorthg,Ioscr1,Iosmat,Iosvec,Iosym
      integer Ioteig,Iout,Iovmat,IPRINT,Ipunch,LENB,MAXSHL,MB,NATOMS,NB
      dimension A(*),B(*),AA(*),BB(*),SCR(*),IAN(*),C(*),ENEG(*)
      parameter(MAXSHL=100,LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/B1(LENB)
      common/io/In,Iout,Ipunch
      common/b2/B2(LENB)
      common/rwf401/Iosmat,Iodmat,Iocmat,Iovmat,Iocore,Iobas,Iodum,Iomin
     &c,Iomins,Iominv,Iodtot,Ioeig,Iogues,Iosym,Ioproj,Iosvec,Ioscr1,Ior
     &thg,Ioteig
      
      
99001 format(' PROJECTED HUCKEL GUESS.')
      write(Iout,99001)
      call minbas(MB,NATOMS,IAN,C)
      do 100 i=1,LENB
      B2(i)=B1(i)
100   continue
      call ovlp(B,MB,MB,NB,IPRINT)
      call twrite(Iomins,B,NB,NB,MB,MB,1)
      if(IPRINT.GT.1)call gesprt(2,B,0,NB,NB,MB)
      
      call rootmt(B,A,BB,AA,NB,MB,1)
      call twrite(Iominv,B,NB,NB,MB,MB,0)
      if(IPRINT.GT.1)call gesprt(3,B,0,NB,NB,MB)
      
      call tread(Iobas,B1,LENB,1,LENB,1,0)
      call frmprj(A,B,AA,NB,NB,MB,IPRINT,1)
      
      call hukges(A,B,AA,BB,ENEG,NB,MB,Iominv,Iomins,IAN,NATOMS,0,0,0,0,
     &0)
      
      call projec(A,B,AA,BB,SCR,NB,NB,MB,1)
      return
      
      end
C* :1 * 
      
