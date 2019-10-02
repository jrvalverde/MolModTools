
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getges"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getges.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 32 "getges.web"
      subroutine getges(STR,A,B,AA,BB,CC,INC,IFCP,JPRJ,MD,NB,MB,NE,IUHF)
      implicit none
      double precision A,AA,B,BB,CC,one,two,x
      integer IFCP,In,INC,Iobas,Iocmat,Iocore,Iodmat,Iodtot,Iodum,Ioeig,
     &Iogues,Iominc,Iomins,Iominv,Ioproj,Iorthg,Ioscr1,Iosmat,Iosvec,Ios
     &ym
      integer Ioteig,Iout,Iovmat,Ipunch,ititle,IUHF,JPRJ,MB,mbas,MD,NB,N
     &E,nwrd
      dimension ititle(30)
      dimension A(*),B(*),AA(*),BB(*),CC(*)
      integer STR(*)
      common/io/In,Iout,Ipunch
      common/rwf401/Iosmat,Iodmat,Iocmat,Iovmat,Iocore,Iobas,Iodum,Iomin
     &c,Iomins,Iominv,Iodtot,Ioeig,Iogues,Iosym,Ioproj,Iosvec,Ioscr1,Ior
     &thg,Ioteig
      data one/1.0D0/,two/2.0D0/
99001 format('  DATA READ IN: ')
      
      call binrd(A,ititle,STR,nwrd,mbas)
      call strout(Iout,ititle,80,1)
      
      call unpcck(IFCP-1,IFCP-1,A,MD,MD,MB,MB)
      
      if(JPRJ.NE.0)call projec(A,B,AA,BB,CC,MD,NB,MB,IFCP)
      
      if(IFCP.NE.2)then
      call twrite(Iocmat-1+INC,A,MD,MD,NB,NB,0)
      
      x=two
      if(IUHF.EQ.1)x=one
      call dform(A,B,MD,NB,NE,x)
      endif
      
      call twrite(Iodmat-1+INC,B,MD,MD,NB,NB,1)
      return
      
      end
C* :1 * 
      
