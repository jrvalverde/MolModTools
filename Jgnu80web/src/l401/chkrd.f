
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 chkrd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "chkrd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "chkrd.web"
      subroutine chkrd(A,NB,MB,IFCP,JPRJ)
      implicit none
      double precision A,B,B2
      integer i,I56d,Ialt,Ibasis,Iblock,Icmp,Icmplt,Idgn,Idon1,Idon2,Idu
     &mp,IFCP,Iguess,Imix,Iobas,Iocmat,Iocore,Iodmat,Iodtot,Iodum
      integer Ioeig,Iogues,Iominc,Iomins,Iominv,Ioproj,Iorthg,Ioscr1,Ios
     &mat,Iosvec,Iosym,Ioteig,Iovmat,Ipolh,Iprint,Iproj,Iscale,Ismear,it
     &itle,Itst
      integer Iuhf,JPRJ,LENB,MAXSHL,MB,mbasb,mbasc,mbasd,NB,nwrdb,nwrdc,
     &nwrdd
      integer jbasis(4),jalpha(4),jadens(4)
      dimension ititle(20)
      dimension A(*)
      parameter(MAXSHL=100,LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/B(LENB)
      common/b2/B2(LENB)
      common/ops401/Iguess,Iproj,Iuhf,Icmp,Ialt,Imix,Idgn,Iscale,Ismear,
     &Iblock,Icmplt,Itst,Ibasis,Ipolh,Idon1,Idon2,Iprint,Idump,I56d
      common/rwf401/Iosmat,Iodmat,Iocmat,Iovmat,Iocore,Iobas,Iodum,Iomin
     &c,Iomins,Iominv,Iodtot,Ioeig,Iogues,Iosym,Ioproj,Iosvec,Ioscr1,Ior
     &thg,Ioteig
      
      data jbasis/'BASI','S   ','    ','    '/
      data jalpha/'ALPH','A MO',' COE','FS  '/
      data jadens/'ALPH','A DE','NSIT','Y   '/
      JPRJ=0
      call binrd(B2,ititle,jbasis,nwrdb,mbasb)
      if(nwrdb.NE.0)then
      
      call tread(Iobas,B,LENB,1,LENB,1,0)
      do 50 i=1,LENB
      if(B(i).NE.B2(i))goto 100
50    continue
      
      JPRJ=0
      if(Iproj.EQ.2)JPRJ=2
      endif
      goto 200
      
100   JPRJ=2
      
200   call binrd(A,ititle,jalpha,nwrdc,mbasc)
      if(nwrdc.EQ.0)then
      
      call binrd(A,ititle,jadens,nwrdd,mbasd)
      
      if(nwrdd.EQ.0)call geserr(1)
      IFCP=2
      MB=mbasd
      else
      
      IFCP=1
      MB=mbasc
      endif
      
      if(MB.NE.NB.AND.JPRJ.EQ.0)JPRJ=1
      if(Iproj.EQ.3)JPRJ=0
      if(MB.NE.NB.AND.JPRJ.EQ.0)call geserr(2)
      if(JPRJ.EQ.2.AND.MB.NE.mbasb)call geserr(5)
      
      return
      
      end
C* :1 * 
      
