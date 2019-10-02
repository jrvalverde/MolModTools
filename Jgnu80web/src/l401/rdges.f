
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rdges"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rdges.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 54 "rdges.web"
      subroutine rdges(A,B,AA,BB,CC,MD,NB,MB,IFCP,JPRJ,NATOMS,IAN,C,NAE,
     &NBE)
      implicit none
      double precision A,AA,B,B1,B2,BB,C,CC
      integer i,I56d,Ialt,IAN,Ibasis,Iblock,Icmp,Icmplt,Idgn,Idon1,Idon2
     &,Idump,IFCP,Iguess,Imix,Iobas,Iocmat,Iocore,Iodmat,Iodtot
      integer Iodum,Ioeig,Iogues,Iominc,Iomins,Iominv,Ioproj,Iorthg,Iosc
     &r1,Iosmat,Iosvec,Iosym,Ioteig,Iovmat,Ipolh,Iprint,Iproj,Iscale,Ism
     &ear,ititle
      integer Itst,Iuhf,JPRJ,LENB,MAXSHL,MB,mbasb,MD,NAE,NATOMS,NB,NBE,n
     &wrdb
      dimension ititle(20)
      dimension A(*),B(*),AA(*),BB(*),CC(*),IAN(*),C(*)
      integer jbasis(4),ialpha(4),ibeta(4),iadens(4),ibdens(4)
      integer ialfai(4),ibetai(4),iadeni(4),ibdeni(4)
      parameter(MAXSHL=100,LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/B1(LENB)
      common/b2/B2(LENB)
      common/rwf401/Iosmat,Iodmat,Iocmat,Iovmat,Iocore,Iobas,Iodum,Iomin
     &c,Iomins,Iominv,Iodtot,Ioeig,Iogues,Iosym,Ioproj,Iosvec,Ioscr1,Ior
     &thg,Ioteig
      common/ops401/Iguess,Iproj,Iuhf,Icmp,Ialt,Imix,Idgn,Iscale,Ismear,
     &Iblock,Icmplt,Itst,Ibasis,Ipolh,Idon1,Idon2,Iprint,Idump,I56d
      
      data jbasis/'BASI','S   ','    ','    '/
      data ialpha/'ALPH','A MO',' COE','FS  '/
      data iadens/'ALPH','A DE','NSIT','Y   '/
      data iadeni/'ALPH','A DE','NSIT','Y,I '/
      data ibeta/'BETA',' MO ','COEF','S   '/
      data ibdens/'BETA',' DEN','SITY','    '/
      data ibdeni/'BETA',' DEN','SITY',',I  '/
      data ialfai/'ALPH','A MO',' COE','FS,I'/
      data ibetai/'BETA',' MO ','COEF','S,I '/
      if(JPRJ.NE.0)then
      if(JPRJ.EQ.2)then
      
      call binrd(B2,ititle,jbasis,nwrdb,mbasb)
      if(mbasb.NE.MB)call geserr(5)
      else
      
      call minbas(mbasb,NATOMS,IAN,C)
      do 20 i=1,LENB
      B2(i)=B1(i)
20    continue
      if(mbasb.NE.MB)call geserr(3)
      endif
      
      call tread(Iobas,B1,LENB,1,LENB,1,0)
      call frmprj(A,B,AA,MD,NB,MD,Iprint,IFCP)
      endif
      
      
      
      if(IFCP.EQ.1)call getges(ialpha,A,B,AA,BB,CC,1,IFCP,JPRJ,MD,NB,MB,
     &NAE,Iuhf)
      if(IFCP.EQ.2)call getges(iadens,A,B,AA,BB,CC,1,IFCP,JPRJ,MD,NB,MB,
     &NAE,Iuhf)
      
      if(IFCP.EQ.1.AND.Iuhf.EQ.1)call getges(ibeta,A,B,AA,BB,CC,3,IFCP,J
     &PRJ,MD,NB,MB,NBE,Iuhf)
      if(IFCP.EQ.2.AND.Iuhf.EQ.1)call getges(ibdens,A,B,AA,BB,CC,3,IFCP,
     &JPRJ,MD,NB,MB,NBE,Iuhf)
      
      if(IFCP.EQ.1.AND.Icmp.EQ.1)call getges(ialfai,A,B,AA,BB,CC,2,IFCP,
     &JPRJ,MD,NB,MB,NAE,Iuhf)
      if(IFCP.EQ.2.AND.Icmp.EQ.1)call getges(iadeni,A,B,AA,BB,CC,2,IFCP,
     &JPRJ,MD,NB,MB,NAE,Iuhf)
      
      if(IFCP.EQ.1.AND.Icmp.EQ.1.AND.Iuhf.EQ.1)call getges(ibetai,A,B,AA
     &,BB,CC,4,IFCP,JPRJ,MD,NB,MB,NBE,Iuhf)
      if(IFCP.EQ.2.AND.Icmp.EQ.1.AND.Iuhf.EQ.1)call getges(ibdeni,A,B,AA
     &,BB,CC,4,IFCP,JPRJ,MD,NB,MB,NBE,Iuhf)
      
      return
      
      end
C* :1 * 
      
