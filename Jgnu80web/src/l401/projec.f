
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 projec"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "projec.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "projec.web"
      subroutine projec(A,B,AA,BB,SCR,MDIM,NBASIS,MBASIS,IFCP)
      implicit none
      double precision A,AA,B,BB,SCR
      integer i,I56d,Ialt,Ibasis,Iblock,Icmp,Icmplt,Idgn,Idon1,Idon2,Idu
     &mp,IFCP,Iguess,Imix,Iobas,Iocmat,Iocore,Iodmat,Iodtot,Iodum
      integer Ioeig,Iogues,Iominc,Iomins,Iominv,Ioproj,Iorthg,Ioscr1,Ios
     &mat,Iosvec,Iosym,Ioteig,Iovmat,Ipolh,Iprint,Iproj,Iscale,Ismear,It
     &st,Iuhf
      integer j,MBASIS,MDIM,NBASIS
      dimension AA(*),BB(*),SCR(*)
      dimension A(MDIM,MDIM),B(MDIM,MDIM)
      common/ops401/Iguess,Iproj,Iuhf,Icmp,Ialt,Imix,Idgn,Iscale,Ismear,
     &Iblock,Icmplt,Itst,Ibasis,Ipolh,Idon1,Idon2,Iprint,Idump,I56d
      common/rwf401/Iosmat,Iodmat,Iocmat,Iovmat,Iocore,Iobas,Iodum,Iomin
     &c,Iomins,Iominv,Iodtot,Ioeig,Iogues,Iosym,Ioproj,Iosvec,Ioscr1,Ior
     &thg,Ioteig
      
      
      
      
      
      
      call tread(Ioproj,B,MDIM,MDIM,NBASIS,MBASIS,0)
      if(Idump.NE.0)call gesprt(7,A,0,MDIM,NBASIS,MBASIS)
      
      call matrec(B,A,AA,MDIM,NBASIS,MBASIS,MBASIS,4)
      if(IFCP.EQ.1)then
      
      if(Icmp.EQ.1)return
      
      
      call twrite(Iorthg,A,MDIM,MDIM,NBASIS,MBASIS,0)
      
      do 50 i=1,NBASIS
      do 20 j=1,MBASIS
      B(j,i)=A(i,j)
20    continue
50    continue
      call matrec(B,A,AA,MDIM,MBASIS,NBASIS,MBASIS,1)
      
      call rootmt(B,A,BB,AA,MDIM,MBASIS,1)
      
      call tread(Iorthg,A,MDIM,MDIM,NBASIS,MBASIS,0)
      call matrec(A,B,AA,MDIM,NBASIS,MBASIS,MBASIS,1)
      
      if(Icmplt.NE.0.AND.MBASIS.LT.NBASIS)then
      call tread(Iosvec,B,MDIM,MDIM,NBASIS,NBASIS,0)
      call complt(A,A,B,AA,BB,SCR,MDIM,NBASIS,MBASIS,MBASIS,Icmp,Icmplt,
     &Idump)
      endif
      else
      
      call matrec(A,B,AA,MDIM,NBASIS,MBASIS,NBASIS,3)
      return
      endif
      
      call tread(Iovmat,B,MDIM,MDIM,NBASIS,NBASIS,0)
      call matrec(B,A,AA,MDIM,NBASIS,NBASIS,NBASIS,4)
      return
      
      end
C* :1 * 
      
