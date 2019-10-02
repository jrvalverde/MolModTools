
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 scan"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "scan.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "scan.web"
      subroutine scan(A00,DAVAIL,SAVAIL,NBASIS)
      implicit none
      double precision A00,Thresh
      integer Iad1,Iad2,Iad3,Ias1,Ias2,Idmm1,Idmm2,Ieval,Iflag,In,Ioab,I
     &opcl,Iout,Ipunch,Ispect,Ispin,Loab,Lspect,Maxbuc,NBASIS
      integer Nd,Noa,Noa2,Noa3,Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobva,Nob
     &vb,Novaa,Novab,Novbb,Nrorb,Nva,Nva2,Nva3,Nvavb
      integer Nvb,Nvb2,Nvb3
      logical DAVAIL,SAVAIL
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/constr/Iopcl
      common/bucknr/Idmm1(21),Iad1,Iad2,Iad3,Ias1,Ias2,Idmm2(24)
      common/scana/Thresh,Nd,Ispin,Iflag
      common/io/In,Iout,Ipunch
      
      
      
      
      
      
      
      call track('  SCAN')
      
      
      Nd=NBASIS-Nrorb
      Iflag=0
      if(DAVAIL)then
      Ispin=1
      if(Novaa.GT.0)call scanaa(Iad1,Noa,Nva,A00)
      Ispin=2
      if(Novab.GT.0)call scanab(Iad2,Noa,Nob,Nva,Nvb,A00)
      Ispin=3
      if(Iopcl.GT.0.AND.Novbb.GT.0)call scanaa(Iad3,Nob,Nvb,A00)
      endif
      if(SAVAIL)then
      Ispin=1
      if(Noava.GT.0)call scanss(Ias1,Noa,Nva,A00)
      Ispin=2
      if(Iopcl.GT.0.AND.Nobvb.GT.0)call scanss(Ias2,Nob,Nvb,A00)
      endif
      
      return
      
      end
C* :1 * 
      
