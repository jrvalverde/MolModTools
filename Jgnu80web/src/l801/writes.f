
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 writes"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "writes.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "writes.web"
      subroutine writes(SF)
      implicit none
      double precision anorm,Atmchg,buf,C,SF
      integer i,ia,Ian,Icharg,ii,In,Iop,Iout,iprint,Ipunch,m,Multip,Nae,
     &naebe,naep,Natoms,Nbasis,Nbe,Ne,nva
      integer nvap,nvapb,nvb
      dimension SF(6,9)
      dimension buf(45)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/io/In,Iout,Ipunch
      
99001 format(/' ALPHA:')
99002 format(10D13.7)
99003 format(/' BETA:')
99004 format(/' LENGTH=',d20.8/)
      
      iprint=Iop(33)
      if(iprint.EQ.0)return
      anorm=0.D0
      
      nva=Nbasis-Nae
      nvb=Nbasis-Nbe
      m=0
      do 100 ii=1,Nae
      do 50 ia=1,nva
      anorm=anorm+SF(ii,ia)**2
      m=m+1
      buf(m)=SF(ii,ia)
50    continue
100   continue
      write(Iout,99001)
      write(Iout,99002)(buf(i),i=1,m)
      
      naep=Nae+1
      naebe=Nae+Nbe
      nvap=nva+1
      nvapb=nva+nvb
      m=0
      do 200 ii=naep,naebe
      do 150 ia=nvap,nvapb
      anorm=anorm+SF(ii,ia)**2
      m=m+1
      buf(m)=SF(ii,ia)
150   continue
200   continue
      write(Iout,99003)
      write(Iout,99002)(buf(i),i=1,m)
      write(Iout,99004)anorm
      return
      
      end
C* :1 * 
      
