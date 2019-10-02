
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 writed"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "writed.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "writed.web"
      subroutine writed(SF)
      implicit none
      double precision anorm,Atmchg,buf,C,SF
      integer i,i1,i2,ia,Ian,ib,Icharg,ii,ij,In,Iop,Iout,iprint,Ipunch,m
     &,Multip,Nae,naem,Natoms,Nbasis
      integer Nbe,Ne,noap,noapb,noapbm,nva,nvam,nvap,nvapb,nvapbm,nvb,nv
     &bm
      dimension SF(6,6,9,9)
      dimension buf(2025)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/io/In,Iout,Ipunch
      
99001 format(/' ALPHA-ALPHA:')
99002 format(10D13.7)
99003 format(/' ALPHA-BETA:')
99004 format(/' BETA-BETA:')
99005 format(/' LENGTH=',d20.8/)
      
      iprint=Iop(33)
      if(iprint.EQ.0)return
      anorm=0.D0
      
      naem=Nae-1
      nva=Nbasis-Nae
      nvam=nva-1
      nvb=Nbasis-Nbe
      nvbm=nvb-1
      if(Nae.GT.1.AND.nva.GT.1)then
      m=0
      do 50 ii=1,naem
      i1=ii+1
      do 20 ij=i1,Nae
      do 10 ia=1,nvam
      i2=ia+1
      do 5 ib=i2,nva
      m=m+1
      buf(m)=SF(ii,ij,ia,ib)
      anorm=anorm+buf(m)**2
5     continue
10    continue
20    continue
50    continue
      write(Iout,99001)
      write(Iout,99002)(buf(i),i=1,m)
      endif
      
      noap=Nae+1
      noapb=Nae+Nbe
      nvap=nva+1
      nvapb=nva+nvb
      m=0
      do 200 ii=1,Nae
      do 100 ij=noap,noapb
      do 80 ia=1,nva
      do 60 ib=nvap,nvapb
      m=m+1
      buf(m)=SF(ii,ij,ia,ib)
      anorm=anorm+buf(m)**2
60    continue
80    continue
100   continue
200   continue
      write(Iout,99003)
      write(Iout,99002)(buf(i),i=1,m)
      
      noapbm=noapb-1
      nvapbm=nvapb-1
      if(noapb.LE.noap.OR.nvapb.LE.nvap)return
      m=0
      do 300 ii=noap,noapbm
      i1=ii+1
      do 250 ij=i1,noapb
      do 220 ia=nvap,nvapbm
      i2=ia+1
      do 210 ib=i2,nvapb
      m=m+1
      buf(m)=SF(ii,ij,ia,ib)
      anorm=anorm+buf(m)**2
210   continue
220   continue
250   continue
300   continue
      write(Iout,99004)
      write(Iout,99002)(buf(i),i=1,m)
      write(Iout,99005)anorm
      return
      
      end
C* :1 * 
      
