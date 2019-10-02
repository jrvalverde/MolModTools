
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 herm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "herm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "herm.web"
      subroutine herm(NBASIS,A,KK)
      implicit none
      double precision A,a0
      integer i,i0,i1,i1i,i1r,i2,ii,j,KK,Ksm,Kspin,Ksw,Mdim,Mdsq,Mshifs,
     &Mtt,NBASIS,Nesk,Nest,Nest1
      integer Nse,Nsep,Ntt
      logical Cmp,Rhf
      dimension A(*)
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/maxdm/Mdim,Mtt,Ntt,Mdsq,Mshifs
      
      
      
      do 100 i=2,NBASIS
      i1=i-1
      i1r=i1*Mdim
      do 50 j=1,i1
      i0=(j-1)*Mdim+i
      i2=i1r+j
      a0=A(i0)
      A(i0)=A(i2)
      A(i2)=a0
50    continue
100   continue
      
      if(KK.GE.2.OR..NOT.Cmp)return
      
      do 200 i=2,NBASIS
      i1=i-1
      ii=i+Mdsq
      i1i=i1*Mdim+Mdsq
      do 150 j=1,i1
      i0=(j-1)*Mdim+ii
      i2=i1i+j
      a0=A(i0)
      A(i0)=-A(i2)
      A(i2)=-a0
150   continue
200   continue
      do 300 i=1,NBASIS
      i0=(i-1)*Mdim+i+Mdsq
      A(i0)=-A(i0)
300   continue
      return
      
      end
C* :1 * 
      
