
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 csymm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "csymm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "csymm.web"
      subroutine csymm(A,MDIM,NDIM,CMP)
      implicit none
      double precision A,phi,r
      integer i,iad,ii,In,Iout,Ipunch,j,jlim,ll,lu,MDIM,mtt,NDIM,ntt
      logical CMP
      dimension A(*),r(5),phi(5)
      common/io/In,Iout,Ipunch
      
      
      
99001 format(6x,5(i16,9x))
99002 format(i4,2x,5(f14.7,f11.7))
      if(CMP)then
      
      mtt=MDIM*(MDIM+1)/2
      ntt=NDIM*(NDIM+1)/2
      ll=1
50    lu=min0(NDIM,ll+4)
      write(Iout,99001)(i,i=ll,lu)
      do 100 i=ll,NDIM
      iad=i*(i-1)/2
      ii=min0(i,lu)
      jlim=ii-ll+1
      do 60 j=1,jlim
      call carpol(A(iad+j),A(iad+j+mtt),r(j),phi(j))
60    continue
      write(Iout,99002)i,(r(j),phi(j),j=1,jlim)
100   continue
      ll=lu+1
      if(ll.LE.NDIM)goto 50
      return
      endif
      
      call dsymm(A,NDIM)
      return
      
      end
C* :1 * 
      
