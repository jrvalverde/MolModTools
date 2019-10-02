
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cmat"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cmat.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "cmat.web"
      subroutine cmat(A,M,N,MD,ND,CMP)
      implicit none
      double precision A,phi,r
      integer i,il,In,Iout,Ipunch,iu,j,jlim,M,MD,N,ND
      logical CMP
      dimension A(M,N),r(5),phi(5)
      common/io/In,Iout,Ipunch
      
      
      
99001 format(6x,10(i9,3x))
99002 format(i4,2x,10F12.7)
99003 format(6x,5(i16,9x))
99004 format(i4,2x,5(f14.7,f11.7))
      if(.NOT.CMP)then
      
      il=1
50    iu=min0(ND,il+9)
      write(Iout,99001)(i,i=il,iu)
      do 100 i=1,MD
      write(Iout,99002)i,(A(i,j),j=il,iu)
100   continue
      il=iu+1
      if(il.LE.ND)goto 50
      else
      
      il=1
150   iu=min0(ND,il+4)
      write(Iout,99003)(i,i=il,iu)
      do 200 i=1,MD
      jlim=iu-il+1
      do 160 j=1,jlim
      call carpol(A(i,j),A(i,j+N),r(j),phi(j))
160   continue
      write(Iout,99004)i,(r(j),phi(j),j=1,jlim)
200   continue
      il=iu+1
      if(il.LE.ND)goto 150
      return
      endif
      return
      
      end
C* :1 * 
      
