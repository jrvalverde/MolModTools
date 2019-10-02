
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dsymm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dsymm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "dsymm.web"
      subroutine dsymm(A,NDIM)
      implicit none
      double precision A
      integer i,iad,ii,In,init,Iout,Ipunch,irc,irc1,j,ll,lu,NDIM
      dimension A(*)
      common/io/In,Iout,Ipunch
      
      
      
99001 format(6x,10(i9,3x))
99002 format(1x,i5,10F12.7)
      irc1=0
      irc=0
      ll=1
100   lu=min0(NDIM,ll+9)
      irc=irc+NDIM-ll+4
      if(irc.GT.52)then
      irc=0
      if(irc1.EQ.0)then
      endif
      endif
      write(Iout,99001)(i,i=ll,lu)
      init=1
      do 200 i=ll,NDIM
      iad=i*(i-1)/2
      ii=min0(i,lu)
      write(Iout,99002)i,(A(iad+j),j=ll,ii)
200   continue
      irc1=irc
      ll=lu+1
      if(ll.LE.NDIM)goto 100
      return
      
      end
C* :1 * 
      
