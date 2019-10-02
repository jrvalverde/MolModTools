
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ltoutd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ltoutd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "ltoutd.web"
      subroutine ltoutd(N,A,KEY)
      implicit none
      double precision A,gabs,s,thresh,zero
      integer icol,iend,ij,In,Iout,Ipunch,ir,irange,irow,istart,KEY,kl,m
     &,N,numcol
      dimension A(*),s(9)
      common/io/In,Iout,Ipunch
      data thresh/1.0D-6/,zero/0.0D0/
      data numcol/5/
      
      
      
      
      
      
      
99001 format(9(11x,i3))
99002 format(i4,9D14.6)
      
      istart=1
100   m=0
      iend=min0(istart+numcol-1,N)
      write(Iout,99001)(ir,ir=istart,iend)
      do 200 irow=istart,N
      m=m+1
      irange=min0(m,numcol)
      icol=istart
      kl=irow*(irow-1)/2
      do 150 ir=1,irange
      ij=kl+icol
      icol=icol+1
      s(ir)=A(ij)
      if(KEY.EQ.0.AND.gabs(s(ir)).LT.thresh)s(ir)=zero
150   continue
      write(Iout,99002)irow,(s(ir),ir=1,irange)
200   continue
      istart=istart+numcol
      if(istart.LE.N)goto 100
      return
      
      end
C* :1 * 
      
