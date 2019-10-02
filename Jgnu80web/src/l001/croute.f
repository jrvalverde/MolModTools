
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 croute"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "croute.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "croute.web"
      subroutine croute
      implicit none
      integer Dirtab,First,i,In,Iout,Ipunch,Irtcrd,Ititle,Jmptab,Labl,le
     &n,Optab,Ovtab,qparse,result,Segtab
      dimension result(3)
      common/label/Labl(1000),Ititle(100),Irtcrd(100)
      common/io/In,Iout,Ipunch
      common/crtab/First(32),Dirtab(28),Ovtab(13),Optab(35),Segtab(29),J
     &mptab(21)
      
      call fillrt(0,0,0)
      
      call qpinit(First,0,1,0)
      
      len=0
      call pakstr(Irtcrd,400,Irtcrd,len)
      
      result(1)=0
      result(2)=0
      result(3)=0
100   i=qparse(result,First,Irtcrd,len)
      
      if(i.NE.0)then
      if(i.NE.1)then
      
      write(Iout,99001)
      
99001 format('  ERROR PARSING NON-STANDARD ROUTE.')
      
      call qperro(Irtcrd,len,Iout)
      call lnk1e
      else
      call fillrt(result(1),result(2),result(3))
      result(1)=0
      result(2)=0
      result(3)=0
      goto 100
      endif
      endif
      
      call fillrt(6,0,0)
      return
      
      end
C* :1 * 
      
