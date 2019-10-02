
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gparse"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gparse.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "gparse.web"
      
      integer function gparse(LINE,LEN)
      implicit none
      integer i,In,Iout,Ipunch,LEN,LINE,qparse,Result,Table,xbas,xproc
      dimension LINE(*)
      common/ertgen/Result(100)
      common/io/In,Iout,Ipunch
      common/prstlb/Table(1329)
      call qpinit(Table,0,1,0)
      i=qparse(Result,Table,LINE,LEN)
      if(i.NE.0)call qperro(LINE,LEN,Iout)
      
      xbas=Result(2)
      Result(2)=xbas/1000000
      Result(3)=mod(xbas,1000000)/10000
      Result(4)=mod(xbas,10000)/100
      Result(5)=mod(xbas,100)
      
      xproc=Result(6)
      Result(6)=xproc/1000000
      Result(7)=mod(xproc,1000000)/10000
      Result(8)=mod(xproc,10000)/100
      Result(9)=mod(xproc,100)
      
      gparse=i
      return
      end
C* :1 * 
      
