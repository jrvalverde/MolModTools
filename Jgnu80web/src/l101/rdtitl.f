
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rdtitl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rdtitl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "rdtitl.web"
      subroutine rdtitl
      implicit none
      integer i,ic,In,iolbl,Iout,Ipunch,Irtcrd,Ititle,Label,len,lend,len
     &t,lold
      integer dash,blank
      dimension ic(20)
      common/label/Label(1000),Ititle(100),Irtcrd(100)
      common/io/In,Iout,Ipunch
      data iolbl/502/
      data dash/'-'/,blank/' '/
      
99001 format(20A4)
99002 format(2x,19A4)
99003 format(2x,128A1)
      
      len=0
      do 100 i=1,100
      Ititle(i)=blank
100   continue
      
200   read(In,99001)(ic(i),i=1,20)
      lold=len
      call pakstr(ic,80,Ititle,len)
      call puticr(blank,Ititle,len)
      if(lold+1.NE.len)goto 200
      
      call twrite(iolbl,Label,600,1,600,1,0)
      
      len=len-1
      lend=min0(len,76)
      write(Iout,99003)(dash,i=1,lend)
      lent=len/4+1
      write(Iout,99002)(Ititle(i),i=1,lent)
      write(Iout,99003)(dash,i=1,lend)
      return
      
      end
C* :1 * 
      
