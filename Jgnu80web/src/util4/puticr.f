
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 puticr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "puticr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "puticr.web"
      subroutine puticr(ICHR,ITRING,CURSOR)
      implicit none
      integer i,icur,imod,In,Iout,Ipunch
      integer ITRING(*),CURSOR
      integer ICHR,jchr
      integer jtring
      character*1 string(4),chr(4)
      common/io/In,Iout,Ipunch
      equivalence(jchr,chr(1))
      equivalence(jtring,string(1))
      
      jchr=ICHR
      CURSOR=CURSOR+1
      i=CURSOR/4
      imod=mod(CURSOR,4)
      if(imod.GT.0)i=i+1
      if(i.GT.4000)then
      write(Iout,*)' PUTICR: Off the end of /LABEL/'
      call lnk1e
      goto 99999
      endif
      icur=CURSOR-4*(i-1)
      jtring=ITRING(i)
      string(icur)=chr(1)
      ITRING(i)=jtring
      return
      
99999 end
      subroutine putchr(CHR,ITRING,CURSOR)
      implicit none
      integer i,icur,imod
      integer ITRING(20),CURSOR
      integer jtring
      character*1 CHR
      character*1 string(4)
      equivalence(string(1),jtring)
      
      CURSOR=CURSOR+1
      i=CURSOR/4
      imod=mod(CURSOR,4)
      if(imod.GT.0)i=i+1
      icur=CURSOR-4*(i-1)
      jtring=ITRING(i)
      string(icur)=CHR
      ITRING(i)=jtring
      return
      
      end
C* :1 * 
      
