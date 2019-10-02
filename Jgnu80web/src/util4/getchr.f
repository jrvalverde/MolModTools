
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getchr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getchr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "getchr.web"
      integer function getchr(ITRING,CURSOR)
      implicit none
      integer i,icur,imod,iord
      integer ITRING(20),jtring
      integer CURSOR
      character*1 string(4)
      equivalence(jtring,string(1))
      
      CURSOR=CURSOR+1
      i=CURSOR/4
      imod=mod(CURSOR,4)
      if(imod.GT.0)i=i+1
      icur=CURSOR-4*(i-1)
      jtring=ITRING(i)
      getchr=iord(string(icur))
      return
      
      end
C* :1 * 
      
