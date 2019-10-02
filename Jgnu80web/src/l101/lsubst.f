
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lsubst"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lsubst.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 35 "lsubst.web"
      integer function lsubst(NAMCNT,N,STR,LEN)
      implicit none
      integer i,ians,In,Iout,Ipunch,LEN,lent,N,NAMCNT,ncur,STR,tstr
      logical streqc
      dimension tstr(5),STR(*),NAMCNT(*)
      common/io/In,Iout,Ipunch
      
      ncur=0
      do 100 i=1,N
      
      call getb(2,tstr,lent,NAMCNT,ncur)
      if(LEN.EQ.lent)then
      if(streqc(tstr,STR,LEN))then
      ians=i
      goto 200
      endif
      endif
      
100   continue
      
      call strout(Iout,15HUNKNOWN CENTER:,15,0)
      call strout(Iout,STR,LEN,1)
      call fferr(0,0)
      call lnk1e
200   lsubst=ians
      return
      
      end
C* :1 * 
      
