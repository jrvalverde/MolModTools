
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ioinqr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ioinqr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "ioinqr.web"
      function ioinqr(IFLG)
      implicit none
      integer IFLG,ioinqr,kblnk,kfull,klew,kprnt,kread,kval,kwrit
      
      data kfull,kval,klew/4HFULL,3HVAL,3HLEW/
      data kblnk,kprnt,kwrit,kread/4H    ,4HPRNT,4HWRIT,4HREAD/
      
      
      if(IFLG.EQ.kfull)then
      ioinqr=kprnt
      elseif(IFLG.EQ.kval)then
      ioinqr=kprnt
      elseif(IFLG.EQ.klew)then
      ioinqr=kprnt
      elseif(IFLG.GT.0)then
      ioinqr=kprnt
      elseif(IFLG.LT.0.AND.IFLG.GT.-1000)then
      ioinqr=kwrit
      elseif(IFLG.LT.0)then
      ioinqr=kread
      else
      ioinqr=kblnk
      endif
      return
      end
C* :1 * 
      
