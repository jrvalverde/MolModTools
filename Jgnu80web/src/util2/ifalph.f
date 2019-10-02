
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ifalph"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ifalph.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "ifalph.web"
      logical function ifalph(CHR)
      implicit none
      integer iord
      logical val
      integer t,CHR,crop
      t=crop(CHR)
      val=.FALSE.
      if(t.EQ.iord('$').OR.t.EQ.iord('?'))then
      
      val=.TRUE.
      elseif(t.GE.iord('A').AND.t.LE.iord('Z'))then
      val=.TRUE.
      endif
      ifalph=val
      return
      
      end
C* :1 * 
      
