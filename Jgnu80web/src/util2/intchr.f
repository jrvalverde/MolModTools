
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 intchr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "intchr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "intchr.web"
      integer function intchr(CHR,BASE)
      implicit none
      integer BASE,CHR,ilord,lord,t,val
      val=-1
      
      t=ilord(CHR)
      if(t.GT.lord('9'))then
      
      if(t.GE.lord('A'))then
      if(t.LE.lord('Z'))val=t-lord('A')+10
      endif
      elseif(t.GE.lord('0'))then
      val=t-lord('0')
      endif
      if(val.GE.BASE)val=-1
      intchr=val
      return
      
      end
C* :1 * 
      
