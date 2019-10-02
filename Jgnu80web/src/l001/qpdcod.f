
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpdcod"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpdcod.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 58 "qpdcod.web"
      integer function qpdcod(WHAT)
      implicit none
      integer i
      integer intern(21),extern(21),ncodes,WHAT,val
      data ncodes/21/
      data extern/'NUM','ALP','ALN','CHR','D10','D2','D8','D16','I10','I
     &2','I8','I16','FP','DP','NUL','EOL','WRD','STR','EOS','END','@'/
      
      data intern/4,5,6,7,3,17,18,19,8,20,21,22,9,10,11,12,13,14,15,16,2
     &3/
      
      val=0
      if(iabs(WHAT).GT.64)then
      
      do 50 i=1,ncodes
      if(WHAT.EQ.extern(i))then
      val=intern(i)
      goto 100
      endif
      
50    continue
      val=0
      else
      if(WHAT.LT.0)val=2
      if(WHAT.GT.0)val=1
      endif
100   qpdcod=val
      return
      
      end
C* :1 * 
      
