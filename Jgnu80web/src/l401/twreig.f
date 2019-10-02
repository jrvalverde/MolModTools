
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 twreig"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "twreig.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "twreig.web"
      subroutine twreig(AA,B,NB,INC,IOEIG)
      implicit none
      double precision AA,B
      integer i,INC,IOEIG,NB
      dimension B(NB),AA(NB)
      
      if(INC.NE.2)then
      call twrite(IOEIG,AA,NB,1,NB,1,0)
      return
      endif
      
      call tread(IOEIG,B,NB,1,NB,1,0)
      do 100 i=1,NB
      B(NB-1+i)=AA(i)
100   continue
      call twrite(IOEIG,B,NB,1,NB,1,0)
      return
      
      end
C* :1 * 
      
