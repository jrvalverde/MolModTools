
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 iwprj"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "iwprj.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "iwprj.web"
      function iwprj(NCTR)
      implicit none
      integer iwprj,NCTR,nctr0
      data nctr0/0/
      
      
      iwprj=0
      if(NCTR.EQ.nctr0)return
      iwprj=1
      nctr0=NCTR
      return
      end
C* :1 * 
      
