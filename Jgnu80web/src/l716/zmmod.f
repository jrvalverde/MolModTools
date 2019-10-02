
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 zmmod"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "zmmod.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "zmmod.web"
      subroutine zmmod(BL,ALPHA,BETA,IPZ,JPZ,DEL)
      implicit none
      double precision ALPHA,BETA,BL,DEL
      integer IPZ,JPZ
      dimension BL(*),ALPHA(*),BETA(*)
      
      if(JPZ.EQ.2)then
      
      ALPHA(IPZ)=ALPHA(IPZ)+DEL
      elseif(JPZ.EQ.3)then
      
      BETA(IPZ)=BETA(IPZ)+DEL
      else
      
      BL(IPZ)=BL(IPZ)+DEL
      endif
      return
      
      end
C* :1 * 
      
