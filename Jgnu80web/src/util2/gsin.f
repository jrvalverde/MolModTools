
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gsin"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gsin.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "gsin.web"
      double precision function gsin(ARG)
      implicit none
      double precision dsin,ARG
      
      
      
      
      gsin=dsin(ARG)
      return
      
      end
C* :1 * 
      
